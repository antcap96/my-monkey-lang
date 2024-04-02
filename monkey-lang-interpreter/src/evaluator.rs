use std::collections::HashMap;
use std::rc::Rc;

use monkey_lang_core::ast;
use monkey_lang_core::ast::{Expression, Pattern};
use crate::environment::Environment;
use crate::object::{object_to_key, EvaluationError, Object, QuickReturn};

pub fn eval_program(
    program: &ast::Program,
    environment: &mut Environment,
) -> Result<Rc<Object>, EvaluationError> {
    let mut output = Object::null();
    for statement in &program.statements {
        let result = eval_statement(statement, environment);

        match result {
            Err(QuickReturn::Return(value)) => return Ok(value),
            Err(QuickReturn::Error(error)) => return Err(error),
            Ok(object) => output = object,
        };
    }
    Ok(output)
}

fn eval_statement(
    statement: &ast::Statement,
    environment: &mut Environment,
) -> Result<Rc<Object>, QuickReturn> {
    match statement {
        ast::Statement::Expression(expression) => eval_expression(expression, environment),
        ast::Statement::Return(statement) => eval_return_statement(statement, environment),
        ast::Statement::Let(statement) => eval_let_statement(statement, environment),
    }
}

fn eval_let_statement(
    statement: &ast::LetStatement,
    environment: &mut Environment,
) -> Result<Rc<Object>, QuickReturn> {
    let value = eval_expression(&statement.value, environment)?;
    environment.set(statement.identifier.name.clone(), value.clone());
    Ok(value)
}

fn eval_return_statement(
    statement: &ast::ReturnStatement,
    environment: &mut Environment,
) -> Result<Rc<Object>, QuickReturn> {
    let value = eval_expression(&statement.value, environment)?;
    Err(QuickReturn::Return(value))
}

fn eval_expression(
    expression: &Expression,
    environment: &mut Environment,
) -> Result<Rc<Object>, QuickReturn> {
    match expression {
        Expression::IntegerLiteral(value) => Ok(Object::integer(*value)),
        Expression::BooleanLiteral(value) => Ok(Object::boolean(*value)),
        Expression::StringLiteral(value) => Ok(Object::string(value.clone())),
        Expression::NullLiteral => Ok(Object::null()),
        Expression::ArrayLiteral(array) => Ok(Object::array(
            array
                .iter()
                .map(|expression| eval_expression(expression, environment))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        Expression::HashLiteral(literal) => {
            let evaluated_literal = literal
                .iter()
                .map(|(key, value)| {
                    Ok((
                        eval_expression(key, environment)?,
                        eval_expression(value, environment)?,
                    ))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let mut hashmap = HashMap::new();
            for (key, value) in evaluated_literal {
                let hashed_key = object_to_key(&key).map_err(QuickReturn::Error)?;
                hashmap.insert(hashed_key, (key, value));
            }
            Ok(Object::hash(hashmap))
        }
        Expression::Identifier(identifier) => environment.get(&identifier.name).ok_or(
            QuickReturn::Error(EvaluationError::UnknownIdentifier(identifier.name.clone())),
        ),
        Expression::PrefixOperation(kind, expression) => {
            let right = eval_expression(expression, environment);
            eval_prefix_operation(kind, right)
        }
        Expression::InfixOperation(kind, left, right) => {
            let left = eval_expression(left, environment);
            let right = eval_expression(right, environment);
            eval_infix_operation(kind, left, right)
        }
        Expression::IfExpression {
            condition,
            consequence,
            alternative,
        } => {
            let condition = eval_expression(condition, environment)?;
            match condition.as_ref() {
                Object::Boolean(value) => {
                    if *value {
                        eval_block_statement(consequence, environment)
                    } else if let Some(alternative) = alternative {
                        eval_block_statement(alternative, environment)
                    } else {
                        Ok(Object::null())
                    }
                }
                _ => Err(QuickReturn::Error(EvaluationError::NonBooleanCondition(
                    condition,
                ))),
            }
        }
        Expression::FunctionLiteral { parameters, body } => Ok(Object::function(
            parameters.clone(),
            body.clone(),
            environment.clone(),
        )),
        Expression::CallExpression {
            function,
            arguments,
        } => {
            let function = eval_expression(function, environment)?;
            match function.as_ref() {
                Object::Function(function) => eval_call_function(function, arguments, environment),
                Object::BuiltinFunction(function) => {
                    eval_call_builtin_function(function, arguments, environment)
                }
                _ => Err(QuickReturn::Error(EvaluationError::CallNonFunction(
                    function,
                ))),
            }
        }
        Expression::IndexExpression { left, index } => {
            let left = eval_expression(left, environment)?;
            let index = eval_expression(index, environment)?;
            match (left.as_ref(), index.as_ref()) {
                (Object::Array(array), Object::Integer(index)) => Ok(array
                    .get(*index as usize)
                    .cloned()
                    .unwrap_or(Object::null())),
                (Object::Array(_), _) => Err(QuickReturn::Error(
                    EvaluationError::IndexingWithNonInteger(index),
                )),
                (Object::Hash(hash), _) => {
                    let hashed_index = object_to_key(&index).map_err(QuickReturn::Error)?;
                    Ok(hash
                        .get(&hashed_index)
                        .map(|(_, value)| value.clone())
                        .unwrap_or(Object::null()))
                }
                _ => Err(QuickReturn::Error(EvaluationError::IndexNotSupported(left))),
            }
        }
        Expression::MatchExpression { expression, cases } => {
            let object = eval_expression(expression, environment)?;
            for case in cases {
                if let MatchResult::Match(identifiers) = case.pattern.matches(&object) {
                    for (identifier, value) in identifiers {
                        environment.set(identifier.name, value);
                    }
                    return eval_block_statement(&case.body, environment);
                }
            }
            Err(QuickReturn::Error(EvaluationError::NoMatchingCase(object)))
        }
    }
}

fn eval_call_function(
    function: &crate::object::Function,
    arguments: &Vec<Expression>,
    environment: &mut Environment,
) -> Result<Rc<Object>, QuickReturn> {
    if function.parameters.len() != arguments.len() {
        let expected = function.parameters.len();
        return Err(QuickReturn::Error(EvaluationError::WrongArgumentCount {
            function: function.clone(),
            expected,
            actual: arguments.len(),
        }));
    }
    let arguments = eval_expressions(arguments, environment)?;
    let (return_value, new_environment) =
        apply_function(function, arguments).map_err(QuickReturn::Error)?;
    match return_value.as_ref() {
        Object::Function(f) => {
            // Ensure returned functions keep a strong reference to the
            // environment used to evaluate them, so that captured variables are
            // still available. Because environments keep a strong reference to
            // its parent, this keeps all possible captured variables alive.
            //
            // When the functions goes out of scope, the environments can be
            // removed
            return Ok(Rc::new(Object::Function(
                f.clone_with_captured_environment(new_environment),
            )));
        }
        _ => return Ok(return_value),
    }
}

fn eval_call_builtin_function(
    function: &crate::object::BuiltinFunction,
    arguments: &Vec<Expression>,
    environment: &mut Environment,
) -> Result<Rc<Object>, QuickReturn> {
    let arguments = eval_expressions(arguments, environment)?;
    (function.func)(arguments)
}

fn eval_expressions(
    arguments: &Vec<Expression>,
    environment: &mut Environment,
) -> Result<Vec<Rc<Object>>, QuickReturn> {
    let mut result = Vec::new();
    for argument in arguments {
        result.push(eval_expression(argument, environment)?);
    }
    Ok(result)
}

fn apply_function(
    function: &crate::object::Function,
    arguments: Vec<Rc<Object>>,
) -> Result<(Rc<Object>, Environment), EvaluationError> {
    let mut new_environment = Environment::new_enclosed(Environment {
        environment: function
            .parent_env
            .upgrade()
            .expect("The function environment should always be kept alive"),
    });
    for (parameter, argument) in function.parameters.iter().zip(arguments.iter()) {
        new_environment.set(parameter.name.clone(), argument.clone());
    }
    let result = eval_block_statement(&function.body, &mut new_environment);
    match result {
        Ok(object) => Ok((object, new_environment)),
        Err(QuickReturn::Return(value)) => Ok((value, new_environment)),
        Err(QuickReturn::Error(err)) => Err(err),
    }
}

fn eval_block_statement(
    block: &ast::BlockStatement,
    environment: &mut Environment,
) -> Result<Rc<Object>, QuickReturn> {
    let mut result = Object::null();
    for statement in &block.statements {
        result = eval_statement(statement, environment)?;
    }
    Ok(result)
}

fn eval_prefix_operation(
    kind: &ast::PrefixOperationKind,
    right: Result<Rc<Object>, QuickReturn>,
) -> Result<Rc<Object>, QuickReturn> {
    let right = right?;
    match (&kind, right.as_ref()) {
        (ast::PrefixOperationKind::Bang, Object::Boolean(value)) => Ok(Object::boolean(!value)),
        (ast::PrefixOperationKind::Minus, Object::Integer(value)) => Ok(Object::integer(-value)),
        _ => Err(QuickReturn::Error(EvaluationError::UnknownPrefixOperator {
            right: Box::new(right),
            operation: kind.clone(),
        })),
    }
}

fn eval_infix_operation(
    kind: &ast::InfixOperationKind,
    left: Result<Rc<Object>, QuickReturn>,
    right: Result<Rc<Object>, QuickReturn>,
) -> Result<Rc<Object>, QuickReturn> {
    use ast::InfixOperationKind;
    let left = left?;
    let right = right?;
    match (kind, left.as_ref(), right.as_ref()) {
        (InfixOperationKind::Plus, Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::integer(left + right))
        }
        (InfixOperationKind::Plus, Object::String(left), Object::String(right)) => {
            Ok(Object::string(format!("{}{}", left, right)))
        }
        (InfixOperationKind::Minus, Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::integer(left - right))
        }
        (InfixOperationKind::Multiply, Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::integer(left * right))
        }
        (InfixOperationKind::Divide, Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::integer(left / right))
        }
        (InfixOperationKind::LessThan, Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::boolean(left < right))
        }
        (InfixOperationKind::GreaterThan, Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::boolean(left > right))
        }
        (InfixOperationKind::Equal, Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::boolean(left == right))
        }
        (InfixOperationKind::Equal, Object::String(left), Object::String(right)) => {
            Ok(Object::boolean(left == right))
        }
        (InfixOperationKind::NotEqual, Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::boolean(left != right))
        }
        (InfixOperationKind::NotEqual, Object::String(left), Object::String(right)) => {
            Ok(Object::boolean(left != right))
        }
        (InfixOperationKind::Equal, Object::Boolean(left), Object::Boolean(right)) => {
            Ok(Object::boolean(left == right))
        }
        (InfixOperationKind::NotEqual, Object::Boolean(left), Object::Boolean(right)) => {
            Ok(Object::boolean(left != right))
        }
        _ => Err(QuickReturn::Error(EvaluationError::UnknownInfixOperator {
            left: Box::new(left),
            right: Box::new(right),
            operation: kind.clone(),
        })),
    }
}

enum MatchResult {
    Match(Vec<(ast::Identifier, Rc<Object>)>),
    NoMatch,
}

trait PatternMatches {
    fn matches(&self, object: &Rc<Object>) -> MatchResult;
}

impl PatternMatches for Pattern {
    fn matches(&self, object: &Rc<Object>) -> MatchResult {
        match (self, object.as_ref()) {
            (Pattern::Identifier(ident), _) => {
                MatchResult::Match(vec![(ident.clone(), object.clone())])
            }
            (Pattern::IntegerLiteral(pattern), Object::Integer(object)) => {
                if pattern == object {
                    MatchResult::Match(vec![])
                } else {
                    MatchResult::NoMatch
                }
            }
            (Pattern::BooleanLiteral(pattern), Object::Boolean(object)) => {
                if pattern == object {
                    MatchResult::Match(vec![])
                } else {
                    MatchResult::NoMatch
                }
            }
            (Pattern::StringLiteral(pattern), Object::String(object)) => {
                if pattern == object {
                    MatchResult::Match(vec![])
                } else {
                    MatchResult::NoMatch
                }
            }
            (Pattern::NullLiteral, Object::Null) => MatchResult::Match(vec![]),
            (Pattern::ArrayPattern(pattern), Object::Array(arr)) => {
                let mut identifiers = Vec::new();
                if (pattern.contents.len() > arr.len())
                    || (pattern.remainder.is_none() && (pattern.contents.len() != arr.len()))
                {
                    return MatchResult::NoMatch;
                }
                for (left, right) in pattern.contents.iter().zip(arr.iter()) {
                    if let MatchResult::Match(vec) = left.matches(right) {
                        identifiers.extend(vec);
                    } else {
                        return MatchResult::NoMatch;
                    }
                }
                if let Some(ref remainder) = pattern.remainder {
                    identifiers.push((
                        remainder.clone(),
                        Object::array(arr[pattern.contents.len()..].to_vec()),
                    ));
                }
                MatchResult::Match(identifiers)
            }
            (Pattern::HashPattern(pattern), Object::Hash(hash)) => {
                let mut identifiers = Vec::new();
                if (pattern.contents.len() > hash.len())
                    || (pattern.remainder.is_none() && (pattern.contents.len() != hash.len()))
                {
                    return MatchResult::NoMatch;
                }
                for (left_key, left_value) in pattern.contents.iter() {
                    if let Some((_right_key, right_value)) = hash.get(left_key) {
                        if let MatchResult::Match(vec) = left_value.matches(right_value) {
                            identifiers.extend(vec);
                        } else {
                            return MatchResult::NoMatch;
                        }
                    } else {
                        return MatchResult::NoMatch;
                    }
                }
                if let Some(ref remainder) = pattern.remainder {
                    identifiers.push((
                        remainder.clone(),
                        Object::hash(
                            hash.iter()
                                .filter(|(key, _)| !pattern.contents.iter().any(|(k, _)| k == *key))
                                .map(|(key, value)| (key.clone(), value.clone()))
                                .collect(),
                        ),
                    ));
                }
                MatchResult::Match(identifiers)
            }
            _ => MatchResult::NoMatch,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use monkey_lang_core::lexer::Tokenizer;
    use crate::object::{EvaluationError, Object};
    use monkey_lang_core::parser::Parser;

    fn test_evaluation(inputs: Vec<(&str, Result<Rc<Object>, EvaluationError>)>) {
        for (input, output) in inputs {
            let tokenizer = Tokenizer::new(input);
            let mut parser = Parser::new(tokenizer);
            let ast = parser.parse_program().unwrap();
            let result = super::eval_program(&ast, &mut super::Environment::new());

            assert_eq!(result, output);
        }
    }

    #[test]
    fn test_literal() {
        let inputs = vec![
            ("5;", Ok(Object::integer(5))),
            ("true;", Ok(Object::boolean(true))),
            ("false;", Ok(Object::boolean(false))),
            ("\"hello\";", Ok(Object::string("hello".to_owned()))),
            ("null;", Ok(Object::null())),
        ];

        test_evaluation(inputs);
    }

    #[test]
    fn big_test() {
        let inputs = vec![
            ("--5;", Ok(Object::integer(5))),
            ("56;", Ok(Object::integer(56))),
            ("-10;", Ok(Object::integer(-10))),
            ("true;", Ok(Object::boolean(true))),
            ("false;", Ok(Object::boolean(false))),
            ("!false;", Ok(Object::boolean(true))),
            ("!!true;", Ok(Object::boolean(true))),
        ];

        test_evaluation(inputs);
    }

    #[test]
    fn test_let_statements() {
        let inputs = vec![
            ("let a = 5; a;", Ok(Object::integer(5))),
            ("let a = 5 * 5; a;", Ok(Object::integer(25))),
            ("let a = 5; let b = a; b;", Ok(Object::integer(5))),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Ok(Object::integer(15)),
            ),
        ];

        test_evaluation(inputs);
    }

    #[test]
    fn test_function_application() {
        let inputs = vec![
            (
                "let identity = fn(x) { x }; identity(5)",
                Ok(Object::integer(5)),
            ),
            (
                "let identity = fn(x) { return x }; identity(5)",
                Ok(Object::integer(5)),
            ),
            (
                "let double = fn(x) { x * 2 }; double(5)",
                Ok(Object::integer(10)),
            ),
            (
                "let add = fn(x, y) { x + y }; add(5, 5)",
                Ok(Object::integer(10)),
            ),
            (
                "let add = fn(x, y) { x + y }; add(5 + 5, add(5, 5))",
                Ok(Object::integer(20)),
            ),
            ("fn(x) { x }(5)", Ok(Object::integer(5))),
            (
                "
                let factorial = fn(n) {
                    if n < 2 {1}
                    else {factorial(n - 1) * n}
                };
                factorial(3)",
                Ok(Object::integer(6)),
            ),
            (
                "
                let func = fn(a) {
                    fn(b) {
                        a + b
                    }
                };
                func(5)(10)",
                Ok(Object::integer(15)),
            ),
        ];

        test_evaluation(inputs);
    }

    #[test]
    fn test_closure() {
        let inputs = vec![
            (
                r#"
            let fa = fn() {
                let x = 5;
                let fb = fn() {
                    x
                };
                fb
            };
            let temp = fa();
            temp()"#,
                Ok(Object::integer(5)),
            ),
            (
                r#"
            let fa = fn() {
                let x = 5;
                let fb = fn() {
                    fn() { x }
                };
                fb
            };
            let temp = fa();
            let temp_ = temp();
            temp_()"#,
                Ok(Object::integer(5)),
            ),
            (
                r#"
            let fa = fn() {
                let is_even = fn(x) {
                    if x == 0 {
                        true
                    } else {
                        is_odd(x-1)
                    }
                };
                let is_odd = fn(x) {
                    if x == 0 {
                        false
                    } else {
                        is_even(x-1)
                    }
                };
                is_even
            };
            let temp = fa();
            temp(3);"#,
                Ok(Object::boolean(false)),
            ),
        ];

        test_evaluation(inputs)
    }

    #[test]
    fn test_unused_function_envs_are_dropped() {
        let input = r#"
        let fa = fn() {
            let x = 5;
            let fb = fn() {
                x
            };
            fb
        };
        fa()"#;
        let tokenizer = Tokenizer::new(input);
        let mut parser = Parser::new(tokenizer);
        let ast = parser.parse_program().unwrap();
        let mut env = super::Environment::new();
        let mut result = super::eval_program(&ast, &mut env).expect("No errors in execution");

        // `fa` is the only variable in the environment
        assert_eq!(env.environment.borrow().store.len(), 1);

        match Rc::<Object>::get_mut(&mut result)
            .expect("This should be the only reference to this object")
        {
            Object::Function(f) => {
                // parent_env should be kept alive in captured_environments
                assert!(f.parent_env.upgrade().is_some());
                assert_eq!(f.captured_environments.len(), 1);
                // We remove the captured environment causing parent_env to be
                // dropped
                f.captured_environments.clear();
                assert!(f.parent_env.upgrade().is_none());
            }
            _ => panic!("The output should be a function"),
        }
    }

    #[test]
    fn test_match_expression() {
        let inputs = vec![
            ("match true {true=>{1}}", Ok(Object::integer(1))),
            ("match false {true=>{1} false=>{2}}", Ok(Object::integer(2))),
            (
                r#"match "asd" {a => {a}}"#,
                Ok(Object::string("asd".to_owned())),
            ),
            (
                r#"match 5 {
                    true => {1}
                    2 => {2}
                    "asd" => {3}
                    5 => {4}
                    null => {5}
                }"#,
                Ok(Object::integer(4)),
            ),
            (
                r#"match [1,2,3] {
                    [1, ...a] => {a}
                }"#,
                Ok(Object::array(vec![Object::integer(2), Object::integer(3)])),
            ),
            (
                r#"match [[1, 2], 3] {
                    [[1, a], 3] => {a}
                }"#,
                Ok(Object::integer(2)),
            ),
            (
                r#"match {1: "2", false: 4} {
                    {1: "2", false: a} => {a}
                }"#,
                Ok(Object::integer(4)),
            ),
            (
                r#"match {1: {"2": 3}, false: 4, true: 9} {
                    {1: {"2": b}, false: a, ...c} => {b}
                }"#,
                Ok(Object::integer(3)),
            ),
        ];

        test_evaluation(inputs);
    }
}

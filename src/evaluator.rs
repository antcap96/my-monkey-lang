use std::collections::HashMap;

use crate::ast::Expression;
use crate::environment::Environment;
use crate::object::{EvaluationError, Object, QuickReturn};
use gc::Gc;

pub fn eval_program(
    program: &crate::ast::Program,
    environment: &mut Environment,
) -> Result<Gc<Object>, EvaluationError> {
    let mut output = Object::null();
    for statement in &program.statements {
        let result = eval_statement(&statement, environment);

        match result {
            Err(QuickReturn::Return(value)) => return Ok(value),
            Err(QuickReturn::Error(error)) => return Err(error),
            Ok(object) => output = object,
        };
    }
    Ok(output)
}

fn eval_statement(
    statement: &crate::ast::Statement,
    environment: &mut Environment,
) -> Result<Gc<Object>, QuickReturn> {
    match statement {
        crate::ast::Statement::Expression(expression) => eval_expression(expression, environment),
        crate::ast::Statement::Return(statement) => eval_return_statement(statement, environment),
        crate::ast::Statement::Let(statement) => eval_let_statement(statement, environment),
    }
}

fn eval_let_statement(
    statement: &crate::ast::LetStatement,
    environment: &mut Environment,
) -> Result<Gc<Object>, QuickReturn> {
    let value = eval_expression(&statement.value, environment)?;
    environment.set(statement.identifier.name.clone(), value.clone());
    Ok(value)
}

fn eval_return_statement(
    statement: &crate::ast::ReturnStatement,
    environment: &mut Environment,
) -> Result<Gc<Object>, QuickReturn> {
    let value = eval_expression(&statement.value, environment)?;
    Err(QuickReturn::Return(value))
}

fn eval_expression(
    expression: &Expression,
    environment: &mut Environment,
) -> Result<Gc<Object>, QuickReturn> {
    match expression {
        Expression::IntegerLiteral(value) => Ok(Object::integer(*value)),
        Expression::BooleanLiteral(value) => Ok(Object::boolean(*value)),
        Expression::StringLiteral(value) => Ok(Object::string(value.clone())),
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
                let hashed_key = crate::object::HashableObject::from_object(&key)
                    .map_err(|e| QuickReturn::Error(e))?;
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
                _ => Err(QuickReturn::Error(EvaluationError::IndexNotSupported(left))),
            }
        }
    }
}

fn eval_call_function(
    function: &crate::object::Function,
    arguments: &Vec<Expression>,
    environment: &mut Environment,
) -> Result<Gc<Object>, QuickReturn> {
    if function.parameters.len() != arguments.len() {
        let expected = function.parameters.len();
        return Err(QuickReturn::Error(EvaluationError::WrongArgumentCount {
            function: function.clone(),
            expected,
            actual: arguments.len(),
        }));
    }
    let arguments = eval_expressions(arguments, environment)?;
    apply_function(function, arguments).map_err(|err| QuickReturn::Error(err))
}

fn eval_call_builtin_function(
    function: &crate::object::BuiltinFunction,
    arguments: &Vec<Expression>,
    environment: &mut Environment,
) -> Result<Gc<Object>, QuickReturn> {
    let arguments = eval_expressions(arguments, environment)?;
    (function.func)(arguments)
}

fn eval_expressions(
    arguments: &Vec<Expression>,
    environment: &mut Environment,
) -> Result<Vec<Gc<Object>>, QuickReturn> {
    let mut result = Vec::new();
    for argument in arguments {
        result.push(eval_expression(argument, environment)?);
    }
    Ok(result)
}

fn apply_function(
    function: &crate::object::Function,
    arguments: Vec<Gc<Object>>,
) -> Result<Gc<Object>, EvaluationError> {
    let mut new_environment = Environment::new_enclosed(function.env.clone());
    for (parameter, argument) in function.parameters.iter().zip(arguments.iter()) {
        new_environment.set(parameter.name.clone(), argument.clone());
    }
    let result = eval_block_statement(&function.body, &mut new_environment);
    match result {
        Ok(object) => Ok(object),
        Err(QuickReturn::Return(value)) => Ok(value),
        Err(QuickReturn::Error(err)) => Err(err),
    }
}

fn eval_block_statement(
    block: &crate::ast::BlockStatement,
    environment: &mut Environment,
) -> Result<Gc<Object>, QuickReturn> {
    let mut result = Object::null();
    for statement in &block.statements {
        result = eval_statement(statement, environment)?;
    }
    Ok(result)
}

fn eval_prefix_operation(
    kind: &crate::ast::PrefixOperationKind,
    right: Result<Gc<Object>, QuickReturn>,
) -> Result<Gc<Object>, QuickReturn> {
    let right = right?;
    match (&kind, right.as_ref()) {
        (crate::ast::PrefixOperationKind::Bang, Object::Boolean(value)) => {
            Ok(Object::boolean(!value))
        }
        (crate::ast::PrefixOperationKind::Minus, Object::Integer(value)) => {
            Ok(Object::integer(-value))
        }
        _ => Err(QuickReturn::Error(EvaluationError::UnknownPrefixOperator {
            right: Box::new(right),
            operation: kind.clone(),
        })),
    }
}

fn eval_infix_operation(
    kind: &crate::ast::InfixOperationKind,
    left: Result<Gc<Object>, QuickReturn>,
    right: Result<Gc<Object>, QuickReturn>,
) -> Result<Gc<Object>, QuickReturn> {
    use crate::ast::InfixOperationKind;
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

#[cfg(test)]
mod tests {
    use crate::{lexer::Tokenizer, object::Object, parser::Parser};

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

        for (input, output) in inputs {
            let tokenizer = Tokenizer::new(input);
            let mut parser = Parser::new(tokenizer);
            let ast = parser.parse_program().unwrap();
            let result = super::eval_program(&ast, &mut super::Environment::new());

            assert_eq!(result, output);
        }
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

        for (input, output) in inputs {
            let tokenizer = Tokenizer::new(input);
            let mut parser = Parser::new(tokenizer);
            let ast = parser.parse_program().unwrap();
            let result = super::eval_program(&ast, &mut super::Environment::new());

            assert_eq!(result, output);
        }
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

        for (input, output) in inputs {
            let tokenizer = Tokenizer::new(input);
            let mut parser = Parser::new(tokenizer);
            let ast = parser.parse_program().unwrap();
            let result = super::eval_program(&ast, &mut super::Environment::new());

            assert_eq!(result, output);
        }
    }
}

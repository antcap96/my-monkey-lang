use crate::ast::Expression;
use crate::environment::Environment;
use crate::object::{EvaluationError, Object, ObjectCore, QuickReturn};

pub fn eval_program(
    program: &crate::ast::Program,
    environment: &mut Environment,
) -> Result<Object, EvaluationError> {
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
) -> Result<Object, QuickReturn> {
    match statement {
        crate::ast::Statement::Expression(expression) => eval_expression(expression, environment),
        crate::ast::Statement::Return(statement) => eval_return_statement(statement, environment),
        crate::ast::Statement::Let(statement) => eval_let_statement(statement, environment),
    }
}

fn eval_let_statement(
    statement: &crate::ast::LetStatement,
    environment: &mut Environment,
) -> Result<Object, QuickReturn> {
    let value = eval_expression(&statement.value, environment)?;
    environment.set(statement.identifier.name.clone(), value.clone());
    Ok(value)
}

fn eval_return_statement(
    statement: &crate::ast::ReturnStatement,
    environment: &mut Environment,
) -> Result<Object, QuickReturn> {
    let value = eval_expression(&statement.value, environment)?;
    Err(QuickReturn::Return(value))
}

fn eval_expression(
    expression: &Expression,
    environment: &mut Environment,
) -> Result<Object, QuickReturn> {
    match expression {
        Expression::IntegerLiteral(value) => Ok(Object::integer(*value)),
        Expression::BooleanLiteral(value) => Ok(Object::boolean(*value)),
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
            match condition.core_ref() {
                ObjectCore::Boolean(value) => {
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
            let ObjectCore::Function(function) = function.core_ref() else {
                return Err(QuickReturn::Error(EvaluationError::CallNonFunction(
                    function,
                )));};
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
    }
}

fn eval_expressions(
    arguments: &Vec<Expression>,
    environment: &mut Environment,
) -> Result<Vec<Object>, QuickReturn> {
    let mut result = Vec::new();
    for argument in arguments {
        result.push(eval_expression(argument, environment)?);
    }
    Ok(result)
}

fn apply_function(
    function: &crate::object::Function,
    arguments: Vec<Object>,
) -> Result<Object, EvaluationError> {
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
) -> Result<Object, QuickReturn> {
    let mut result = Object::null();
    for statement in &block.statements {
        result = eval_statement(statement, environment)?;
    }
    Ok(result)
}

fn eval_prefix_operation(
    kind: &crate::ast::PrefixOperationKind,
    right: Result<Object, QuickReturn>,
) -> Result<Object, QuickReturn> {
    let right = right?;
    match (&kind, right.core_ref()) {
        (crate::ast::PrefixOperationKind::Bang, ObjectCore::Boolean(value)) => {
            Ok(Object::boolean(!value))
        }
        (crate::ast::PrefixOperationKind::Minus, ObjectCore::Integer(value)) => {
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
    left: Result<Object, QuickReturn>,
    right: Result<Object, QuickReturn>,
) -> Result<Object, QuickReturn> {
    use crate::ast::InfixOperationKind;
    let left = left?;
    let right = right?;
    match (kind, left.core_ref(), right.core_ref()) {
        (InfixOperationKind::Plus, ObjectCore::Integer(left), ObjectCore::Integer(right)) => {
            Ok(Object::integer(left + right))
        }
        (InfixOperationKind::Minus, ObjectCore::Integer(left), ObjectCore::Integer(right)) => {
            Ok(Object::integer(left - right))
        }
        (InfixOperationKind::Multiply, ObjectCore::Integer(left), ObjectCore::Integer(right)) => {
            Ok(Object::integer(left * right))
        }
        (InfixOperationKind::Divide, ObjectCore::Integer(left), ObjectCore::Integer(right)) => {
            Ok(Object::integer(left / right))
        }
        (InfixOperationKind::LessThan, ObjectCore::Integer(left), ObjectCore::Integer(right)) => {
            Ok(Object::boolean(left < right))
        }
        (
            InfixOperationKind::GreaterThan,
            ObjectCore::Integer(left),
            ObjectCore::Integer(right),
        ) => Ok(Object::boolean(left > right)),
        (InfixOperationKind::Equal, ObjectCore::Integer(left), ObjectCore::Integer(right)) => {
            Ok(Object::boolean(left == right))
        }
        (InfixOperationKind::NotEqual, ObjectCore::Integer(left), ObjectCore::Integer(right)) => {
            Ok(Object::boolean(left != right))
        }
        (InfixOperationKind::Equal, ObjectCore::Boolean(left), ObjectCore::Boolean(right)) => {
            Ok(Object::boolean(left == right))
        }
        (InfixOperationKind::NotEqual, ObjectCore::Boolean(left), ObjectCore::Boolean(right)) => {
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
                "let identity = fn(x) { x; }; identity(5);",
                Ok(Object::integer(5)),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Ok(Object::integer(5)),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Ok(Object::integer(10)),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);",
                Ok(Object::integer(10)),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Ok(Object::integer(20)),
            ),
            ("fn(x) { x; }(5)", Ok(Object::integer(5))),
            (
                "
                let factorial = fn(n) {
                    if n < 2 {1;}
                    else {factorial(n - 1) * n;};
                };
                factorial(3);",
                Ok(Object::integer(6)),
            ),
            (
                "
                let func = fn(a) {
                    fn(b) {
                        a + b;
                    };
                };
                func(5)(10);",
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

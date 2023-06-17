use crate::ast::Expression;
use crate::environment::Environment;
use crate::object::{EvaluationError, Object, QuickReturn};
use std::cell::RefCell;
use std::rc::Rc;
// page 128

/**
I'm guessing I have to split BlockStatement into a type that gives an object
and a type that gives a returnable object. If statements have a returnable object
while functions always give just an object.
*/

// TODO: should this be Rc<Object>?
pub fn eval_program(
    program: crate::ast::Program,
    environment: &mut Rc<RefCell<Environment>>,
) -> Result<Object, EvaluationError> {
    let mut output = Object::Null;
    for statement in program.statements {
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
    statement: crate::ast::Statement,
    environment: &mut Rc<RefCell<Environment>>,
) -> Result<Object, QuickReturn> {
    match statement {
        crate::ast::Statement::Expression(expression) => eval_expression(expression, environment),
        crate::ast::Statement::Return(statement) => eval_return_statement(statement, environment),
        crate::ast::Statement::Let(statement) => eval_let_statement(statement, environment),
    }
}

fn eval_let_statement(
    statement: crate::ast::LetStatement,
    environment: &mut Rc<RefCell<Environment>>,
) -> Result<Object, QuickReturn> {
    let value = eval_expression(statement.value, environment)?;
    environment
        .borrow_mut()
        .set(&statement.name.name, value.clone());
    Ok(value)
}

fn eval_return_statement(
    statement: crate::ast::ReturnStatement,
    environment: &mut Rc<RefCell<Environment>>,
) -> Result<Object, QuickReturn> {
    let value = eval_expression(statement.value, environment)?;
    Err(QuickReturn::Return(value))
}

fn eval_expression(
    expression: Expression,
    environment: &mut Rc<RefCell<Environment>>,
) -> Result<Object, QuickReturn> {
    match expression {
        Expression::IntegerLiteral(value) => Ok(Object::Integer(value)),
        Expression::BooleanLiteral(value) => Ok(Object::Boolean(value)),
        Expression::Identifier(identifier) => {
            environment
                .borrow()
                .get(&identifier.name)
                .ok_or(QuickReturn::Error(EvaluationError::UnknownIdentifier(
                    identifier.name,
                )))
        }
        Expression::PrefixOperation(kind, expression) => {
            let right = eval_expression(*expression, environment);
            eval_prefix_operation(kind, right)
        }
        Expression::InfixOperation(kind, left, right) => {
            let left = eval_expression(*left, environment);
            let right = eval_expression(*right, environment);
            eval_infix_operation(kind, left, right)
        }
        Expression::IfExpression {
            condition,
            consequence,
            alternative,
        } => {
            let condition = eval_expression(*condition, environment)?;
            match condition {
                Object::Boolean(value) => {
                    if value {
                        eval_block_statement(consequence, environment)
                    } else if let Some(alternative) = alternative {
                        eval_block_statement(alternative, environment)
                    } else {
                        Ok(Object::Null)
                    }
                }
                _ => Err(QuickReturn::Error(EvaluationError::NonBooleanCondition(
                    condition,
                ))),
            }
        }
        Expression::FunctionLiteral { parameters, body } => {
            Ok(Object::Function(crate::object::Function {
                parameters,
                body,
                env: Rc::clone(environment),
            }))
        }
        Expression::CallExpression {
            function,
            arguments,
        } => {
            let function = eval_expression(*function, environment)?;
            let Object::Function(function) = function else {
                return Err(QuickReturn::Error(EvaluationError::CallNonFunction(
                    function,
                )));};
            if function.parameters.len() != arguments.len() {
                let expected = function.parameters.len();
                return Err(QuickReturn::Error(EvaluationError::WrongArgumentCount {
                    function,
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
    arguments: Vec<Expression>,
    environment: &mut Rc<RefCell<Environment>>,
) -> Result<Vec<Object>, QuickReturn> {
    let mut result = Vec::new();
    for argument in arguments {
        result.push(eval_expression(argument, environment)?);
    }
    Ok(result)
}

fn apply_function(
    function: crate::object::Function,
    arguments: Vec<Object>,
) -> Result<Object, EvaluationError> {
    let mut new_environment = Environment::new_enclosed(Rc::clone(&function.env));
    for (parameter, argument) in function.parameters.iter().zip(arguments.iter()) {
        new_environment
            .borrow_mut()
            .set(&parameter.name, argument.clone());
    }
    let result = eval_block_statement(function.body, &mut new_environment);
    match result {
        Ok(ok @ _) => Ok(ok),
        Err(QuickReturn::Return(value)) => Ok(value),
        Err(QuickReturn::Error(err)) => Err(err),
    }
}

fn eval_block_statement(
    block: crate::ast::BlockStatement,
    environment: &mut Rc<RefCell<Environment>>,
) -> Result<Object, QuickReturn> {
    let mut result = Object::Null;
    for statement in block.statements {
        result = eval_statement(statement, environment)?;
    }
    Ok(result)
}

fn eval_prefix_operation(
    kind: crate::ast::PrefixOperationKind,
    right: Result<Object, QuickReturn>,
) -> Result<Object, QuickReturn> {
    let right = right?;
    match (&kind, &right) {
        (crate::ast::PrefixOperationKind::Bang, Object::Boolean(value)) => {
            Ok(Object::Boolean(!value))
        }
        (crate::ast::PrefixOperationKind::Minus, Object::Integer(value)) => {
            Ok(Object::Integer(-value))
        }
        _ => Err(QuickReturn::Error(EvaluationError::UnknownPrefixOperator {
            right: Box::new(right),
            operation: kind,
        })),
    }
}

fn eval_infix_operation(
    kind: crate::ast::InfixOperationKind,
    left: Result<Object, QuickReturn>,
    right: Result<Object, QuickReturn>,
) -> Result<Object, QuickReturn> {
    use crate::ast::InfixOperationKind;
    let left = left?;
    let right = right?;
    match (&kind, &left, &right) {
        (InfixOperationKind::Plus, Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::Integer(left + right))
        }
        (InfixOperationKind::Minus, Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::Integer(left - right))
        }
        (InfixOperationKind::Multiply, Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::Integer(left * right))
        }
        (InfixOperationKind::Divide, Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::Integer(left / right))
        }
        (InfixOperationKind::LessThan, Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::Boolean(left < right))
        }
        (InfixOperationKind::GreaterThan, Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::Boolean(left > right))
        }
        (InfixOperationKind::Equal, Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::Boolean(left == right))
        }
        (InfixOperationKind::NotEqual, Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::Boolean(left != right))
        }
        (InfixOperationKind::Equal, Object::Boolean(left), Object::Boolean(right)) => {
            Ok(Object::Boolean(left == right))
        }
        (InfixOperationKind::NotEqual, Object::Boolean(left), Object::Boolean(right)) => {
            Ok(Object::Boolean(left != right))
        }
        _ => Err(QuickReturn::Error(EvaluationError::UnknownInfixOperator {
            left: Box::new(left),
            right: Box::new(right),
            operation: kind,
        })),
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Tokenizer, object::Object, parser::Parser};

    #[test]
    fn big_test() {
        let inputs = vec![
            ("--5;", Ok(Object::Integer(5))),
            ("56;", Ok(Object::Integer(56))),
            ("-10;", Ok(Object::Integer(-10))),
            ("true;", Ok(Object::Boolean(true))),
            ("false;", Ok(Object::Boolean(false))),
            ("!false;", Ok(Object::Boolean(true))),
            ("!!true;", Ok(Object::Boolean(true))),
        ];

        for (input, output) in inputs {
            let tokenizer = Tokenizer::new(input);
            let mut parser = Parser::new(tokenizer);
            let ast = parser.parse_program().unwrap();
            let result = super::eval_program(ast, &mut super::Environment::new());

            assert_eq!(result, output);
        }
    }

    #[test]
    fn test_let_statements() {
        let inputs = vec![
            ("let a = 5; a;", Ok(Object::Integer(5))),
            ("let a = 5 * 5; a;", Ok(Object::Integer(25))),
            ("let a = 5; let b = a; b;", Ok(Object::Integer(5))),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Ok(Object::Integer(15)),
            ),
        ];

        for (input, output) in inputs {
            let tokenizer = Tokenizer::new(input);
            let mut parser = Parser::new(tokenizer);
            let ast = parser.parse_program().unwrap();
            let result = super::eval_program(ast, &mut super::Environment::new());

            assert_eq!(result, output);
        }
    }

    #[test]
    fn test_function_application() {
        let inputs = vec![
            (
                "let identity = fn(x) { x; }; identity(5);",
                Ok(Object::Integer(5)),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Ok(Object::Integer(5)),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Ok(Object::Integer(10)),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);",
                Ok(Object::Integer(10)),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Ok(Object::Integer(20)),
            ),
            ("fn(x) { x; }(5)", Ok(Object::Integer(5))),
            (
                "
                let factorial = fn(n) {
                    if n < 2 {1;}
                    else {factorial(n - 1) * n;};
                };
                factorial(3);",
                Ok(Object::Integer(6)),
            ),
            (
                "
                let func = fn(a) {
                    fn(b) {
                        a + b;
                    };
                };
                func(5)(10);",
                Ok(Object::Integer(15)),
            ),
        ];

        for (input, output) in inputs {
            let tokenizer = Tokenizer::new(input);
            let mut parser = Parser::new(tokenizer);
            let ast = parser.parse_program().unwrap();
            let result = super::eval_program(ast, &mut super::Environment::new());

            assert_eq!(result, output);
        }
    }
}

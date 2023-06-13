use crate::ast::Expression;
use crate::environment::Environment;
use crate::object::{EvaluationError, Object, QuickReturn};

// page 128

/**
I'm guessing I have to split BlockStatement into a type that gives an object
and a type that gives a returnable object. If statements have a returnable object
while functions always give just an object.
*/

// TODO: should this be Rc<Object>?
pub fn eval_program(
    program: crate::ast::Program,
    environment: &mut Environment,
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
    environment: &mut Environment,
) -> Result<Object, QuickReturn> {
    match statement {
        crate::ast::Statement::Expression(expression) => eval_expression(expression, environment),
        crate::ast::Statement::Return(statement) => eval_return_statement(statement, environment),
        crate::ast::Statement::Let(statement) => eval_let_statement(statement, environment),
    }
}

fn eval_let_statement(
    statement: crate::ast::LetStatement,
    environment: &mut Environment,
) -> Result<Object, QuickReturn> {
    let value = eval_expression(statement.value, environment)?;
    environment.set(&statement.name.name, value.clone());
    Ok(value)
}

fn eval_return_statement(
    statement: crate::ast::ReturnStatement,
    environment: &mut Environment,
) -> Result<Object, QuickReturn> {
    let value = eval_expression(statement.value, environment)?;
    Err(QuickReturn::Return(value))
}

fn eval_expression(
    expression: Expression,
    environment: &mut Environment,
) -> Result<Object, QuickReturn> {
    match expression {
        Expression::IntegerLiteral(value) => Ok(Object::Integer(value)),
        Expression::BooleanLiteral(value) => Ok(Object::Boolean(value)),
        Expression::Identifier(identifier) => environment.get(&identifier.name).ok_or(
            QuickReturn::Error(EvaluationError::UnknownIdentifier(identifier.name)),
        ),
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
        _ => todo!(),
    }
}

fn eval_block_statement(
    block: crate::ast::BlockStatement,
    environment: &mut Environment,
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
}

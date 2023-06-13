use crate::ast::Expression;
use crate::object::Object;

// page 128

/**
I'm guessing I have to split BlockStatement into a type that gives an object
and a type that gives a returnable object. If statements have a returnable object
while functions always give just an object.
*/

// TODO: should this be Rc<Object>?
pub fn eval_program(program: crate::ast::Program) -> Object {
    eval_statements(program.statements)
}

fn eval_statements(statements: Vec<crate::ast::Statement>) -> Object {
    let mut result = Object::Null;
    for statement in statements {
        result = eval_statement(statement);
    }
    result
}

fn eval_statement(statement: crate::ast::Statement) -> Object {
    match statement {
        crate::ast::Statement::Expression(expression) => eval_expression(expression),
        _ => todo!(),
    }
}

fn eval_expression(expression: Expression) -> Object {
    match expression {
        Expression::IntegerLiteral(value) => Object::Integer(value),
        Expression::BooleanLiteral(value) => Object::Boolean(value),
        Expression::PrefixOperation(kind, expression) => {
            let right = eval_expression(*expression);
            eval_prefix_operation(kind, right)
        }
        Expression::InfixOperation(kind, left, right) => {
            let left = eval_expression(*left);
            let right = eval_expression(*right);
            eval_infix_operation(kind, left, right)
        }
        Expression::IfExpression { condition, consequence, alternative } => {
            let condition = eval_expression(*condition);
            match condition {
                Object::Boolean(value) => {
                    if value {
                        eval_block_statement(consequence)
                    } else if let Some(alternative) = alternative {
                        eval_block_statement(alternative)
                    } else {
                        Object::Null
                    }
                }
                _ => Object::Null,
            }
        }
        _ => todo!(),
    }
}

fn eval_block_statement(block: crate::ast::BlockStatement) -> Object {
    eval_statements(block.statements)
}

fn eval_prefix_operation(kind: crate::ast::PrefixOperationKind, right: Object) -> Object {
    match kind {
        crate::ast::PrefixOperationKind::Bang => match right {
            Object::Boolean(value) => Object::Boolean(!value),
            _ => Object::Null,
        },
        crate::ast::PrefixOperationKind::Minus => match right {
            Object::Integer(value) => Object::Integer(-value),
            _ => Object::Null,
        },
    }
}

fn eval_infix_operation(
    kind: crate::ast::InfixOperationKind,
    left: Object,
    right: Object,
) -> Object {
    use crate::ast::InfixOperationKind;
    match kind {
        InfixOperationKind::Plus => {
            infix_integer_only_operation(left, right, |left, right| Object::Integer(left + right))
        }
        InfixOperationKind::Minus => {
            infix_integer_only_operation(left, right, |left, right| Object::Integer(left - right))
        }
        InfixOperationKind::Multiply => {
            infix_integer_only_operation(left, right, |left, right| Object::Integer(left * right))
        }
        InfixOperationKind::Divide => {
            infix_integer_only_operation(left, right, |left, right| Object::Integer(left / right))
        }
        InfixOperationKind::LessThan => {
            infix_integer_only_operation(left, right, |left, right| Object::Boolean(left < right))
        }
        InfixOperationKind::GreaterThan => {
            infix_integer_only_operation(left, right, |left, right| Object::Boolean(left > right))
        }
        InfixOperationKind::Equal => infix_complete_operation(
            left,
            right,
            |left, right| Object::Boolean(left == right),
            |left, right| Object::Boolean(left == right),
        ),
        InfixOperationKind::NotEqual => infix_complete_operation(
            left,
            right,
            |left, right| Object::Boolean(left != right),
            |left, right| Object::Boolean(left != right),
        ),
    }
}

fn infix_integer_only_operation(
    left: Object,
    right: Object,
    op: impl FnOnce(i64, i64) -> Object,
) -> Object {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => op(left, right),
        _ => Object::Null,
    }
}

fn infix_complete_operation(
    left: Object,
    right: Object,
    bool_op: impl FnOnce(bool, bool) -> Object,
    integer_op: impl FnOnce(i64, i64) -> Object,
) -> Object {
    match (left, right) {
        (Object::Boolean(left), Object::Boolean(right)) => bool_op(left, right),
        (Object::Integer(left), Object::Integer(right)) => integer_op(left, right),
        _ => Object::Null,
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Tokenizer, object::Object, parser::Parser};

    #[test]
    fn test_literals() {
        let inputs = vec![
            ("--5;", Object::Integer(5)),
            ("56;", Object::Integer(56)),
            ("-10;", Object::Integer(-10)),
            ("true;", Object::Boolean(true)),
            ("false;", Object::Boolean(false)),
            ("!false;", Object::Boolean(true)),
            ("!!true;", Object::Boolean(true)),
        ];

        for (input, output) in inputs {
            let tokenizer = Tokenizer::new(input);
            let mut parser = Parser::new(tokenizer);
            let ast = parser.parse_program().unwrap();
            let result = super::eval_program(ast);

            assert_eq!(result, output);
        }
    }
}

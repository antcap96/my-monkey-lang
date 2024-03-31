use std::rc::Rc;

use crate::object::{BuiltinFunction, EvaluationError, Object, QuickReturn};

fn unexpected_number_of_arguments_error(expected: usize, got: usize) -> QuickReturn {
    QuickReturn::Error(EvaluationError::BuiltinFunctionError(
        format!(
            "unexpected number of arguments. Expected {} got {}",
            expected, got
        )
        .into(),
    ))
}

fn builtin_first(args: Vec<Rc<Object>>) -> Result<Rc<Object>, QuickReturn> {
    if args.len() != 1 {
        return Err(unexpected_number_of_arguments_error(1, args.len()));
    }
    match args[0].as_ref() {
        Object::Array(arr) => {
            if arr.is_empty() {
                return Ok(Object::null());
            }
            Ok(arr[0].clone())
        }
        _ => Err(QuickReturn::Error(EvaluationError::BuiltinFunctionError(
            format!("unexpected argument type. Expected Array got {:?}", args[0]).into(),
        ))),
    }
}

fn builtin_last(args: Vec<Rc<Object>>) -> Result<Rc<Object>, QuickReturn> {
    if args.len() != 1 {
        return Err(unexpected_number_of_arguments_error(1, args.len()));
    }
    match args[0].as_ref() {
        Object::Array(arr) => {
            if arr.is_empty() {
                return Ok(Object::null());
            }
            Ok(arr[arr.len() - 1].clone())
        }
        _ => Err(QuickReturn::Error(EvaluationError::BuiltinFunctionError(
            format!("unexpected argument type. Expected Array got {:?}", args[0]).into(),
        ))),
    }
}

fn builtin_len(args: Vec<Rc<Object>>) -> Result<Rc<Object>, QuickReturn> {
    if args.len() != 1 {
        return Err(unexpected_number_of_arguments_error(1, args.len()));
    }
    match args[0].as_ref() {
        Object::String(s) => Ok(Object::integer(s.len() as i64)),
        Object::Array(arr) => Ok(Object::integer(arr.len() as i64)),
        _ => Err(QuickReturn::Error(EvaluationError::BuiltinFunctionError(
            format!(
                "unexpected argument type. Expected String got {:?}",
                args[0]
            )
            .into(),
        ))),
    }
}

fn builtin_push(args: Vec<Rc<Object>>) -> Result<Rc<Object>, QuickReturn> {
    if args.len() != 2 {
        return Err(unexpected_number_of_arguments_error(2, args.len()));
    }
    match args[0].as_ref() {
        Object::Array(arr) => {
            let mut new_arr = arr.clone();
            new_arr.push(args[1].clone());
            Ok(Object::array(new_arr))
        }
        _ => Err(QuickReturn::Error(EvaluationError::BuiltinFunctionError(
            format!("unexpected argument type. Expected Array got {:?}", args[0]).into(),
        ))),
    }
}

fn builtin_tail(args: Vec<Rc<Object>>) -> Result<Rc<Object>, QuickReturn> {
    if args.len() != 1 {
        return Err(unexpected_number_of_arguments_error(1, args.len()));
    }
    match args[0].as_ref() {
        Object::Array(arr) => {
            if arr.is_empty() {
                return Ok(Object::null());
            }
            Ok(Object::array(arr[1..].to_owned()))
        }
        _ => Err(QuickReturn::Error(EvaluationError::BuiltinFunctionError(
            format!("unexpected argument type. Expected Array got {:?}", args[0]).into(),
        ))),
    }
}

pub fn builtin_to_string(args: Vec<Rc<Object>>) -> Result<Rc<Object>, QuickReturn> {
    if args.len() != 1 {
        return Err(unexpected_number_of_arguments_error(1, args.len()));
    }
    Ok(Object::string(to_string(args[0].clone())))
}

fn to_string(obj: Rc<Object>) -> String {
    match obj.as_ref() {
        Object::String(s) => s.clone(),
        Object::Integer(i) => i.to_string(),
        Object::Boolean(b) => b.to_string(),
        Object::Null => "null".to_owned(),
        Object::Array(arr) => {
            let mut s = "[".to_owned();
            for (i, obj) in arr.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                s.push_str(&to_string(obj.clone()));
            }
            s.push(']');
            s
        }
        Object::Hash(hash) => {
            let mut s = "{".to_owned();
            for (i, (key, value)) in hash.values().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                s.push_str(&format!(
                    "{}: {}",
                    to_string(key.clone()),
                    to_string(value.clone())
                ));
            }
            s.push('}');
            s
        }
        Object::Function(_) => format!("<fn@{:x}>", obj.as_ref() as *const Object as usize),
        Object::BuiltinFunction(_) => {
            format!("<builtin-fn@{:x}>", obj.as_ref() as *const Object as usize)
        }
    }
}

// TODO: have only a single version of each builtin function.
// Consider keeping the name in the object
pub(crate) fn map_builtins(s: &str) -> Option<BuiltinFunction> {
    match s {
        "first" => Some(BuiltinFunction {
            func: builtin_first,
        }),
        "last" => Some(BuiltinFunction { func: builtin_last }),
        "len" => Some(BuiltinFunction { func: builtin_len }),
        "push" => Some(BuiltinFunction { func: builtin_push }),
        "tail" => Some(BuiltinFunction { func: builtin_tail }),
        "to_string" => Some(BuiltinFunction {
            func: builtin_to_string,
        }),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::builtin_len;
    use super::Object;
    use crate::object::EvaluationError;
    use crate::object::QuickReturn;

    #[test]
    fn test_len() {
        let no_arguments = builtin_len(vec![]);
        assert_eq!(
            no_arguments,
            Err(QuickReturn::Error(EvaluationError::BuiltinFunctionError(
                "unexpected number of arguments. Expected 1 got 0".into()
            )))
        );

        let too_many_arguments = builtin_len(vec![
            Object::string("hello".to_owned()),
            Object::string("world".to_owned()),
        ]);
        assert_eq!(
            too_many_arguments,
            Err(QuickReturn::Error(EvaluationError::BuiltinFunctionError(
                "unexpected number of arguments. Expected 1 got 2".into()
            )))
        );

        let empty_array = builtin_len(vec![Object::array(vec![])]);
        assert_eq!(empty_array, Ok(Object::integer(0)));

        let two_elements = builtin_len(vec![Object::array(vec![
            Object::string("hello".to_owned()),
            Object::string("world".to_owned()),
        ])]);
        assert_eq!(two_elements, Ok(Object::integer(2)));

        let string_len = builtin_len(vec![Object::string("hello".to_owned())]);
        assert_eq!(string_len, Ok(Object::integer(5)));

        let integer_len = builtin_len(vec![Object::integer(42)]);
        assert_eq!(
            integer_len,
            Err(QuickReturn::Error(EvaluationError::BuiltinFunctionError(
                "unexpected argument type. Expected String got Integer(42)".into()
            )))
        );
    }
}

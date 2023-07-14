use crate::object::{BuiltinFunction, EvaluationError, Object, QuickReturn};
use gc::Gc;

fn unexpected_number_of_arguments_error(expected: usize, got: usize) -> QuickReturn {
    QuickReturn::Error(EvaluationError::BuiltinFunctionError(format!(
        "unexpected number of arguments. Expected {} got {}",
        expected, got
    )))
}

fn builtin_first(args: Vec<Gc<Object>>) -> Result<Gc<Object>, QuickReturn> {
    if args.len() != 1 {
        return Err(unexpected_number_of_arguments_error(1, args.len()));
    }
    match args[0].as_ref() {
        Object::Array(arr) => {
            if arr.len() == 0 {
                return Ok(Object::null());
            }
            Ok(arr[0].clone())
        }
        _ => Err(QuickReturn::Error(EvaluationError::BuiltinFunctionError(
            format!("unexpected argument type. Expected Array got {:?}", args[0]),
        ))),
    }
}

fn builtin_last(args: Vec<Gc<Object>>) -> Result<Gc<Object>, QuickReturn> {
    if args.len() != 1 {
        return Err(unexpected_number_of_arguments_error(1, args.len()));
    }
    match args[0].as_ref() {
        Object::Array(arr) => {
            if arr.len() == 0 {
                return Ok(Object::null());
            }
            Ok(arr[arr.len() - 1].clone())
        }
        _ => Err(QuickReturn::Error(EvaluationError::BuiltinFunctionError(
            format!("unexpected argument type. Expected Array got {:?}", args[0]),
        ))),
    }
}

fn builtin_len(args: Vec<Gc<Object>>) -> Result<Gc<Object>, QuickReturn> {
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
            ),
        ))),
    }
}

fn builtin_push(args: Vec<Gc<Object>>) -> Result<Gc<Object>, QuickReturn> {
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
            format!("unexpected argument type. Expected Array got {:?}", args[0]),
        ))),
    }
}

fn builtin_tail(args: Vec<Gc<Object>>) -> Result<Gc<Object>, QuickReturn> {
    if args.len() != 1 {
        return Err(unexpected_number_of_arguments_error(1, args.len()));
    }
    match args[0].as_ref() {
        Object::Array(arr) => {
            if arr.len() == 0 {
                return Ok(Object::null());
            }
            Ok(Object::array(arr[1..].to_owned()))
        }
        _ => Err(QuickReturn::Error(EvaluationError::BuiltinFunctionError(
            format!("unexpected argument type. Expected Array got {:?}", args[0]),
        ))),
    }
}

pub(crate) fn map_builtins(s: &str) -> Option<BuiltinFunction> {
    match s {
        "first" => Some(BuiltinFunction {
            func: builtin_first,
        }),
        "last" => Some(BuiltinFunction { func: builtin_last }),
        "len" => Some(BuiltinFunction { func: builtin_len }),
        "push" => Some(BuiltinFunction { func: builtin_push }),
        "tail" => Some(BuiltinFunction { func: builtin_tail }),
        _ => None,
    }
}

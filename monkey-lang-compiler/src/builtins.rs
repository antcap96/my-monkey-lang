use crate::{object::Object, vm::VmError};

fn argument_count_error(expected: usize, got: usize) -> VmError {
    VmError::BuiltinFunctionError(
        format!(
            "unexpected number of arguments. Expected {} got {}",
            expected, got
        )
        .into(),
    )
}

fn builtin_first(args: &[Object]) -> Result<Object, VmError> {
    match args {
        [Object::Array(arr)] => Ok(arr.first().cloned().unwrap_or(Object::Null)),
        [obj] => Err(VmError::BuiltinFunctionError(
            format!("unexpected argument type. Expected Array got {:?}", obj).into(),
        )),
        _ => Err(argument_count_error(1, args.len())),
    }
}

fn builtin_last(args: &[Object]) -> Result<Object, VmError> {
    match args {
        [Object::Array(arr)] => Ok(arr.last().cloned().unwrap_or(Object::Null)),
        [obj] => Err(VmError::BuiltinFunctionError(
            format!("unexpected argument type. Expected Array got {:?}", obj).into(),
        )),
        _ => Err(argument_count_error(1, args.len())),
    }
}

fn builtin_len(args: &[Object]) -> Result<Object, VmError> {
    match args {
        [Object::String(s)] => Ok(Object::Integer(s.len() as i64)),
        [Object::Array(arr)] => Ok(Object::Integer(arr.len() as i64)),
        [Object::Hash(hash)] => Ok(Object::Integer(hash.len() as i64)),
        [obj] => Err(VmError::BuiltinFunctionError(
            format!(
                "unexpected argument type. Expected String, Array or Hash got {:?}",
                obj
            )
            .into(),
        )),
        _ => Err(argument_count_error(1, args.len())),
    }
}

fn builtin_push(args: &[Object]) -> Result<Object, VmError> {
    match args {
        [Object::Array(arr), obj] => {
            let mut new_arr = arr.clone();
            new_arr.push(obj.clone());
            Ok(Object::Array(new_arr))
        }
        [_, _] => Err(VmError::BuiltinFunctionError(
            "unexpected argument type. Expected Array got something else".into(),
        )),
        _ => Err(argument_count_error(2, args.len())),
    }
}

fn builtin_tail(args: &[Object]) -> Result<Object, VmError> {
    match args {
        [Object::Array(arr)] if arr.is_empty() => Ok(Object::Array(vec![])),
        [Object::Array(arr)] => Ok(Object::Array(arr[1..].to_owned())),
        [obj] => Err(VmError::BuiltinFunctionError(
            format!("unexpected argument type. Expected Array got {:?}", obj).into(),
        )),
        _ => Err(argument_count_error(1, args.len())),
    }
}

fn builtin_to_string(args: &[Object]) -> Result<Object, VmError> {
    if args.len() != 1 {
        return Err(argument_count_error(1, args.len()));
    }
    Ok(Object::String(to_string(&args[0])))
}

fn to_string(obj: &Object) -> String {
    match obj {
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
                s.push_str(&to_string(obj));
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
                s.push_str(&format!("{}: {}", to_string(&key), to_string(&value)));
            }
            s.push('}');
            s
        }
        Object::CompiledFunction(func) => format!("{:?}", func),
        Object::Builtin(idx) => format!("<builtin {}>", BUILTINS[*idx as usize].0),
        // Object::BuiltinFunction(_) => {
        //     format!("<builtin-fn@{:x}>", obj.as_ref() as *const Object as usize)
        // }
    }
}

pub(crate) const BUILTINS: &[(&str, fn(&[Object]) -> Result<Object, VmError>)] = &[
    ("len", builtin_len),
    ("to_string", builtin_to_string),
    ("first", builtin_first),
    ("last", builtin_last),
    ("tail", builtin_tail),
    ("push", builtin_push),
];

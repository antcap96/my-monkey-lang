use crate::object::{EvaluationError, Object, ObjectCore, QuickReturn, BuiltinFunction};

fn builtin_len(args: Vec<Object>) -> Result<Object, QuickReturn> {
    if args.len() != 1 {
        return Err(QuickReturn::Error(EvaluationError::BuiltinFunctionError(
            format!(
                "unexpected number of arguments. Expected 1 got {}",
                args.len()
            ),
        )));
    }
    match args[0].core_ref() {
        ObjectCore::String(s) => Ok(Object::integer(s.len() as i64)),
        _ => Err(QuickReturn::Error(EvaluationError::BuiltinFunctionError(
            format!(
                "unexpected argument type. Expected String got {:?}",
                args[0]
            ),
        ))),
    }
}

pub(crate) fn map_builtins(s: &str) -> Option<BuiltinFunction> {
    match s {
        "len" => Some(BuiltinFunction { func: builtin_len }),
        _ => None,
    }
}

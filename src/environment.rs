use gc::{Finalize, Gc, GcCell, Trace};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Trace, Finalize)]
pub struct EnvironmentCore {
    store: HashMap<String, crate::object::Object>,
    outer: Option<Environment>,
}

#[derive(Debug, PartialEq, Clone, Trace, Finalize)]
pub struct Environment {
    environment: Gc<GcCell<EnvironmentCore>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            environment: Gc::new(GcCell::new(EnvironmentCore {
                store: HashMap::new(),
                outer: None,
            })),
        }
    }

    pub fn new_enclosed(outer: Environment) -> Environment {
        Environment {
            environment: Gc::new(GcCell::new(EnvironmentCore {
                store: HashMap::new(),
                outer: Some(outer),
            })),
        }
    }
    //                                     Result<crate::object::Object, crate::object::QuickReturn>
    pub fn get(&self, key: &str) -> Option<crate::object::Object> {
        let env = self.environment.borrow();
        env.store
            .get(key)
            .cloned()
            .or(env.outer.as_ref().and_then(|outer| outer.get(key)))
            .or(crate::builtins::map_builtins(key)
                .map(crate::object::Object::builtin_function))
    }

    pub fn set(&mut self, key: String, value: crate::object::Object) {
        self.environment.borrow_mut().store.insert(key, value);
    }
}

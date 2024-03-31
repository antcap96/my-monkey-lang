use crate::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(PartialEq, Clone)]
pub struct EnvironmentCore {
    store: HashMap<Rc<str>, Rc<Object>>,
    outer: Option<Environment>,
}

impl std::fmt::Debug for EnvironmentCore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set().entries(self.store.keys()).finish()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    pub environment: Rc<RefCell<EnvironmentCore>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            environment: Rc::new(RefCell::new(EnvironmentCore {
                store: HashMap::new(),
                outer: None,
            })),
        }
    }

    pub fn new_enclosed(outer: Environment) -> Environment {
        Environment {
            environment: Rc::new(RefCell::new(EnvironmentCore {
                store: HashMap::new(),
                outer: Some(outer),
            })),
        }
    }
    //                              Result<crate::object::Object, crate::object::QuickReturn>
    pub fn get(&self, key: &str) -> Option<Rc<Object>> {
        let env = self.environment.borrow();
        env.store
            .get(key)
            .cloned()
            .or(env.outer.as_ref().and_then(|outer| outer.get(key)))
            .or(crate::builtins::map_builtins(key).map(crate::object::Object::builtin_function))
    }

    pub fn set(&mut self, key: Rc<str>, value: Rc<Object>) {
        self.environment.borrow_mut().store.insert(key, value);
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

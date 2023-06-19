use std::collections::HashMap;
use gc::{Finalize, Trace, GcCell, Gc};


#[derive(Debug, PartialEq, Clone, Trace, Finalize)]
pub struct Environment {
    store: HashMap<String, crate::object::Object>,
    outer: Option<Gc<GcCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Gc<GcCell<Self>> {
        Gc::new(GcCell::new(Environment {
            store: HashMap::new(),
            outer: None,
        }))
    }

    pub fn new_enclosed(outer: Gc<GcCell<Self>>) -> Gc<GcCell<Self>> {
        Gc::new(GcCell::new(Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }))
    }
    //                                     Result<crate::object::Object, crate::object::QuickReturn>
    pub fn get(&self, key: &str) -> Option<crate::object::Object> {
        self.store.get(key).cloned().or(self
            .outer
            .as_ref()
            .and_then(|outer| outer.borrow().get(key)))
    }

    pub fn set(&mut self, key: &str, value: crate::object::Object) {
        self.store.insert(key.to_string(), value);
    }
}

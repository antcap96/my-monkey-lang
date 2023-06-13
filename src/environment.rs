use std::collections::HashMap;

pub struct Environment {
    store: HashMap<String, crate::object::Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }
    //                                 Result<crate::object::Object, crate::object::QuickReturn>
    pub fn get(&self, key: &str) -> Option<crate::object::Object> {
        self.store.get(key).cloned() // TODO: avoid cloning with reference counting?
    }

    pub fn set(&mut self, key: &str, value: crate::object::Object) {
        self.store.insert(key.to_string(), value);
    }
}

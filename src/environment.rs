use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, crate::object::Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            outer: None,
        }))
    }

    pub fn new_enclosed(outer: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
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

use std::{collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub enum Scope {
    Global,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    pub name: Rc<str>,
    pub scope: Scope,
    pub index: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SymbolTable {
    store: HashMap<Rc<str>, Symbol>,
    pub num_definitions: usize,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: Rc<str>) -> &Symbol {
        let symbol = Symbol {
            name: name.clone(),
            scope: Scope::Global,
            index: self.num_definitions,
        };
        self.store.insert(name.clone(), symbol);
        self.num_definitions += 1;
        &self.store.get(&name).unwrap()
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store.get(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define() {
        let a: Rc<str> = Rc::from("a");
        let b: Rc<str> = Rc::from("b");
        let expected = SymbolTable {
            store: HashMap::from([
                (
                    a.clone(),
                    Symbol {
                        name: a.clone(),
                        scope: Scope::Global,
                        index: 0,
                    },
                ),
                (
                    b.clone(),
                    Symbol {
                        name: b.clone(),
                        scope: Scope::Global,
                        index: 1,
                    },
                ),
            ]),
            num_definitions: 2,
        };
        let mut global = SymbolTable::new();
        global.define(a.clone());
        global.define(b.clone());
        assert_eq!(global, expected);
    }

    #[test]
    fn resolve_global() {
        let a: Rc<str> = Rc::from("a");
        let b: Rc<str> = Rc::from("b");
        let mut global = SymbolTable::new();
        global.define(a.clone());
        global.define(b.clone());
        assert_eq!(
            global.resolve(&a),
            Some(&Symbol {
                name: a.clone(),
                scope: Scope::Global,
                index: 0,
            })
        );
        assert_eq!(
            global.resolve(&b),
            Some(&Symbol {
                name: b.clone(),
                scope: Scope::Global,
                index: 1,
            })
        );
    }
}

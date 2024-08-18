use std::{collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub enum Scope {
    Global,
    Local,
    Builtin,
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
    pub num_definitions: usize, // TODO: could this be replaced with store.len()?
    pub outer: Option<Box<SymbolTable>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
            outer: None,
        }
    }

    pub fn new_enclosed(symbol_table: SymbolTable) -> SymbolTable {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
            outer: Some(Box::new(symbol_table)),
        }
    }

    pub fn define(&mut self, name: Rc<str>) -> &Symbol {
        let symbol = match self.outer {
            Some(_) => Symbol {
                name: name.clone(),
                scope: Scope::Local,
                index: self.num_definitions,
            },
            None => Symbol {
                name: name.clone(),
                scope: Scope::Global,
                index: self.num_definitions,
            },
        };
        self.store.insert(name.clone(), symbol);
        self.num_definitions += 1;
        self.store.get(&name).unwrap()
    }

    pub fn define_builtin(&mut self, name: Rc<str>, index: usize) -> &Symbol {
        let symbol = Symbol {
            name: name.clone(),
            scope: Scope::Builtin,
            index,
        };
        self.store.insert(name.clone(), symbol);
        self.store.get(&name).unwrap()
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store.get(name).or_else(|| {
            self.outer
                .as_ref()
                .and_then(|store| store.as_ref().resolve(name))
        })
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
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
            outer: None,
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

    #[test]
    fn test_resolve_local() {
        let a: Rc<str> = Rc::from("a");
        let b: Rc<str> = Rc::from("b");
        let c: Rc<str> = Rc::from("c");
        let d: Rc<str> = Rc::from("d");

        let mut global = SymbolTable::new();
        global.define(a.clone());
        global.define(b.clone());

        let mut local = SymbolTable::new_enclosed(global);
        local.define(c.clone());
        local.define(d.clone());

        assert_eq!(
            local.resolve(&a),
            Some(&Symbol {
                name: a.clone(),
                scope: Scope::Global,
                index: 0,
            })
        );
        assert_eq!(
            local.resolve(&b),
            Some(&Symbol {
                name: b.clone(),
                scope: Scope::Global,
                index: 1,
            })
        );
        assert_eq!(
            local.resolve(&c),
            Some(&Symbol {
                name: c.clone(),
                scope: Scope::Local,
                index: 0,
            })
        );
        assert_eq!(
            local.resolve(&d),
            Some(&Symbol {
                name: d.clone(),
                scope: Scope::Local,
                index: 1,
            })
        );
    }
}

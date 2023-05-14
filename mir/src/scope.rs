use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{ItemId, TypeId, UnitId, ValueId};

#[derive(Debug, Clone)]
pub struct ScopeStack<'a> {
    scopes: Vec<Rc<RefCell<Scope>>>,
    current: Rc<RefCell<Scope>>,
    marker: std::marker::PhantomData<&'a ()>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeItem {
    Function {
        unit: UnitId,
        id: ItemId,
        ty: TypeId,
    },
    Type {
        name: String,
        id: TypeId,
    },
    Variable {
        name: String,
        ty: TypeId,
        value: ValueId,
        initialized: bool,
    },
}

#[derive(Debug)]
pub struct Scope {
    items: HashMap<String, ScopeItem>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            items: HashMap::new(),
        }
    }
}

impl<'a> ScopeStack<'a> {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            current: Rc::new(RefCell::new(Scope::new())),
            marker: std::marker::PhantomData,
        }
    }

    pub fn insert(&mut self, name: String, item: ScopeItem) {
        self.current.borrow_mut().items.insert(name, item);
    }

    pub fn get(&'a self, name: &str) -> Option<ScopeItem> {
        if let Some(curr) = self.current.borrow_mut().items.get(name) {
            Some(curr.clone())
        } else {
            for scope in self.scopes.iter().rev() {
                if let Some(item) = scope.borrow().items.get(name) {
                    return Some(item.clone());
                }
            }
            None
        }
    }

    pub fn make_child(&'a self) -> Self {
        let mut new_scopes = self.scopes.clone();
        new_scopes.push(self.current.clone());
        let new = Self {
            scopes: new_scopes,
            current: Rc::new(RefCell::new(Scope::new())),
            marker: std::marker::PhantomData,
        };
        new
    }
}

#[cfg(test)]
pub mod tests {
    #[test]
    fn test_stack() {
        let mut stack = super::ScopeStack::new();
        stack.insert(
            "test".to_owned(),
            crate::scope::ScopeItem::Type {
                name: "test_t".to_owned(),
                id: crate::TypeId::default(),
            },
        );
        println!("{:?}", stack);

        let mut child = stack.make_child();
        child.insert(
            "test2".to_owned(),
            crate::scope::ScopeItem::Type {
                name: "test2_t".to_owned(),
                id: crate::TypeId::default(),
            },
        );
        println!("{:?}", child);

        let child2 = child.make_child();
        println!("{:?}", child2);
        assert_eq!(
            child2.get("test").unwrap(),
            crate::scope::ScopeItem::Type {
                name: "test_t".to_owned(),
                id: crate::TypeId::default(),
            }
        );
    }
}

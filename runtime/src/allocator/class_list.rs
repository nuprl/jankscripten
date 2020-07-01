use crate::string::StrPtr;
use std::collections::HashMap;

pub struct ClassList {
    classes: HashMap<u16, ClassDef>,
    next_class_type: u16,
}

impl ClassList {
    pub fn new() -> Self {
        Self {
            classes: HashMap::new(),
            next_class_type: 0,
        }
    }
    pub fn new_container_type(&mut self, class: ClassDef) -> u16 {
        let type_tag = self.next_class_type;
        self.next_class_type += 1;
        self.classes.insert(type_tag, class);
        return type_tag;
    }
    pub fn get_container_size(&self, container_type: &u16) -> usize {
        self.classes[container_type].size
    }
    pub fn transition(&mut self, class_tag: u16, name: StrPtr) -> u16 {
        // create a new class
        let class = self.classes.get_mut(&class_tag).unwrap();
        let new_class = class.branch(name, self.next_class_type);
        self.new_container_type(new_class)
    }
}

#[derive(Clone, Debug)]
pub struct ClassDef {
    size: usize,
    offsets: Vec<(StrPtr, usize)>,
    transitions: Vec<(StrPtr, u16)>,
}
impl ClassDef {
    /// this is the very base class
    pub fn new() -> Self {
        Self {
            size: 0,
            offsets: Vec::new(),
            transitions: Vec::new(),
        }
    }
    fn branch(&mut self, name: StrPtr, new_tag: u16) -> Self {
        self.transitions.push((name, new_tag));
        let mut offsets = self.offsets.clone();
        offsets.push((name, self.size));
        Self {
            size: self.size + 1,
            offsets,
            transitions: Vec::new(),
        }
    }
}

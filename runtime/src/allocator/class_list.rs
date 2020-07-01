//! The global classes with their offsets, transitions, and sizes
//!
//! Some terminology:
//! - Class => A set of offsets and transitions
//! - ClassList => The singe global list of classes, associated with a heap
//! - Object => An instance of a class, allocated on the heap with space
//!   for every field but they may not be occupied

use crate::string::StrPtr;

pub struct ClassList {
    /// a HashMap to look up our class is obviously a non-starter when
    /// classes are meant to optimize HashMap lookup
    /// next class type is simply classes.len
    classes: Vec<Class>,
}

impl ClassList {
    pub fn new() -> Self {
        Self {
            // initialize with empty object
            classes: vec![Class::new()],
        }
    }
    pub fn new_class_type(&mut self, class: Class) -> u16 {
        let type_tag = self.classes.len() as u16;
        self.classes.push(class);
        type_tag
    }
    pub fn get_class_size(&self, container_type: u16) -> usize {
        self.get_class(container_type).size
    }
    pub fn get_class(&self, class_tag: u16) -> &Class {
        &self.classes.get(class_tag as usize).unwrap()
    }
    /// look up transitions, if none is relevant make one, and return new
    /// class tag
    pub fn transition(&mut self, class_tag: u16, name: StrPtr) -> u16 {
        let new_tag = self.classes.len() as u16;
        let class = &mut self.classes[class_tag as usize];
        match class.lookup_transition(name) {
            Some(tag) => tag,
            None => {
                let new_class = class.branch(name, new_tag);
                self.new_class_type(new_class)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Class {
    pub size: usize,
    offsets: Vec<(StrPtr, usize)>,
    transitions: Vec<(StrPtr, u16)>,
}
impl Class {
    /// this is the very base class
    #[allow(unused)]
    pub fn new() -> Self {
        Self {
            size: 0,
            offsets: Vec::new(),
            transitions: Vec::new(),
        }
    }
    pub fn lookup(&self, name: StrPtr) -> Option<usize> {
        self.offsets
            .iter()
            .find(|(offset_name, _)| offset_name == &name)
            .map(|(_, index)| *index)
    }
    fn lookup_transition(&self, name: StrPtr) -> Option<u16> {
        self.transitions
            .iter()
            // TODO: might need to match types
            .find(|(trans_name, _)| trans_name == &name)
            .map(|(_, index)| *index)
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

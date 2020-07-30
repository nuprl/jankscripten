//! A data structure that supports *O(1)* append operations. See
//! [https://cs.brown.edu/courses/cs019/2009/assignments/join_lists] for more
//! information.

#[derive(Debug)]
pub enum Rope<T> {
    Nil,
    Singleton(T),
    /// Instead of using `Rope::Append` directly, use the `.append` smart
    /// constructor.
    Append(Box<Rope<T>>, Box<Rope<T>>),
}

/// An iterator that consumes a `Rope` and produces its values in order.
pub struct RopeIntoIter<T> {
    stack: Vec<Rope<T>>,
}

impl<T> Default for Rope<T> {
    fn default() -> Rope<T> {
        Rope::Nil
    }
}

impl<T> Rope<T> {
    pub fn new() -> Self {
        Rope::default()
    }

    pub fn singleton(item: T) -> Self {
        Rope::Singleton(item)
    }

    pub fn append(self, other: Rope<T>) -> Self {
        match (&self, &other) {
            (Rope::Nil, _) => other,
            (_, Rope::Nil) => self,
            _ => Rope::Append(Box::new(self), Box::new(other)),
        }
    }
}

impl<T> Iterator for RopeIntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        // Loop instead of recursion to avoid stack overflows.
        loop {
            match self.stack.pop() {
                None => {
                    return None;
                }
                Some(Rope::Nil) => {
                    // this can happen if Rope::Append is used directly.
                }
                Some(Rope::Singleton(item)) => {
                    return Some(item);
                }
                Some(Rope::Append(lhs, rhs)) => {
                    self.stack.push(*rhs);
                    self.stack.push(*lhs);
                }
            }
        }
    }
}

use std::iter::IntoIterator;

impl<T> IntoIterator for Rope<T> {
    type Item = T;
    type IntoIter = RopeIntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        RopeIntoIter { stack: vec![self] }
    }
}
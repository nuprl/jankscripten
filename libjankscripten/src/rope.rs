//! A data structure that supports *O(1)* append operations. See
//! [https://cs.brown.edu/courses/cs019/2009/assignments/join_lists] for more
//! information.

#[derive(Default, Debug)]
pub struct Rope<T> {
    rope: R<T>,
}

#[derive(Debug)]
enum R<T> {
    Nil,
    Singleton(T),
    /// Instead of using `R::Append` directly, use the `.append` smart
    /// constructor.
    Append(Box<R<T>>, Box<R<T>>),
}

/// An iterator that consumes a `Rope` and produces its values in order.
pub struct RopeIntoIter<T> {
    stack: Vec<R<T>>,
}

impl<T> Default for R<T> {
    fn default() -> Self {
        R::Nil
    }
}

impl<T> Rope<T> {
    pub fn new() -> Self {
        Self::nil()
    }

    pub fn nil() -> Self {
        Rope { rope: R::Nil }
    }

    pub fn singleton(item: T) -> Self {
        Rope {
            rope: R::Singleton(item),
        }
    }

    pub fn append(self, other: Rope<T>) -> Self {
        match (&self.rope, &other.rope) {
            (R::Nil, _) => other,
            (_, R::Nil) => self,
            _ => Rope {
                rope: R::Append(Box::new(self.rope), Box::new(other.rope)),
            },
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
                Some(R::Nil) => {
                    // This indicates a bug in the smart constructors.
                    panic!("unexpected R::Nil in interior of Rope");
                }
                Some(R::Singleton(item)) => {
                    return Some(item);
                }
                Some(R::Append(lhs, rhs)) => {
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
        RopeIntoIter {
            stack: match self.rope {
                R::Nil => Vec::new(),
                r => vec![r],
            },
        }
    }
}

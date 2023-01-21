use std::{fmt, rc::Rc};

#[cfg(test)]
mod test {
    use super::*;
    use crate::list;

    #[test]
    fn list() {
        let list = List::default();
        assert_eq!(list.head(), None);

        let list = list.cons(1).cons(2).cons(3);
        assert_eq!(list.head(), Some(&3));

        let list = list.tail();
        assert_eq!(list.head(), Some(&2));

        let list = list.tail();
        assert_eq!(list.head(), Some(&1));

        let list = list.tail();
        assert_eq!(list.head(), None);
    }

    #[test]
    fn list_macro() {
        let list = list![1, 2, 3];
        assert_eq!(vec![1, 2, 3], list.iter().copied().collect::<Vec<_>>());
    }
}

#[macro_export]
macro_rules! list {
    ($($x:expr),*$(,)?) => {{
        let mut list = $crate::list::List::default();
	for x in [$($x),*].into_iter().rev() {
	    list = list.cons(x);
	}
	list
    }};
}

pub struct List<T> {
    content: Link<T>,
    size: usize,
}

impl<T: fmt::Debug> fmt::Debug for List<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<T> Default for List<T> {
    fn default() -> Self {
        Self {
            content: None,
            size: 0,
        }
    }
}

type Link<T> = Option<Rc<Node<T>>>;

struct Node<T> {
    head: T,
    tail: Link<T>,
}

impl<T> List<T> {
    pub fn cons(&self, head: T) -> Self {
        Self {
            content: Some(Rc::new(Node {
                head,
                tail: self.content.clone(),
            })),
            size: self.size + 1,
        }
    }

    #[allow(unused)]
    pub fn tail(&self) -> Self {
        List {
            content: self.content.as_ref().and_then(|node| node.tail.clone()),
            size: self.size.saturating_sub(1),
        }
    }

    #[allow(unused)]
    pub fn head(&self) -> Option<&T> {
        self.content.as_ref().map(|node| &node.head)
    }

    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            next: self.content.as_deref(),
        }
    }
}

impl<T> Drop for List<T> {
    fn drop(&mut self) {
        let mut content = self.content.take();
        while let Some(node) = content {
            if let Ok(mut node) = Rc::try_unwrap(node) {
                content = node.tail.take();
            } else {
                break;
            }
        }
    }
}

pub struct Iter<'a, T> {
    next: Option<&'a Node<T>>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|node| {
            self.next = node.tail.as_deref();
            &node.head
        })
    }
}

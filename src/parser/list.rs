use std::{rc::Rc, fmt};

#[cfg(test)]
mod test {
    use super::*;

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
}

pub struct List<T> {
    content: Link<T>,
}

impl<T: fmt::Debug> fmt::Debug for List<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	f.debug_list().entries(self.iter()).finish()
    }
}

impl<T> Default for List<T> {
    fn default() -> Self {
        Self { content: None }
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
        }
    }

    #[allow(unused)]
    pub fn tail(&self) -> Self {
        List {
            content: self.content.as_ref().and_then(|node| node.tail.clone()),
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

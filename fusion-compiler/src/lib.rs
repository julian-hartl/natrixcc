#[macro_export]
macro_rules! idx {

    ($name:ident) => {
        #[derive(Debug, Clone, Eq, PartialEq, Hash, Copy)]
        pub struct $name {
            index: usize,
        }

        impl Idx for $name {
            fn as_index(&self) -> usize {
                self.index
            }

            fn new(index: usize) -> Self {
                Self {
                    index,
                }
            }
        }
    };
}

pub trait Idx {
    fn as_index(&self) -> usize;
    fn new(index: usize) -> Self;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IdxVec<Index, T> where Index: Idx {
    vec: Vec<T>,
    _marker: std::marker::PhantomData<Index>,
}

impl<Index, T> IdxVec<Index, T> where Index: Idx {
    pub fn new() -> Self {
        Self {
            vec: vec![],
            _marker: std::marker::PhantomData,
        }
    }

    pub fn push(&mut self, value: T) -> Index {
        let next_index = self.vec.len();
        self.vec.push(value);
        return Index::new(next_index);
    }

    pub fn iter(&self) -> impl Iterator<Item=&T> {
        self.vec.iter()
    }

    pub fn indexed_iter(&self) -> impl Iterator<Item=(Index, &T)> {
        self.vec.iter().enumerate().map(|(index, value)| (Index::new(index), value))
    }

    pub fn cloned_indices(&self) -> Vec<Index> {
        self.vec.iter().enumerate().map(|(index, _)| Index::new(index)).collect()
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    pub fn get(&self, index: Index) -> &T {
        return &self[index];
    }
}

impl<Index, T> std::ops::Index<Index> for IdxVec<Index, T> where Index: Idx {
    type Output = T;

    fn index(&self, index: Index) -> &T {
        return &self.vec[index.as_index()];
    }
}

impl<Index, T> std::ops::IndexMut<Index> for IdxVec<Index, T> where Index: Idx {
    fn index_mut(&mut self, index: Index) -> &mut T {
        return &mut self.vec[index.as_index()];
    }
}

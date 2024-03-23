#[macro_export]
macro_rules! idx {
    ($name:ident) => {
        #[derive(Debug, Clone, Eq, PartialEq, Hash, Copy, Ord, PartialOrd)]
        pub struct $name(usize);

        impl Idx for $name {
            fn as_index(&self) -> usize {
                self.0
            }

            fn new(index: usize) -> Self {
                Self(index)
            }

        }
    };
}

pub trait Idx: Copy + Clone + Sized{
    fn as_index(&self) -> usize;
    fn new(index: usize) -> Self;

    fn unreachable() -> Self {
        Self::new(usize::MAX)
    }

    fn first() -> Self {
        Self::new(0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct IdxVec<Index, T>
    where
        Index: Idx,
{
    vec: Vec<T>,
    _marker: std::marker::PhantomData<Index>,
}

impl<Index: Idx, T> IdxVec<Index, T>
{
    pub fn new() -> Self {
        Self {
            vec: vec![],
            _marker: std::marker::PhantomData,
        }
    }

    pub fn push(&mut self, value: T) -> Index {
        let next_index = self.vec.len();
        self.vec.push(value);
        Index::new(next_index)
    }

    pub fn push_with_index(&mut self, value: impl FnOnce(Index) -> T) -> Index {
        let next_index = Index::new(self.vec.len());
        self.vec.push(value(next_index));
        next_index
    }


    pub fn iter(&self) -> impl Iterator<Item=&T> {
        self.vec.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item=&mut T> {
        self.vec.iter_mut()
    }

    pub fn indexed_iter(&self) -> impl Iterator<Item=(Index, &T)> {
        self.vec
            .iter()
            .enumerate()
            .map(|(index, value)| (Index::new(index), value))
    }

    pub fn indexed_iter_mut(&mut self) -> impl Iterator<Item=(Index, &mut T)> {
        self.vec
            .iter_mut()
            .enumerate()
            .map(|(index, value)| (Index::new(index), value))
    }


    pub fn cloned_indices(&self) -> Vec<Index> {
        self.vec
            .iter()
            .enumerate()
            .map(|(index, _)| Index::new(index))
            .collect()
    }


    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    pub fn get(&self, index: Index) -> &T {
        &self[index]
    }

    pub fn get_mut(&mut self, index: Index) -> &mut T {
        &mut self[index]
    }

    pub fn indices(&self) -> impl Iterator<Item=Index> {
        (0..self.vec.len()).map(|index| Index::new(index))
    }
}

impl<I: Idx, T> IdxVec<I, Option<T>> {
    #[inline]
    pub fn remove(&mut self, index: I) -> Option<T> {
        self.vec[index.as_index()].take()
    }

    #[inline]
    pub fn indexed_iter_as_option(&self) -> impl Iterator<Item=Option<(I, &T)>> {
        self.vec
            .iter()
            .enumerate()
            .map(|(index, value)| value.as_ref().map(|value| (I::new(index), value)))
    }

    #[inline]
    pub fn get_or_panic(&self, index: I) -> &T {
        self.get(index).as_ref().unwrap_or_else(||  crate::bug!("Index {} does not exist", index.as_index()))
    }

    #[inline]
    pub fn get_mut_or_panic(&mut self, index: I) -> &mut T {
        self.get_mut(index).as_mut().unwrap_or_else(|| crate::bug!("Index {} does not exist", index.as_index()))
    }

}

impl<Index, T> std::ops::Index<Index> for IdxVec<Index, T>
    where
        Index: Idx,
{
    type Output = T;

    fn index(&self, index: Index) -> &T {
        &self.vec[index.as_index()]
    }
}

impl<Index, T> std::ops::IndexMut<Index> for IdxVec<Index, T>
    where
        Index: Idx,
{
    fn index_mut(&mut self, index: Index) -> &mut T {
        return &mut self.vec[index.as_index()];
    }
}

#[macro_export]
macro_rules! bug {
    ($( $arg:tt )*) => ({
        panic!("You have hit a bug in the compiler. Please report this to the developers. {}", format_args!($($arg)*));
    });
}

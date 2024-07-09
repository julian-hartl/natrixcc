use fxhash::FxBuildHasher;
pub use indexmap::*;

pub type FxIndexMap<K, V> = IndexMap<K, V, FxBuildHasher>;
pub type FxIndexSet<T> = IndexSet<T, FxBuildHasher>;

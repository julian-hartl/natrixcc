use std::fmt::Debug;

#[derive(Clone, Debug)]
pub enum Element<V: Value> {
    Bottom,
    Top,
    Value(V),
}

impl<V: Value> Default for Element<V> {
    fn default() -> Self {
        Self::Bottom
    }
}

impl<V: Value> Element<V> {
    pub fn join(&mut self, other: Self) -> bool {
        match (self.clone(), other) {
            (_, Self::Bottom) | (Self::Top, _) => {
                return false;
            }
            (Self::Bottom, other) =>
                *self = other,
            (_, Self::Top) => {
                *self = Self::Top;
            }
            (Self::Value(mut new_value), Self::Value(other_value)) => {
                if new_value.join(other_value) {
                    *self = Self::Value(new_value);
                } else {
                    return false;
                }
            }
        };
        true
    }
}

pub trait Value: Sized + Clone + Debug {
    fn join(&mut self, other: Self) -> bool;
}

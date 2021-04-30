use std::marker::PhantomData;

use super::{
  direction::Direction,
  edge::EdgeType,
  index::{DefaultIdx, EdgeIndex, Index, NodeIndex},
  node::Node,
};

/// An iterator over either the `Node`s without `Edge`s to them or from them.
pub struct Externals<'a, N: 'a, T, Idx: Index = DefaultIdx> {
  /// Iterator over `Node`s.
  iter: std::iter::Enumerate<std::slice::Iter<'a, Node<N, Idx>>>,
  /// Direction of flow.
  direction: Direction,
  #[doc(hidden)]
  _t: PhantomData<T>,
}

impl<'a, N: 'a, T: EdgeType, Idx: Index> Externals<'a, N, T, Idx> {
  /// Create new `Externals`.
  pub fn new(
    iter: std::iter::Enumerate<std::slice::Iter<'a, Node<N, Idx>>>,
    direction: Direction,
  ) -> Self {
    Self {
      iter,
      direction,
      _t: PhantomData,
    }
  }
}

impl<'a, N: 'a, T: EdgeType, Idx: Index> Iterator for Externals<'a, N, T, Idx> {
  type Item = NodeIndex<Idx>;

  fn next(&mut self) -> Option<Self::Item> {
    let k = self.direction.index();

    loop {
      match self.iter.next() {
        Some((index, node)) => {
          if node.next[k] == EdgeIndex::end()
            && (T::is_directed() || node.next[1 - k] == EdgeIndex::end())
          {
            return Some(NodeIndex::new(index));
          } else {
            continue;
          }
        }
        None => return None,
      }
    }
  }
}

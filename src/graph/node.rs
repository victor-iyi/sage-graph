use crate::clone_fields;
use crate::graph::{
  direction::Direction,
  index::{DefaultIdx, EdgeIndex, Index},
};

/// Graph's Node type.
pub struct Node<N, Idx = DefaultIdx> {
  /// Associated node data.
  pub weight: N,

  /// Next edge in outgoing and incoming edge lists.
  next: [EdgeIndex<Idx>; 2],
}

impl<N, Idx> Clone for Node<N, Idx>
where
  N: Clone,
  Idx: Copy,
{
  clone_fields!(Node, weight, next);
}

impl<N, Idx: Index> Node<N, Idx> {
  /// Creates a new `Node`.
  pub fn new(weight: N, next: [EdgeIndex<Idx>; 2]) -> Self {
    Node { weight, next }
  }

  /// Returns the next edge index in a given direction.
  pub fn next_edge(&self, dir: Direction) -> EdgeIndex<Idx> {
    self.next[dir.index()]
  }
}

use crate::clone_fields;
use crate::graph::{
  direction::{Directed, Direction, Undirected},
  index::{DefaultIdx, EdgeIndex, Index, NodeIndex},
};

/// Graph' Edge.
pub struct Edge<E, Idx = DefaultIdx> {
  /// Associated edge data.
  pub weight: E,
  /// Next edge in outgoing and incoming edge lists.
  next: [EdgeIndex<Idx>; 2],
  /// Start and end node index.
  node: [NodeIndex<Idx>; 2],
}

impl<E, Idx> Clone for Edge<E, Idx>
where
  E: Clone,
  Idx: Copy,
{
  clone_fields!(Edge, weight, next, node);
}

impl<E, Idx: Index> Edge<E, Idx> {
  /// Returns the next edge index in a given direction.
  pub fn next_edge(&self, dir: Direction) -> EdgeIndex<Idx> {
    self.next[dir.index()]
  }

  /// Returns the source node index.
  pub fn source(&self) -> NodeIndex<Idx> {
    self.node[0]
  }

  /// Return the target node index.
  pub fn target(&self) -> NodeIndex<Idx> {
    self.node[1]
  }
}

pub trait EdgeDirection {
  fn is_directed() -> bool;
}

impl EdgeDirection for Directed {
  #[inline]
  fn is_directed() -> bool {
    true
  }
}

impl EdgeDirection for Undirected {
  #[inline]
  fn is_directed() -> bool {
    true
  }
}

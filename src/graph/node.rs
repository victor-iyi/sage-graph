use std::marker::PhantomData;

use super::{
  direction::Direction,
  index::{node_index, DefaultIdx, EdgeIndex, Index, NodeIndex},
};
use crate::clone_fields;

/// Graph's Node type.
pub struct Node<N, Idx = DefaultIdx> {
  /// Associated node data.
  pub weight: N,
  /// Next edge in outgoing and incoming edge lists.
  pub(crate) next: [EdgeIndex<Idx>; 2],
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
  pub fn next_edge(&self, direction: Direction) -> EdgeIndex<Idx> {
    self.next[direction.index()]
  }
}

/// Iterator yeilding mutable access to all node weights.
pub struct NodeWeightsMut<'a, N: 'a, Idx: Index = DefaultIdx> {
  nodes: std::slice::IterMut<'a, Node<N, Idx>>,
}

impl<'a, N: 'a, Idx: Index> NodeWeightsMut<'a, N, Idx> {
  pub fn new(nodes: std::slice::IterMut<'a, Node<N, Idx>>) -> Self {
    Self { nodes }
  }
}

impl<'a, N, Idx> Iterator for NodeWeightsMut<'a, N, Idx>
where
  Idx: Index,
{
  type Item = &'a mut N;

  fn next(&mut self) -> Option<&'a mut N> {
    self.nodes.next().map(|node| &mut node.weight)
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    self.nodes.size_hint()
  }
}

pub struct NodeIndices<Idx = DefaultIdx> {
  r: std::ops::Range<usize>,
  _t: PhantomData<fn() -> Idx>,
}

impl<Idx: Index> NodeIndices<Idx> {
  /// Create a new node indices
  pub fn new(r: std::ops::Range<usize>) -> Self {
    Self { r, _t: PhantomData }
  }
}

impl<Idx: Index> Iterator for NodeIndices<Idx> {
  type Item = NodeIndex<Idx>;

  fn next(&mut self) -> Option<Self::Item> {
    self.r.next().map(node_index)
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    self.r.size_hint()
  }
}

impl<Idx: Index> DoubleEndedIterator for NodeIndices<Idx> {
  fn next_back(&mut self) -> Option<Self::Item> {
    self.r.next_back().map(node_index)
  }
}

impl<Idx: Index> ExactSizeIterator for NodeIndices<Idx> {}

/// Iterator over all `Node`s of a `Graph`.
pub struct NodeIterator<'a, N: 'a, Idx: Index = DefaultIdx> {
  /// Inner iterator over `Node` & it's index (`NodeIndex`).
  iter: std::iter::Enumerate<std::slice::Iter<'a, Node<N, Idx>>>,
}

// impl<'a, N: 'a, Idx: Index> NodeIterator<'a, N, Idx> {
//   /// Create a new iterator over all `Node`s of a `Graph`.
//   pub fn new(
//     iter: std::iter::Enumerate<std::slice::Iter<'a, Node<N, Idx>>>,
//   ) -> Self {
//     Self { iter }
//   }
// }

impl<'a, N, Idx> Iterator for NodeIterator<'a, N, Idx>
where
  Idx: Index,
{
  type Item = (NodeIndex<Idx>, &'a N);

  fn next(&mut self) -> Option<Self::Item> {
    self
      .iter
      .next()
      .map(|(i, node)| (node_index(i), &node.weight))
  }
}

impl<'a, N, Idx: Index> DoubleEndedIterator for NodeIterator<'a, N, Idx> {
  fn next_back(&mut self) -> Option<Self::Item> {
    self
      .iter
      .next_back()
      .map(|(i, node)| (node_index(i), &node.weight))
  }
}

impl<'a, N, Idx: Index> ExactSizeIterator for NodeIterator<'a, N, Idx> {}

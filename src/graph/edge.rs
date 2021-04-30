#![allow(dead_code)]

use std::marker::PhantomData;

use super::{
  direction::{Directed, Direction, Undirected},
  index::{edge_index, DefaultIdx, EdgeIndex, Index, NodeIndex},
};
use crate::clone_fields;

/// Graph' Edge.
pub struct Edge<E, Idx = DefaultIdx> {
  /// Associated edge data.
  pub weight: E,
  /// Next edge in outgoing and incoming edge lists.
  pub(crate) next: [EdgeIndex<Idx>; 2],
  /// Start and end node index.
  pub(crate) node: [NodeIndex<Idx>; 2],
}

impl<E, Idx> Clone for Edge<E, Idx>
where
  E: Clone,
  Idx: Copy,
{
  clone_fields!(Edge, weight, next, node);
}

impl<E, Idx: Index> Edge<E, Idx> {
  /// Create a new `Edge`.
  pub fn new(
    weight: E,
    next: [EdgeIndex<Idx>; 2],
    node: [NodeIndex<Idx>; 2],
  ) -> Self {
    Edge { weight, next, node }
  }

  /// Returns the next edge index in a given direction.
  pub fn next_edge(&self, direction: Direction) -> EdgeIndex<Idx> {
    self.next[direction.index()]
  }

  /// Returns outgoing edge index.
  pub fn outgoing(&self) -> EdgeIndex<Idx> {
    self.next[0]
  }

  /// Returns incoming edge index.
  pub fn incoming(&self) -> EdgeIndex<Idx> {
    self.next[1]
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

/// EdgeType abstracts if a [`Graph`] is [`DiGraph`] or [`UnDiGraph`].
///
/// [`DiGraph`]: enum.DiGraph
/// [`UnDiGraph`]: enum.UnDiGraph
/// [`Graph`]: struct.Graph
pub trait EdgeType {
  fn is_directed() -> bool;
}

impl EdgeType for Directed {
  #[inline]
  fn is_directed() -> bool {
    true
  }
}

impl EdgeType for Undirected {
  #[inline]
  fn is_directed() -> bool {
    false
  }
}

/// Iterator over the edges of from or to a node.
pub struct Edges<'a, E: 'a, T, Idx: 'a = DefaultIdx>
where
  T: EdgeType,
  Idx: Index,
{
  /// Starting node to skip over.
  skip_start: NodeIndex<Idx>,
  edges: &'a [Edge<E, Idx>],

  /// Next edge to visit.
  next: [EdgeIndex<Idx>; 2],

  /// For directed graphs; the direction to iterate in.
  /// For undirected graphs; the direction of edges.
  direction: Direction,

  /// Type of graph: directed graph (`DiGraph`)
  /// or undirected graph: (`UnDiGraph`).
  _t: PhantomData<T>,
}

impl<'a, E, T: EdgeType, Idx: Index> Edges<'a, E, T, Idx> {
  /// Create a new iterator over edges.
  pub fn new(
    skip_start: NodeIndex<Idx>,
    edges: &'a [Edge<E, Idx>],
    next: [EdgeIndex<Idx>; 2],
    direction: Direction,
  ) -> Self {
    Self {
      skip_start,
      edges,
      next,
      direction,
      _t: PhantomData,
    }
  }
}

impl<'a, E, T: EdgeType, Idx: Index> Iterator for Edges<'a, E, T, Idx> {
  type Item = EdgeRef<'a, E, Idx>;

  fn next(&mut self) -> Option<Self::Item> {
    //   type         direction     |     iterate over    reverse
    //                              |
    //   Directed     Outgoing      |     Outgoing        No
    //   Directed     Incoming      |     Incoming        No
    //   Undirected   Outgoing      |     Both            Incoming
    //   Undirected   Incoming      |     Both            Outgoing
    //
    // For iterate_over, "both" is represented as None.
    // For revese, "no" is represented as None.
    let (iterate_over, reverse) = if T::is_directed() {
      (Some(self.direction), None)
    } else {
      (None, Some(self.direction.opposite()))
    };

    if iterate_over.unwrap_or(Direction::Outgoing) == Direction::Outgoing {
      let i = self.next[0].index();
      if let Some(Edge { node, weight, next }) = self.edges.get(i) {
        self.next[0] = next[0];
        return Some(EdgeRef {
          index: edge_index(i),
          node: if reverse == Some(Direction::Outgoing) {
            swap_pair(*node)
          } else {
            *node
          },
          weight,
        });
      }
    } // end if

    if iterate_over.unwrap_or(Direction::Incoming) == Direction::Incoming {
      while let Some(Edge { node, weight, next }) =
        self.edges.get(self.next[1].index())
      {
        let edge_idx = self.next[1];
        self.next[1] = next[1];
        // In any of the "both" situations, self-loops would be iteated over twice.
        // Skip them here..
        if iterate_over.is_none() && node[0] == self.skip_start {
          continue;
        }

        return Some(EdgeRef {
          index: edge_idx,
          node: if reverse == Some(Direction::Incoming) {
            swap_pair(*node)
          } else {
            *node
          },
          weight,
        });
      }
    }

    None
  }
}

impl<'a, E, T: EdgeType, Idx: Index> Clone for Edges<'a, E, T, Idx> {
  fn clone(&self) -> Self {
    Edges {
      skip_start: self.skip_start,
      edges: self.edges,
      next: self.next,
      direction: self.direction,
      _t: self._t,
    }
  }
}

/// Iterator over the multiple directed edges connecting a source node to a target node.
pub struct EdgesConnecting<'a, E: 'a, T, Idx: 'a = DefaultIdx>
where
  T: EdgeType,
  Idx: Index,
{
  /// Target node which (multiple) connection ends.
  target_node: NodeIndex<Idx>,
  /// List of edges connected to `EdgesConnecting::target_node`.
  edges: Edges<'a, E, T, Idx>,
  /// `EdgeType` directed or undirected.
  _t: PhantomData<T>,
}

impl<'a, E: 'a, T, Idx: 'a> EdgesConnecting<'a, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
  /// Create a new iterator over multiple directed edges connecting
  /// source node to target node.
  pub fn new(target_node: NodeIndex<Idx>, edges: Edges<'a, E, T, Idx>) -> Self {
    Self {
      target_node,
      edges,
      _t: PhantomData,
    }
  }
}

impl<'a, E, T, Idx> Iterator for EdgesConnecting<'a, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
  type Item = EdgeRef<'a, E, Idx>;

  fn next(&mut self) -> Option<EdgeRef<'a, E, Idx>> {
    while let Some(edge) = self.edges.next() {
      if edge.node[1] == self.target_node {
        return Some(edge);
      }
    }
    None
  }
}

/// Reference to a `Graph`'s edge.
#[derive(Debug)]
pub struct EdgeRef<'a, E: 'a, Idx = DefaultIdx> {
  /// Referencing `Edge`'s index.
  index: EdgeIndex<Idx>,
  /// Referencing `Edge`'s start and end `Node`.
  node: [NodeIndex<Idx>; 2],
  /// Reference to `Edge`'s associated data.
  weight: &'a E,
}

impl<'a, E, Idx: Index> EdgeRef<'a, E, Idx> {
  pub fn weight(&self) -> &'a E {
    self.weight
  }
}

impl<'a, E, Idx: Index> Clone for EdgeRef<'a, E, Idx> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<'a, E, Idx: Index> Copy for EdgeRef<'a, E, Idx> {}

impl<'a, E, Idx: Index> PartialEq for EdgeRef<'a, E, Idx>
where
  E: PartialEq,
{
  fn eq(&self, rhs: &Self) -> bool {
    self.index == rhs.index && self.weight == rhs.weight
  }
}

/// Iterator yielding mutable access to all edge weights.
pub struct EdgeWeightsMut<'a, E: 'a, Idx: Index = DefaultIdx> {
  edges: std::slice::IterMut<'a, Edge<E, Idx>>,
}

impl<'a, E, Idx: Index> EdgeWeightsMut<'a, E, Idx> {
  /// Create a new iterator yielding mutable edge weights.
  pub fn new(edges: std::slice::IterMut<'a, Edge<E, Idx>>) -> Self {
    Self { edges }
  }
}

impl<'a, E, Idx: Index> Iterator for EdgeWeightsMut<'a, E, Idx> {
  type Item = &'a mut E;

  fn next(&mut self) -> Option<Self::Item> {
    self.edges.next().map(|edge| &mut edge.weight)
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    self.edges.size_hint()
  }
}

/// Iterator over all `Edge`s of a `Graph`.
pub struct EdgeIterator<'a, E: 'a, Idx: Index = DefaultIdx> {
  /// Inner iterator over `Edge` & it's index (`EdgeIndex`).
  iter: std::iter::Enumerate<std::slice::Iter<'a, Edge<E, Idx>>>,
}

impl<'a, E: 'a, Idx: Index> EdgeIterator<'a, E, Idx> {
  /// Create a new iterator over all `Edge`s of a `Graph`.
  pub fn new(
    iter: std::iter::Enumerate<std::slice::Iter<'a, Edge<E, Idx>>>,
  ) -> Self {
    Self { iter }
  }
}

impl<'a, E, Idx: Index> Iterator for EdgeIterator<'a, E, Idx> {
  type Item = EdgeRef<'a, E, Idx>;

  fn next(&mut self) -> Option<Self::Item> {
    self.iter.next().map(|(i, edge)| EdgeRef {
      index: edge_index(i),
      node: edge.node,
      weight: &edge.weight,
    })
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    self.iter.size_hint()
  }
}

impl<'a, E, Idx: Index> DoubleEndedIterator for EdgeIterator<'a, E, Idx> {
  fn next_back(&mut self) -> Option<Self::Item> {
    self.iter.next_back().map(|(i, edge)| EdgeRef {
      index: edge_index(i),
      node: edge.node,
      weight: &edge.weight,
    })
  }
}

impl<'a, E, Idx> ExactSizeIterator for EdgeIterator<'a, E, Idx> where Idx: Index {}

/// Iterator over the edge indces of a graph.
#[derive(Clone, Debug)]
pub struct EdgeIndices<Idx = DefaultIdx> {
  r: std::ops::Range<usize>,
  _t: PhantomData<fn() -> Idx>,
}

impl<Idx> EdgeIndices<Idx> {
  pub fn new(range: std::ops::Range<usize>) -> Self {
    Self {
      r: range,
      _t: PhantomData,
    }
  }
}

impl<Idx: Index> Iterator for EdgeIndices<Idx> {
  type Item = EdgeIndex<Idx>;

  fn next(&mut self) -> Option<Self::Item> {
    self.r.next().map(edge_index)
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    self.r.size_hint()
  }
}

impl<Idx: Index> DoubleEndedIterator for EdgeIndices<Idx> {
  fn next_back(&mut self) -> Option<Self::Item> {
    self.r.next_back().map(edge_index)
  }
}

impl<Idx: Index> ExactSizeIterator for EdgeIndices<Idx> {}

/// Swap pair of slice `T`.
fn swap_pair<T>(mut x: [T; 2]) -> [T; 2] {
  x.swap(0, 1);
  x
}

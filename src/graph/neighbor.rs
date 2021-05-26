use super::{
  super::Graph,
  direction::Direction,
  edge::{Edge, EdgeType},
  index::{DefaultIdx, EdgeIndex, Index, NodeIndex},
};
use crate::clone_fields;

pub struct Neighbors<'a, E: 'a, Idx: 'a = DefaultIdx> {
  pub skip_start: NodeIndex<Idx>,
  pub edges: &'a [Edge<E, Idx>],
  pub next: [EdgeIndex<Idx>; 2],
}

impl<'a, E: 'a, Idx> Neighbors<'a, E, Idx>
where
  Idx: Index,
{
  /// Create a new `Neighbors` instance.
  pub fn new(
    skip_start: NodeIndex<Idx>,
    edges: &'a [Edge<E, Idx>],
    next: [EdgeIndex<Idx>; 2],
  ) -> Self {
    Self {
      skip_start,
      edges,
      next,
    }
  }
}

impl<'a, E, Idx> Iterator for Neighbors<'a, E, Idx>
where
  Idx: Index,
{
  type Item = NodeIndex<Idx>;

  fn next(&mut self) -> Option<Self::Item> {
    // First; any outgoing edges.
    if let Some(edge) = self.edges.get(self.next[0].index()) {
      self.next[0] = edge.next[0];
      return Some(edge.node[1]);
    }

    // Then incoming edges.
    // For an undirected iterator: traverse both incoming & outgoing edge list.
    // Make sure we don't double count self loops by skipping them in the incoming
    // list.
    while let Some(edge) = self.edges.get(self.next[1].index()) {
      self.next[1] = edge.next[1];
      if edge.node[0] != self.skip_start {
        return Some(edge.node[0]);
      }
    }

    None
  }
}

impl<'a, E, Idx: Index> Clone for Neighbors<'a, E, Idx> {
  clone_fields!(Neighbors, skip_start, edges, next);
}

impl<'a, E, Idx: Index> Neighbors<'a, E, Idx> {
  /// Retruns a "walker" object that can be used to step
  /// through the neighbors and edges from the origin node.
  pub fn detach(&self) -> Walker<Idx> {
    Walker {
      skip_start: self.skip_start,
      next: self.next,
    }
  }
}

/// Neighbors "walker" object.
trait WalkerObj<N, E, T: EdgeType, Idx: Index> {
  /// Returns next `EdgeIndex` and next `NodeIndex` in the loop.
  fn next(
    &mut self,
    g: &Graph<N, E, T, Idx>,
  ) -> Option<(EdgeIndex<Idx>, NodeIndex<Idx>)>;

  /// Next `EdgeIndex` in the loop.
  fn next_edge(&mut self, g: &Graph<N, E, T, Idx>) -> Option<EdgeIndex<Idx>> {
    self.next(g).map(|t| t.0)
  }

  /// Next `NodeIndex` in the loop.
  fn next_node(&mut self, g: &Graph<N, E, T, Idx>) -> Option<NodeIndex<Idx>> {
    self.next(g).map(|t| t.1)
  }
}

/// A "walker" object that can be used to step through the edge list of a [`Node`]
///
/// Created with [`.detach()`].
///
/// [`Node`]: crate::graph::Node
/// [`.detach()`]: struct.Neighbors.html#method.detach
///
/// The walker does not borrow from the graph, so it lets you step through
/// neighbors or incident edges while also mutating graph weights.
pub struct Walker<Idx> {
  skip_start: NodeIndex<Idx>,
  next: [EdgeIndex<Idx>; 2],
}

impl<Idx: Index> Clone for Walker<Idx> {
  clone_fields!(Walker, skip_start, next);
}

impl<N, E, T, Idx> WalkerObj<N, E, T, Idx> for Walker<Idx>
where
  T: EdgeType,
  Idx: Index,
{
  fn next(
    &mut self,
    g: &Graph<N, E, T, Idx>,
  ) -> Option<(EdgeIndex<Idx>, NodeIndex<Idx>)> {
    // First any outgoing edges.
    if let Some(edge) = g.edges.get(self.next[0].index()) {
      let e = self.next[0];
      self.next[0] = edge.next[0];
      return Some((e, edge.node[1]));
    }

    // Then incoming edges
    // For an `Undirected` iteator (traverse both incoming and outoing
    // edge lists).
    //
    // NOTE: Make sure we don't double count self loops by skipping
    // them in the incoming list.
    while let Some(edge) = g.edges.get(self.next[1].index()) {
      let e = self.next[1];
      self.next[1] = edge.next[1];
      if edge.node[0] != self.skip_start {
        return Some((e, edge.node[0]));
      }
    }

    None
  }
}

/// Mutable walker object over `Edges`.
pub(crate) struct EdgesWalkerMut<'a, E: 'a, Idx: Index = DefaultIdx> {
  pub(crate) edges: &'a mut [Edge<E, Idx>],
  pub(crate) next: EdgeIndex<Idx>,
  pub(crate) direction: Direction,
}

impl<'a, E: 'a, Idx> EdgesWalkerMut<'a, E, Idx>
where
  Idx: Index,
{
  /// Create new mutable `Walker` object over `Edges`.
  pub(crate) fn new(
    edges: &'a mut [Edge<E, Idx>],
    next: EdgeIndex<Idx>,
    direction: Direction,
  ) -> Self {
    Self {
      edges,
      next,
      direction,
    }
  }
}

impl<'a, E: 'a, Idx> EdgesWalkerMut<'a, E, Idx>
where
  Idx: Index,
{
  pub(crate) fn next_edge(&mut self) -> Option<&mut Edge<E, Idx>> {
    self.next().map(|t| t.1)
  }

  pub(crate) fn next(&mut self) -> Option<(EdgeIndex<Idx>, &mut Edge<E, Idx>)> {
    let this_index = self.next;
    let k = self.direction.index();
    match self.edges.get_mut(self.next.index()) {
      Some(edge) => {
        self.next = edge.next[k];
        Some((this_index, edge))
      }
      None => None,
    }
  }
}

/// Create a mutable walker object over Edges
pub(crate) fn edges_walker_mut<E, Idx>(
  edges: &mut [Edge<E, Idx>],
  next: EdgeIndex<Idx>,
  direction: Direction,
) -> EdgesWalkerMut<E, Idx>
where
  Idx: Index,
{
  EdgesWalkerMut::new(edges, next, direction)
}

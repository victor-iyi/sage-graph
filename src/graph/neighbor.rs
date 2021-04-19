#![allow(dead_code)]

use super::{
  edge::Edge,
  index::{DefaultIdx, EdgeIndex, Index, NodeIndex},
};
use crate::clone_fields;

pub struct Neighbors<'a, E: 'a, Idx: 'a = DefaultIdx> {
  skip_start: NodeIndex<Idx>,
  edges: &'a [Edge<E, Idx>],
  next: [EdgeIndex<Idx>; 2],
}

impl<'a, E, Idx> Iterator for Neighbors<'a, E, Idx>
where
  Idx: Index,
{
  type Item = NodeIndex<Idx>;

  fn next(&mut self) -> Option<Self::Item> {
    // First; any outgoing edges.
    match self.edges.get(self.next[0].index()) {
      Some(edge) => {
        self.next[0] = edge.next[0];
        return Some(edge.node[1]);
      }
      None => {}
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

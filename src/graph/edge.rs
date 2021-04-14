use crate::clone_fields;
use crate::graph::{
  direction::{Directed, Direction, Undirected},
  index::{edge_index, DefaultIdx, EdgeIndex, Index, NodeIndex},
};
use std::marker::PhantomData;

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
  /// Create a new edge.
  pub fn new(
    weight: E,
    next: [EdgeIndex<Idx>; 2],
    node: [NodeIndex<Idx>; 2],
  ) -> Self {
    Edge { weight, next, node }
  }

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

/// Iterator over the edges of from or to a node.
pub struct Edges<'a, E: 'a, D, Idx: 'a = DefaultIdx>
where
  D: EdgeDirection,
  Idx: Index,
{
  /// starting node to skip over.
  skip_start: NodeIndex<Idx>,
  edges: &'a [Edge<E, Idx>],

  /// Next edge to visit.
  next: [EdgeIndex<Idx>; 2],

  /// For directed graphs; the direction to iterate in.
  /// For undirected graphs; the direction of edges.
  direction: Direction,
  d: PhantomData<D>,
}

impl<'a, E, D, Idx> Iterator for Edges<'a, E, D, Idx>
where
  D: EdgeDirection,
  Idx: Index,
{
  type Item = EdgeReference<'a, E, Idx>;

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
    let (iterate_over, reverse) = if D::is_directed() {
      (Some(self.direction), None)
    } else {
      (None, Some(self.direction.opposite()))
    };

    if iterate_over.unwrap_or(Direction::Outgoing) == Direction::Outgoing {
      let i = self.next[0].index();
      if let Some(Edge { node, weight, next }) = self.edges.get(i) {
        self.next[0] = next[0];
        return Some(EdgeReference {
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

        return Some(EdgeReference {
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

impl<'a, E, D, Idx> Clone for Edges<'a, E, D, Idx>
where
  Idx: Index,
  D: EdgeDirection,
{
  fn clone(&self) -> Self {
    Edges {
      skip_start: self.skip_start,
      edges: self.edges,
      next: self.next,
      direction: self.direction,
      d: self.d,
    }
  }
}

/// Iterator over the multiple directed edges connecting a source node to a target node.
pub struct EdgesConnecting<'a, E: 'a, D, Idx: 'a = DefaultIdx>
where
  D: EdgeDirection,
  Idx: Index,
{
  target_node: NodeIndex<Idx>,
  edges: Edges<'a, E, D, Idx>,
  d: PhantomData<D>,
}

impl<'a, E, D, Idx> Iterator for EdgesConnecting<'a, E, D, Idx>
where
  D: EdgeDirection,
  Idx: Index,
{
  type Item = EdgeReference<'a, E, Idx>;

  fn next(&mut self) -> Option<EdgeReference<'a, E, Idx>> {
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
pub struct EdgeReference<'a, E: 'a, Idx = DefaultIdx> {
  index: EdgeIndex<Idx>,
  node: [NodeIndex<Idx>; 2],
  weight: &'a E,
}

impl<'a, Idx, E> EdgeReference<'a, E, Idx>
where
  Idx: Index,
{
  pub fn weight(&self) -> &'a E {
    self.weight
  }
}

impl<'a, E, Idx: Index> Clone for EdgeReference<'a, E, Idx> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<'a, E, Idx: Index> Copy for EdgeReference<'a, E, Idx> {}

impl<'a, E, Idx: Index> PartialEq for EdgeReference<'a, E, Idx>
where
  E: PartialEq,
{
  fn eq(&self, rhs: &Self) -> bool {
    self.index == rhs.index && self.weight == rhs.weight
  }
}

/// Iterator over all edges of a graph.
pub struct EdgeReferences<'a, E: 'a, Idx: Index = DefaultIdx> {
  iter: std::iter::Enumerate<std::slice::Iter<'a, Edge<E, Idx>>>,
}

impl<'a, E, Idx> Iterator for EdgeReferences<'a, E, Idx>
where
  Idx: Index,
{
  type Item = EdgeReference<'a, E, Idx>;

  fn next(&mut self) -> Option<Self::Item> {
    self.iter.next().map(|(i, edge)| EdgeReference {
      index: edge_index(i),
      node: edge.node,
      weight: &edge.weight,
    })
  }
}

/// Iterator over the edge indces of a graph.
#[derive(Clone, Debug)]
pub struct EdgeIndices<Idx = DefaultIdx> {
  r: std::ops::Range<usize>,
  t: PhantomData<fn() -> Idx>,
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

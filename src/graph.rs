pub mod direction;
pub mod edge;
pub mod index;
pub mod node;

use std::fmt;
use std::marker::PhantomData;
use std::mem::size_of;

use crate::iterator::format::{DebugMap, IterFormatExt, NoPretty};
use direction::{Directed, Undirected};
use edge::{Edge, EdgeDirection};
use index::{DefaultIdx, EdgeIndex, Index, NodeIndex};
use node::Node;

/// [`Graph`] - a graph data structure using an adjacency list representation.
///
/// [`Graph`]: struct.Graph
pub struct Graph<N, E, D = Directed, Idx = DefaultIdx> {
  nodes: Vec<Node<N, Idx>>,
  edges: Vec<Edge<E, Idx>>,
  direction: PhantomData<D>,
}

/// A directed Graph i.e a graph with directed [`Edge`]s.
///
/// [`Edge`]: crate::Edge
pub type DiGraph<N, E, Idx = DefaultIdx> = Graph<N, E, Directed, Idx>;

/// An undirected Graph i.e a graph with undirected [`Edge`]s.
///
/// [`Edge`]: create::Edge
pub type UnDiGraph<N, E, Idx = DefaultIdx> = Graph<N, E, Undirected, Idx>;

impl<N, E, D, Idx: Index> Clone for Graph<N, E, D, Idx>
where
  N: Clone,
  E: Clone,
{
  fn clone(&self) -> Self {
    Graph {
      nodes: self.nodes.clone(),
      edges: self.edges.clone(),
      direction: self.direction,
    }
  }

  fn clone_from(&mut self, rhs: &Self) {
    self.nodes.clone_from(&rhs.nodes);
    self.edges.clone_from(&rhs.edges);
    self.direction = rhs.direction;
  }
}

impl<N, E, D, Idx> Graph<N, E, D, Idx>
where
  D: EdgeDirection,
  Idx: Index,
{
  /// Create a new [`Graph`] with estimated capacity.
  pub fn with_capacity(nodes: usize, edges: usize) -> Self {
    Graph {
      nodes: Vec::with_capacity(nodes),
      edges: Vec::with_capacity(edges),
      direction: PhantomData,
    }
  }

  /// Return the number of nodes (vertices) in the graph.
  ///
  /// Computes in `O(1)` time.
  pub fn node_count(&self) -> usize {
    self.nodes.len()
  }

  /// Return the number of edges in the graph.
  ///
  /// Computes in `O(1)` time.
  pub fn edge_count(&self) -> usize {
    self.edges.len()
  }

  /// Check if graph is a Directed or Undirected graph.
  #[inline]
  pub fn is_directed(&self) -> bool {
    D::is_directed()
  }

  /// Add a node (also called vertex) with associated data `weight` to the graph.
  ///
  /// Computes in `O(1)` time.
  ///
  /// Return the index of the new node.
  ///
  /// **Panics** if the Graph is at the maximum capacity of nodes for its index
  /// type (N/A if usize).
  pub fn add_node(&mut self, weight: N) -> NodeIndex<Idx> {
    let node = Node::new(weight, [EdgeIndex::end(), EdgeIndex::end()]);
    let node_idx = NodeIndex::new(self.nodes.len());
    // Check for max capacity, except if we use `usize`.
    assert!(
      <Idx as Index>::max().index() == !0 || NodeIndex::end() != node_idx
    );
    self.nodes.push(node);
    node_idx
  }

  /// Access the weight for a given node.
  ///
  /// Also available with indexing: `&graph[node]`.
  pub fn node_weight(&self, node: NodeIndex<Idx>) -> Option<&N> {
    self.nodes.get(node.index()).map(|n| &n.weight)
  }
}

impl<N, E, D, Idx> fmt::Debug for Graph<N, E, D, Idx>
where
  N: fmt::Debug,
  E: fmt::Debug,
  D: EdgeDirection,
  Idx: Index,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let direction = if self.is_directed() {
      "Directed"
    } else {
      "Undirected"
    };
    let mut fmt_struct = f.debug_struct("Graph");
    fmt_struct.field("direction", &direction);
    fmt_struct.field("node_count", &self.node_count());
    fmt_struct.field("edge_count", &self.edge_count());

    if self.edge_count() > 0 {
      fmt_struct.field(
        "edges",
        &self
          .edges
          .iter()
          .map(|e| NoPretty((e.source().index(), e.target().index())))
          .format(", "),
      );
    }

    // skip weights if they are ZST!
    if size_of::<N>() != 0 {
      fmt_struct.field(
        "node_weights",
        &DebugMap(|| self.nodes.iter().map(|n| &n.weight).enumerate()),
      );
    }

    if size_of::<E>() != 0 {
      fmt_struct.field(
        "edge_weights",
        &DebugMap(|| self.edges.iter().map(|e| &e.weight).enumerate()),
      );
    }

    fmt_struct.finish()
  }
}

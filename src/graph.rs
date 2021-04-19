#![allow(dead_code)]

use std::fmt;
use std::marker::PhantomData;
use std::mem::size_of;

pub mod direction;
pub mod edge;
pub mod index;
pub mod neighbor;
pub mod node;

use crate::iterator::format::{DebugMap, IterFormatExt, NoPretty};
use direction::{Directed, Direction, Undirected, DIRECTIONS};
use edge::{Edge, EdgeType};
use index::{DefaultIdx, EdgeIndex, Index, NodeIndex};
use node::Node;

/// [`Graph`] - a graph data structure using an adjacency list representation.
///
/// [`Graph`]: struct.Graph
pub struct Graph<N, E, T = Directed, Idx = DefaultIdx> {
  nodes: Vec<Node<N, Idx>>,
  edges: Vec<Edge<E, Idx>>,
  _type: PhantomData<T>,
}

/// A directed Graph i.e a graph with directed [`Edge`]s.
///
/// [`Edge`]: crate::Edge
pub type DiGraph<N, E, Idx = DefaultIdx> = Graph<N, E, Directed, Idx>;

/// An undirected Graph i.e a graph with undirected [`Edge`]s.
///
/// [`Edge`]: create::Edge
pub type UnDiGraph<N, E, Idx = DefaultIdx> = Graph<N, E, Undirected, Idx>;

impl<N, E, T, Idx: Index> Clone for Graph<N, E, T, Idx>
where
  N: Clone,
  E: Clone,
{
  fn clone(&self) -> Self {
    Graph {
      nodes: self.nodes.clone(),
      edges: self.edges.clone(),
      _type: self._type,
    }
  }

  fn clone_from(&mut self, rhs: &Self) {
    self.nodes.clone_from(&rhs.nodes);
    self.edges.clone_from(&rhs.edges);
    self._type = rhs._type;
  }
}

impl<N, E, T, Idx> Graph<N, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
  /// Create a new [`Graph`] with estimated capacity.
  pub fn with_capacity(nodes: usize, edges: usize) -> Self {
    Graph {
      nodes: Vec::with_capacity(nodes),
      edges: Vec::with_capacity(edges),
      _type: PhantomData,
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
    T::is_directed()
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

  /// Mutably access the weight for a node.
  ///
  /// Also availble with indexing: `&mut graph[node]`.
  pub fn node_weight_mut(&mut self, node: NodeIndex<Idx>) -> Option<&mut N> {
    self.nodes.get_mut(node.index()).map(|n| &mut n.weight)
  }

  /// Add an edge from `a` to `b` to the graph, with its associated data weight.
  ///
  /// Return the index of the new edge.
  ///
  /// **Panics** if any of the nodes don't exist.
  ///
  /// **Panics** if the [`Graph`] is at the maximum number of edges for its index type
  /// (N/a if usize).
  ///
  /// **NOTE:** `Graph` allows adding parallel ("duplicate") edges. If you want to avoid
  /// this, use [`update_edge(a, b, weight)`] instead.
  ///
  /// [`Graph`]: struct.Graph
  /// [`update_edge(a, b, weight)`]: #method.update_edge
  pub fn add_edge(
    &mut self,
    a: NodeIndex<Idx>,
    b: NodeIndex<Idx>,
    weight: E,
  ) -> EdgeIndex<Idx> {
    let edge_idx = EdgeIndex::new(self.edges.len());
    assert!(
      <Idx as Index>::max().index() == !0 || EdgeIndex::end() != edge_idx
    );

    let mut edge = Edge::new(weight, [EdgeIndex::end(); 2], [a, b]);
    match index_twice(&mut self.nodes, a.index(), b.index()) {
      Pair::None => panic!("Graph::add_edge: node indices out of bounds"),
      Pair::One(an) => {
        edge.next = an.next;
        an.next[0] = edge_idx;
        an.next[1] = edge_idx;
      }
      Pair::Both(an, bn) => {
        // `a` and `b` are differnt indices.
        edge.next = [an.next[0], bn.next[1]];
        an.next[0] = edge_idx;
        bn.next[1] = edge_idx;
      }
    }
    self.edges.push(edge);
    edge_idx
  }

  /// Add or update an edge from `a` to `b`.
  /// If the edge already exists, its weight is updated.
  ///
  /// Return the index of the affected edge.
  ///
  /// Computes in `O(e)` time, where `e` is the number of edges connected to `a`
  /// (and `b`, if the graph edges are undirected).
  ///
  /// **Panics** if any of the nodes don't exist.
  pub fn update_edge(
    &mut self,
    a: NodeIndex<Idx>,
    b: NodeIndex<Idx>,
    weight: E,
  ) -> EdgeIndex<Idx> {
    if let Some(idx) = self.find_edge(a, b) {
      if let Some(edge) = self.edge_weight_mut(idx) {
        *edge = weight;
        return idx;
      }
    }

    self.add_edge(a, b, weight)
  }

  /// Access the weight for a given edge.
  ///
  /// Also available with indexing: `&graph[edge]`.
  pub fn edge_weight(&self, edge: EdgeIndex<Idx>) -> Option<&E> {
    self.edges.get(edge.index()).map(|e| &e.weight)
  }

  /// Mutably access the weight for a given edge.
  ///
  /// Also available with indexing: `&mut graph[edge]`.
  pub fn edge_weight_mut(&mut self, edge: EdgeIndex<Idx>) -> Option<&mut E> {
    self.edges.get_mut(edge.index()).map(|e| &mut e.weight)
  }

  /// Access the source and target nodes for `edge`.
  pub fn edge_endpoints(
    &self,
    edge: EdgeIndex<Idx>,
  ) -> Option<(NodeIndex<Idx>, NodeIndex<Idx>)> {
    self
      .edges
      .get(edge.index())
      .map(|e| (e.source(), e.target()))
  }

  /// Lookup if there's an edge from `a` to `b`.
  ///
  /// Computes in `O(e')` time, where `e'` is the number of edges connected to `a`
  /// (and `b`, if the graph edges are undirected).
  pub fn contains_edge(&self, a: NodeIndex<Idx>, b: NodeIndex<Idx>) -> bool {
    self.find_edge(a, b).is_some()
  }

  /// Lookup an edge from `a` to `b`.
  ///
  /// Computes in `O(e')` time, where `e'` is the number of edges connected to `a`
  /// (and `b`, if the graph edges are undirected).
  pub fn find_edge(
    &self,
    a: NodeIndex<Idx>,
    b: NodeIndex<Idx>,
  ) -> Option<EdgeIndex<Idx>> {
    if !self.is_directed() {
      self.find_edge_undirected(a, b).map(|(idx, _)| idx)
    } else {
      match self.nodes.get(a.index()) {
        Some(node) => self.find_edge_directed_from_node(node, b),
        None => None,
      }
    }
  }

  fn find_edge_directed_from_node(
    &self,
    node: &Node<N, Idx>,
    b: NodeIndex<Idx>,
  ) -> Option<EdgeIndex<Idx>> {
    let mut edge_idx = node.next[0];
    while let Some(edge) = self.edges.get(edge_idx.index()) {
      if edge.node[1] == b {
        return Some(edge_idx);
      }
      edge_idx = edge.next[0];
    }
    None
  }

  /// Lookup an edge between `a` and `b`, in either direction.
  ///
  /// If the graph is undirected, then this is equivalent to [`find_edge()`].
  ///
  /// Return the edge index and it's directionality with [`Outgoing`] meaning
  /// from `a` to `b` and `Incoming` the reverse, or `None` if the edge does
  /// not exist.
  ///
  /// [`find_edge()`]: #method.find_edge
  /// [`Outgoing`]: crate::graph::direction::Direction::Outgoing
  fn find_edge_undirected(
    &self,
    a: NodeIndex<Idx>,
    b: NodeIndex<Idx>,
  ) -> Option<(EdgeIndex<Idx>, Direction)> {
    match self.nodes.get(a.index()) {
      Some(node) => self.find_edge_undirected_from_node(node, b),
      None => None,
    }
  }

  fn find_edge_undirected_from_node(
    &self,
    node: &Node<N, Idx>,
    b: NodeIndex<Idx>,
  ) -> Option<(EdgeIndex<Idx>, Direction)> {
    for &d in &DIRECTIONS {
      let k = d.index();
      let mut edge_idx = node.next[k];
      while let Some(edge) = self.edges.get(edge_idx.index()) {
        if edge.node[1 - k] == b {
          return Some((edge_idx, d));
        }
        edge_idx = edge.next[k];
      }
    }
    None
  }
}

impl<N, E, T, Idx> fmt::Debug for Graph<N, E, T, Idx>
where
  N: fmt::Debug,
  E: fmt::Debug,
  T: EdgeType,
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

impl<N, E> Graph<N, E, Directed> {
  /// Creates a new directed Graph (i.e graph with directed eges).
  ///
  /// This is a convenience method. Use `Graph::with_capacity` or `Graph::default`
  /// for a constructor that is generic in all the type parameters of Graph.
  pub fn new() -> Self {
    Graph {
      nodes: Vec::new(),
      edges: Vec::new(),
      _type: PhantomData,
    }
  }
}

impl<N, E> Default for Graph<N, E> {
  fn default() -> Self {
    Self::new()
  }
}

impl<N, E> Graph<N, E, Undirected> {
  /// Creates a new undirected Graph (i.e graph with no directed edges).
  ///
  /// This is a convenience method. Use `Graph::with_capacity` or `Graph::default`
  /// for a constructor that is generic in all the type parameters of Graph.
  pub fn new_undirected() -> Self {
    Graph {
      nodes: Vec::new(),
      edges: Vec::new(),
      _type: PhantomData,
    }
  }
}

/// `Fozen` graph only allows shared (read-only) access to `Graph`,
/// but allows mutable acess to its node and edge associated data.
///
/// It ensures immutability of `Graph`'s structure while permitting weights
/// to be both read & write.
pub struct Frozen<'a, G: 'a>(&'a mut G);

/// `Pair` two or one values together.
enum Pair<T> {
  Both(T, T),
  One(T),
  None,
}

/// Get mutable reference at index `a` and `b`.
fn index_twice<T>(slice: &mut [T], a: usize, b: usize) -> Pair<&mut T> {
  if std::cmp::max(a, b) >= slice.len() {
    Pair::None
  } else if a == b {
    Pair::One(&mut slice[std::cmp::max(a, b)])
  } else {
    // SAFETY: a & b are in bounds and distinct.
    unsafe {
      let ptr = slice.as_mut_ptr();
      let ar = &mut *ptr.add(a);
      let br = &mut *ptr.add(b);
      Pair::Both(ar, br)
    }
  }
}

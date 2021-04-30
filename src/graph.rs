#![allow(dead_code)]

use std::fmt;
use std::marker::PhantomData;
use std::mem::size_of;

pub mod direction;
pub mod edge;
pub mod externals;
pub mod index;
pub mod neighbor;
pub mod node;

use crate::{
  iterator::format::{DebugMap, IterFormatExt, NoPretty},
  util::enumerate,
};

use self::{
  direction::{Directed, Direction, Undirected, DIRECTIONS},
  edge::{
    Edge, EdgeIndices, EdgeIterator, EdgeType, EdgeWeightsMut, Edges,
    EdgesConnecting,
  },
  externals::Externals,
  index::{DefaultIdx, EdgeIndex, GraphIndex, Index, NodeIndex},
  neighbor::{edges_walker_mut, Neighbors},
  node::{Node, NodeIndices, NodeWeightsMut},
};

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

  /// Remove `a` from the graph if it exists, and return its weight.
  /// If it doesn't exist in the graph, return `None`.
  ///
  /// Apart from `a`, this invalidatese the last node index in the graph
  /// (that node will adopt the removed node index). Edge indices are
  /// invalidated as they would be following the removal of each edge with
  /// an endpoint in `a`.
  ///
  /// Computes in `O(e')` time, where `e'` is the number of affected edges,
  /// including `n` calls to `.remove_edge()` where `n` is the number of edges
  /// with an endpoing in `a`, and including the edges with an endpoint in the
  /// displaced node.
  pub fn remove_node(&mut self, a: NodeIndex<Idx>) -> Option<N> {
    self.nodes.get(a.index())?;
    for d in &DIRECTIONS {
      let k = d.index();

      // Remove all edges from and to this node.
      loop {
        let next = self.nodes[a.index()].next[k];
        if next == EdgeIndex::end() {
          break;
        }
        let ret = self.remove_edge(next);
        debug_assert!(ret.is_some());
        let _ = ret;
      }
    }

    // Use swap_remove -- only the swapped-in node is going to change
    // `NodeIndex<Idx>`, so we only have to walk its edges and update them.

    let node = self.nodes.swap_remove(a.index());

    // Find the edge lists of the node that had to relocate.
    // It may be that no node had to relocate, then we are done already.
    let swap_edges = match self.nodes.get(a.index()) {
      Some(e) => e.next,
      None => return Some(node.weight),
    };

    // The swapped element's old index.
    let old_index = NodeIndex::new(self.nodes.len());
    let new_index = a;

    // Adjust the starts of the out edges, and ends of the in edges.
    for &d in &DIRECTIONS {
      let k = d.index();
      let mut edges = edges_walker_mut(&mut self.edges, swap_edges[k], d);
      while let Some(current_edge) = edges.next_edge() {
        debug_assert!(current_edge.node[k] == old_index);
        current_edge.node[k] = new_index;
      }
    }

    Some(node.weight)
  }

  /// Remove an edge and return its edge weight or `None` if it doesn't exist.
  ///
  /// Apart from `e`, this invalidates the last edge index in the graph (that
  /// edge will adopt the removed edge index).
  ///
  /// Computes in `O(e')` time, wehere `e'` is the size of four particular edge
  /// lists, for the vertices of `e` and the vertices of another affected edge.
  pub fn remove_edge(&mut self, e: EdgeIndex<Idx>) -> Option<E> {
    // Every edge is part of two lists: Outgoing & Incoming edges.
    // Remove it from both.
    let (edge_node, edge_next) = match self.edges.get(e.index()) {
      Some(edge) => (edge.node, edge.next),
      None => return None,
    };

    // Remove the edge from its in and out lists by replacing it with a
    // link to the next in the list.
    self.change_edge_links(edge_node, e, edge_next);
    self.remove_edge_adjust_indices(e)
  }

  fn remove_edge_adjust_indices(&mut self, e: EdgeIndex<Idx>) -> Option<E> {
    // swap_remove the edge -- only the removed edge and the edge swapped into
    // place are affected and need updating indices.
    let edge = self.edges.swap_remove(e.index());
    let swap = match self.edges.get(e.index()) {
      Some(edge_) => edge_.node,
      // No element needed to be swapped.
      None => return Some(edge.weight),
    };
    let swapped_e = EdgeIndex::new(self.edges.len());

    // Update the edge lists by replacing links to the old index by ref to the
    // new edge index.
    self.change_edge_links(swap, swapped_e, [e, e]);
    Some(edge.weight)
  }

  /// For edge `e` with endpoint `edge_node`, replace links to it,
  /// with links to `edge_next`.
  fn change_edge_links(
    &mut self,
    edge_node: [NodeIndex<Idx>; 2],
    e: EdgeIndex<Idx>,
    edge_next: [EdgeIndex<Idx>; 2],
  ) {
    for &d in &DIRECTIONS {
      let k = d.index();
      let node = match self.nodes.get_mut(edge_node[k].index()) {
        Some(r) => r,
        None => {
          debug_assert!(
            false,
            "Edge's endpoint direction={:?} index={:?} not found",
            d, edge_node[k]
          );
          return;
        }
      };

      let first = node.next[k];
      if first == e {
        println!(
          "Updating first edge 0 for node {}, set to {}",
          edge_node[0], edge_next[0]
        );
        node.next[k] = edge_next[k];
      } else {
        let mut edges = edges_walker_mut(&mut self.edges, first, d);
        while let Some(current_edge) = edges.next_edge() {
          if current_edge.next[k] == e {
            current_edge.next[k] = edge_next[k];
            break; // the edge can only be present once in the list.
          }
        }
      }
    }
  }

  /// Return an iteator of all nodes with an edge starting from `a`.
  ///
  /// - `Directed`: Outgoing edges from `a`.
  /// - `Undirected`: All edges from or to `a`.
  ///
  /// Produces an empty iteartor if the node doesn't exist.
  ///
  /// Iterator element type is `NodeIndex<Idx>`.
  ///
  /// Use [`.neighbors(a).detach()`][1] to get neighbor walker that does not
  /// borrow from the graph.
  ///
  /// [1]: crate::graph::neighbor::Neighbors.html#method.detach
  pub fn neighbors(&self, a: NodeIndex<Idx>) -> Neighbors<E, Idx> {
    self.neighbors_directed(a, Direction::Outgoing)
  }

  /// Retrun an iterator of all neighbors that habe an edge between them and `a`,
  /// in the specified direction.
  ///
  /// If the graph's edge are undirected, this is equivalent to `*.neighbors(a)*.`
  ///
  /// - `Directed`, `Outgoing`: All edges from `a`.
  /// - `Directed`, `Incoming`: All edges to `a`.
  /// - `Undirected`: All edges from or to `a`.
  ///
  /// Produces an empty iterator if the node doesn't exist.
  ///
  /// Iterator element type is `NodeIndex<Idx>`.
  ///
  /// For a `Directed` graph, neighbors are listed in reverse order of their
  /// addition to the graph, so the most recently added edge's neighbor walker
  /// that does not borrow form the graph.
  ///
  /// Use [`.neighbors_directed(a, direction).detach()`][1] to get a neighbor walker
  /// that does not borrow from the graph.
  ///
  /// [1]: struct.Neighbors.html#method.detach
  pub fn neighbors_directed(
    &self,
    a: NodeIndex<Idx>,
    direction: Direction,
  ) -> Neighbors<E, Idx> {
    let mut iter = self.neighbors_undirected(a);
    if self.is_directed() {
      let k = direction.index();
      iter.next[1 - k] = EdgeIndex::end();
      iter.skip_start = NodeIndex::end();
    }

    iter
  }

  /// Return an iterator of all neighbors that have an edge between them and
  /// `a`, in either direction.
  ///
  /// If the graph's edges are undirected, this is equivalent to `*.neighbors(a)*.`
  ///
  /// - `Directed` and `Undirected`: All edges from or to `a`.
  ///
  /// Produces an empty iterator if the ndoe doesn't exist.
  ///
  /// Iterator element type is `NodeIndex<Idx>`.
  ///
  /// Use [`.neighbors_undirected(a).detach()`][1] to get neighbor walker that
  /// does not borrow from the graph.
  ///
  /// [1]: struct.Neibors.html#method.detach
  pub fn neighbors_undirected(&self, a: NodeIndex<Idx>) -> Neighbors<E, Idx> {
    let next = match self.nodes.get(a.index()) {
      Some(n) => n.next,
      None => [EdgeIndex::end(), EdgeIndex::end()],
    };

    Neighbors::new(a, &self.edges, next)
  }

  /// Return an iteator of all edges of `a`.
  ///
  /// - `Directed`: Outgoing edges from `a`.
  /// - `Undirected`: All edges connected to `a`.
  ///
  /// Produces an empty iterator if the node doesn't exist.
  ///
  /// Iterator element type is `EdgeRef<E, Idx>`.
  pub fn edges(&self, a: NodeIndex<Idx>) -> Edges<E, T, Idx> {
    self.edges_directed(a, Direction::Outgoing)
  }

  /// Return an iterator of all edges of `a`, in the specific direction.
  ///
  /// - `Directed`, `Outgoing`: All edges from `a`.
  /// - `Directed`, `Incoming`: All edges from `a`.
  /// - `Undirected`, `Outgoing`: All edges connected to `a`, beign the source
  ///   of each edge.
  /// - `undirected`, `Incoming`: All edges connected to `a`, with `a` being
  ///   the target of each edge.
  ///
  ///   Produces an empty iteator if the node `a` doesn't exist.
  ///
  ///   Itator element type is `EdgeRef<E, Idx>`.
  pub fn edges_directed(
    &self,
    a: NodeIndex<Idx>,
    direction: Direction,
  ) -> Edges<E, T, Idx> {
    let next = match self.nodes.get(a.index()) {
      Some(n) => n.next,
      None => [EdgeIndex::end(), EdgeIndex::end()],
    };

    Edges::new(a, &self.edges, next, direction)
  }

  /// Return an iterator over all the edges connecting `a` and `b`.
  ///
  /// - `Directed`: Outgoing edges from `a`.
  /// - `Undirected`: All edges connected to `a`.
  ///
  /// Iterator element type is `[EdgeRef<E, Idx>]`.
  ///
  /// [EdgeRef<E, Idx>]: crate::graph::edge::EdgeRef<E, Idx>
  pub fn edges_connecting(
    &self,
    a: NodeIndex<Idx>,
    b: NodeIndex<Idx>,
  ) -> EdgesConnecting<E, T, Idx> {
    EdgesConnecting::new(b, self.edges_directed(a, Direction::Outgoing))
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

  /// Return an iterator over either the `Node`s without `Edge`s to them
  /// (`Incoming`) or from them (`Outgoing`).
  ///
  /// An *internal* node has both `Incoming` and `Outgoing` `Edge`s. The `Node`s
  /// in `.externals(Incoming)` are the source nodes and `.externals(Outgoing)`
  /// are the sinks of the graph.
  ///
  /// For a graph with undirected edges, both the sinks and the sources are just
  /// the nodes without edges.
  ///
  /// The whole iteration computes in `O(|V|)` time.
  pub fn externals(&self, direction: Direction) -> Externals<N, T, Idx> {
    Externals::new(self.nodes.iter().enumerate(), direction)
  }

  /// Return an iterator over the `Node` indices of the graph.
  ///
  /// For example, in a rare case wehere a graph algorithm were not applicable,
  /// the following code will iterate through all nodes to find a specific index.
  ///
  /// ```rust,norun
  /// # use sage_graph::Graph;
  /// # let mut g = Graph::<&str, i32>::new();
  /// # g.add_node("book");
  /// let index = g.node_indices().find(|i| g[*i] == "book").unwrap();
  /// ```
  pub fn node_indices(&self) -> NodeIndices<Idx> {
    NodeIndices::new(0..self.node_count())
  }

  /// Return an iterator over the edge indices of the graph.
  pub fn edge_indices(&self) -> EdgeIndices<Idx> {
    EdgeIndices::new(0..self.edge_count())
  }

  /// Create an iterator over all edges; in indexed order.
  ///
  /// Iterator element type is `EdgeRef<E, Idx>`.
  pub fn edge_iterator(&self) -> EdgeIterator<E, Idx> {
    EdgeIterator::new(self.edges.iter().enumerate())
  }

  /// Return an iterator yielding mutable access to all node weights.
  ///
  /// The order in which weights are yielded matches the order of their
  /// node indices.
  pub fn node_weights_mut(&mut self) -> NodeWeightsMut<N, Idx> {
    NodeWeightsMut::new(self.nodes.iter_mut())
  }

  /// Return an iterator yielding mutable access to all edge wiehgts.
  ///
  /// The order in which weights are yielded matches the order of their
  /// edge indices.
  pub fn edge_weights_mut(&mut self) -> EdgeWeightsMut<E, Idx> {
    EdgeWeightsMut::new(self.edges.iter_mut())
  }

  /// Reamaining methods are the more interal falvour, read-only acess to the
  /// data structures's internals.

  /// Acess the interal node array.
  pub fn raw_nodes(&self) -> &[Node<N, Idx>] {
    &self.nodes
  }

  /// Access to the internal edge array.
  pub fn raw_edges(&self) -> &[Edge<E, Idx>] {
    &self.edges
  }

  /// Convert the graph into a vector of Nodes and vector of Edges.
  pub fn into_nodes_edges(self) -> (Vec<Node<N, Idx>>, Vec<Edge<E, Idx>>) {
    (self.nodes, self.edges)
  }

  /// Acessor for data structure internals: the first edge in the given direction.
  pub fn first_edge(
    &self,
    a: NodeIndex<Idx>,
    direction: Direction,
  ) -> Option<EdgeIndex<Idx>> {
    match self.nodes.get(a.index()) {
      Some(node) => {
        let edge_idx = node.next[direction.index()];
        if edge_idx == EdgeIndex::end() {
          None
        } else {
          Some(edge_idx)
        }
      }
      None => None,
    }
  }

  /// Accessor for data structure internals: the next edge for the given direction.
  pub fn next_edge(
    &self,
    e: EdgeIndex<Idx>,
    direction: Direction,
  ) -> Option<EdgeIndex<Idx>> {
    match self.edges.get(e.index()) {
      Some(node) => {
        let edge_idx = node.next[direction.index()];
        if edge_idx == EdgeIndex::end() {
          None
        } else {
          Some(edge_idx)
        }
      }
      None => None,
    }
  }

  /// Index the `Graph` by two indices, any combination of node or edge indices
  /// is fine.
  ///
  /// **Panics** If the indices are equal or if they are out of bounds.
  pub fn index_twice_mut<I, J>(
    &mut self,
    i: I,
    j: J,
  ) -> (
    &mut <Self as std::ops::Index<I>>::Output,
    &mut <Self as std::ops::Index<J>>::Output,
  )
  where
    Self: std::ops::IndexMut<I> + std::ops::IndexMut<J>,
    I: GraphIndex,
    J: GraphIndex,
  {
    assert!(I::is_node_index() != J::is_node_index() || i.index() != j.index());

    // SAFETY: Allow two mutable indices here -- they are non-overlapping.
    unsafe {
      let self_mut = self as *mut _;
      (
        <Self as std::ops::IndexMut<I>>::index_mut(&mut *self_mut, i),
        <Self as std::ops::IndexMut<J>>::index_mut(&mut *self_mut, j),
      )
    }
  }

  /// Reverse the direction of all edges.
  pub fn reverse(&mut self) {
    // Swap edge endpoints.
    // edge incoming / outgoing lists,
    // node incoming / outgoing lists
    for edge in &mut self.edges {
      edge.node.swap(0, 1);
      edge.next.swap(0, 1);
    }

    for node in &mut self.nodes {
      node.next.swap(0, 1);
    }
  }

  /// Remove all nodes and edges
  pub fn clear(&mut self) {
    self.nodes.clear();
    self.edges.clear();
  }

  /// Remove all edges
  pub fn clear_edges(&mut self) {
    self.edges.clear();
    for node in &mut self.nodes {
      node.next = [EdgeIndex::end(), EdgeIndex::end()];
    }
  }

  /// Return the current node and edge capacity of the graph.
  pub fn capacity(&self) -> (usize, usize) {
    (self.nodes.capacity(), self.edges.capacity())
  }

  /// Reserve capacity for at leaset `additional` more nodes to be inserted in
  /// the graph. Graph may reserve more spaces to avoid frequest reallocations.
  ///
  /// **Panics** If the new capacity overflows `usize`.
  pub fn reserve_nodes(&mut self, additional: usize) {
    self.nodes.reserve(additional);
  }

  /// Reserves capacity for at least `additional` more edges to be inserted in
  /// the graph. Graph may reserve more spaces to avoid frequent reallocations.
  ///
  /// **Panics** If the new capacity overflows `usize.`
  pub fn reserve_edges(&mut self, additional: usize) {
    self.edges.reserve(additional);
  }

  /// Reserve the minimum capacity for exactly `additional` more nodes to be
  /// inserted in the graph. Does nothing if the capacity is already sufficient.
  ///
  /// Prefer [`reserve_nodes`] if future insertions are expected.
  ///
  /// **Panics** If the new capacity overflows `usize`.
  ///
  /// [`reverve_nodes`]: struct.Graph.html#method.reserve_nodes
  pub fn reserve_exact_nodes(&mut self, additional: usize) {
    self.nodes.reserve_exact(additional);
  }

  /// Reserve the minimum capacity for exactly `additional` more edges to be
  /// inserted in the graph. Does nothing if the capacity is already sufficient.
  ///
  /// Prefer [`reserve_edges`] if future insertions are expected.
  ///
  /// **Panics** If the new capacity overflows `usize`.
  ///
  /// [`reverve_edges`]: struct.Graph.html#method.reserve_edges
  pub fn reserve_exact_edges(&mut self, additional: usize) {
    self.edges.reserve_exact(additional);
  }

  /// Shrinks the capacity of the underlying nodes collection as much as possible.
  pub fn shrink_to_fit_nodes(&mut self) {
    self.nodes.shrink_to_fit();
  }

  /// Shrinks the capacity of the underlying edges collection as much as possible.
  pub fn shrink_to_fit_edges(&mut self) {
    self.edges.shrink_to_fit();
  }

  /// Shrinks the capacity of the graph as much as possible.
  pub fn shrink_to_fit(&mut self) {
    self.nodes.shrink_to_fit();
    self.edges.shrink_to_fit();
  }

  /// Keep all nodes that return `true` from the `visit` closure, remove the others.
  ///
  /// `visit` is provided a proxy reference to the graph, so that the graph can be
  /// walked and associated data modified.
  ///
  /// The order nodes are visited is not specified.
  pub fn retain_nodes<F>(&mut self, mut visit: F)
  where
    F: FnMut(Frozen<Self>, NodeIndex<Idx>) -> bool,
  {
    for index in self.node_indices().rev() {
      if !visit(Frozen(self), index) {
        let ret = self.remove_node(index);
        debug_assert!(ret.is_some());
        let _ = ret;
      }
    }
  }

  /// Keep all edges that return `true` from the `visit` closure, remove the others.
  ///
  /// `visit` is provided a proxy reference to the graph, so that the graph can be
  /// walked and associated data modified.
  ///
  /// The order edges are visited is not specified.
  pub fn retain_edges<F>(&mut self, mut visit: F)
  where
    F: FnMut(Frozen<Self>, EdgeIndex<Idx>) -> bool,
  {
    for index in self.edge_indices().rev() {
      if !visit(Frozen(self), index) {
        let ret = self.remove_edge(index);
        debug_assert!(ret.is_some());
        let _ = ret;
      }
    }
  }

  /// Create a new `Graph` by mapping node and edge weights to new values.
  ///
  /// The resulting graph has the same structure and the same graph indices as
  /// `self`.
  pub fn map<'a, F, G, N2, E2>(
    &'a self,
    mut node_map: F,
    mut edge_map: G,
  ) -> Graph<N2, E2, T, Idx>
  where
    F: FnMut(NodeIndex<Idx>, &'a N) -> N2,
    G: FnMut(EdgeIndex<Idx>, &'a E) -> E2,
  {
    let mut g = Graph::with_capacity(self.node_count(), self.edge_count());

    g.nodes.extend(enumerate(&self.nodes).map(|(i, node)| Node {
      weight: node_map(NodeIndex::new(i), &node.weight),
      next: node.next,
    }));

    g.edges.extend(enumerate(&self.edges).map(|(i, edge)| Edge {
      weight: edge_map(EdgeIndex::new(i), &edge.weight),
      next: edge.next,
      node: edge.node,
    }));

    g
  }

  /// Create a new `Graph` by mapping nodes and edges. A node or edge may be
  /// mapped to `None` to exclude it from the resulting graph.
  ///
  /// Nodes are mapped first with the `node_map` closure, then `edge_map` is
  /// called for the edges that have not had any endpoint removed.
  ///
  /// The resulting graph has the structure of a subgraph of the original graph.
  /// If no nodes are removed, the resulting graph has compactible node indices;
  /// if neither node nor edges are rmoved, the result has the same graph indices
  /// as `self`.
  pub fn filter_map<'a, F, G, N2, E2>(
    &'a self,
    mut node_map: F,
    mut edge_map: G,
  ) -> Graph<N2, E2, T, Idx>
  where
    F: FnMut(NodeIndex<Idx>, &'a N) -> Option<N2>,
    G: FnMut(EdgeIndex<Idx>, &'a E) -> Option<E2>,
  {
    let mut g = Graph::with_capacity(0, 0);

    // Mapping from old node index to new node index, end represents removed.
    let mut node_index_map = vec![NodeIndex::end(); self.node_count()];

    for (i, node) in enumerate(&self.nodes) {
      if let Some(node_weight) = node_map(NodeIndex::new(i), &node.weight) {
        node_index_map[i] = g.add_node(node_weight);
      }
    }

    for (i, edge) in enumerate(&self.edges) {
      // Skip edge if any endpoint was removed.
      let source = node_index_map[edge.source().index()];
      let target = node_index_map[edge.target().index()];
      if source != NodeIndex::end() && target != NodeIndex::end() {
        if let Some(edge_weight) = edge_map(EdgeIndex::new(i), &edge.weight) {
          g.add_edge(source, target, edge_weight);
        }
      }
    }

    g
  }

  /// Convert the graph into either undirected or directed. No edge adjustments
  /// are done, so you may want to go over the result to remove or add edges.
  ///
  /// Computes in `O(1)` time.
  pub fn into_edge_type<NewT>(self) -> Graph<N, E, NewT, Idx>
  where
    NewT: EdgeType,
  {
    Graph {
      nodes: self.nodes,
      edges: self.edges,
      _type: PhantomData,
    }
  }

  //
  // Internal methods
  //

  /// Fix up node and edge links after deserialization.
  // #[cfg(feature = "serde-1")]
  fn link_edges(&mut self) -> Result<(), NodeIndex<Idx>> {
    for (edge_index, edge) in enumerate(&mut self.edges) {
      let a = edge.source();
      let b = edge.target();
      let edge_idx = EdgeIndex::new(edge_index);

      match index_twice(&mut self.nodes, a.index(), b.index()) {
        Pair::None => return Err(if a > b { a } else { b }),
        Pair::One(an) => {
          edge.next = an.next;
          an.next[0] = edge_idx;
          an.next[1] = edge_idx;
        }
        Pair::Both(an, bn) => {
          // a and b are different indices
          edge.next = [an.next[0], bn.next[1]];
          an.next[0] = edge_idx;
          bn.next[1] = edge_idx;
        }
      }
    }

    Ok(())
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

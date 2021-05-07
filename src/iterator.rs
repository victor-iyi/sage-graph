use crate::{
  delegate_impl,
  graph::{
    direction::Direction,
    edge::{self, EdgeIterator, EdgeType},
    index::{EdgeIndex, Index, NodeIndex},
    neighbor,
    node::NodeIndices,
    Frozen, Graph,
  },
  trait_template,
};

use std::{
  collections::HashSet,
  hash::{BuildHasher, Hash},
};

use fixedbitset::FixedBitSet;

pub mod format;
pub mod util;

trait_template! {
    /// Base graph trait: defines the associated node identifier and
    /// edge identifier types.
    pub trait GraphBase {
        @escape [type NodeId]
        @escape [type EdgeId]
        @section nodelegate
        /// Edge identifier.
        type EdgeId: Copy + PartialEq;
        /// Node identifier.
        type NodeId: Copy + PartialEq;
    }
}

crate::GraphBase! {delegate_impl []}
crate::GraphBase! {delegate_impl [['a, G], G, &'a mut G, deref]}

/// A copyable reference to a graph.
pub trait GraphRef: Copy + GraphBase {}

impl<'a, G> GraphRef for &'a G where G: GraphBase {}

impl<'a, G> GraphBase for Frozen<'a, G>
where
  G: GraphBase,
{
  type NodeId = G::NodeId;
  type EdgeId = G::EdgeId;
}

// #[cfg(feature = "stable_graph")]
// impl<'a, N, E: 'a, T, Idx> IntoNeighbors for &'a StableGraph<N, E, T, Idx>
// where
//   T: EdgeType,
//   Idx: Index,
// {
//   type Neighbors = stable_graph::Neighbors<'a, E, Idx>;
//   fn neighbors(self, n: Self::NodeId) -> Self::Neighbors {
//     (*self).neighbors(n)
//   }
// }
//
// #[cfg(feature = "graphmap")]
// impl<'a, N: 'a, E, T> IntoNeighbors for &'a GraphMap<N, E, T>
// where
//   N: Copy + Ord + Hash,
//   T: EdgeType,
// {
//   type Neighbors = graphmap::Neighbors<'a, N, T>;
//   fn neighbors(self, n: Self::NodeId) -> Self::Neighbors {
//     self.neighbors(n)
//   }
// }

trait_template! {
    /// Access to the neighbors of each node
    ///
    /// The `Neighbors` are depending on the `Graph`'s `EdgeType`:
    ///
    /// - `Directed`: All targets of `Edge`s from `a`.
    /// - `Undirected`: All other endpoints of `Edge`s connected to `a`.
    pub trait IntoNeighbors : GraphRef {
        @section type
        type Neighbors: Iterator<Item=Self::NodeId>;

        @section self
        /// Return an iterator of the neighbors of node `a`.
        fn neighbors(self: Self, a: Self::NodeId) -> Self::Neighbors;
    }
}

// crate::IntoNeighbors! {delegate_impl []}

trait_template! {
    /// Access to the neighbors of each node, through incoming or outgoing edges.
    ///
    /// Depending on the graph's edge type, the neighbors of a given directionlity
    /// are:
    ///
    /// - `Directed`, `Outgoing`: All target of `Edge`s from `a`.
    /// - `Directed`, `Incoming`: All sources of `Edge`s to `a`.
    /// - `Undirected`: All other endpoints of edge connected to `a`.
    pub trait IntoNeighborsDirected: IntoNeighbors {
        @section type
        type NeighborsDirected: Iterator<Item=Self::NodeId>;

        @section self
        fn neighbors_directed(self, n: Self::NodeId, d: Direction) -> Self::NeighborsDirected;
    }
}

impl<'a, N, E: 'a, T, Idx> IntoNeighbors for &'a Graph<N, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
  type Neighbors = neighbor::Neighbors<'a, E, Idx>;

  fn neighbors(self, n: NodeIndex<Idx>) -> neighbor::Neighbors<'a, E, Idx> {
    Graph::neighbors(self, n)
  }
}

impl<'a, N, E: 'a, T, Idx> IntoNeighborsDirected for &'a Graph<N, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
  type NeighborsDirected = neighbor::Neighbors<'a, E, Idx>;

  fn neighbors_directed(
    self,
    n: NodeIndex<Idx>,
    d: Direction,
  ) -> neighbor::Neighbors<'a, E, Idx> {
    Graph::neighbors_directed(self, n, d)
  }
}

// #[cfg(feature = "stable_graph")]
// impl<'a, N, E: 'a, T, Idx> IntoNeighborsDirected
//   for &'a StableGraph<N, E, T, Idx>
// where
//   T: EdgeType,
//   Idx: Index,
// {
//   type NeighborsDirected = stable_graph::Neighbors<'a, E, Idx>;
//
//   fn neighbors_directed(
//     self,
//     n: NodeIndex<Idx>,
//     d: Direction,
//   ) -> Self::NeighborsDirected {
//     StableGrap::neighbors_directed(self, n, d)
//   }
// }
//
// #[cfg(feature = "graphmap")]
// impl<'a, N: 'a, E, T> IntoNeighborsDirected for &'a GraphMap<N, E, T>
// where
//   N: Copy + Ord + Hash,
//   T: EdgeType,
// {
//   type NeighborsDirected = graphmap::NeighborsDirected<'a, N, T>;
//
//   fn neighbors_directed(self, n: N, dir: Direction) -> Self::NeighborsDirected {
//     self.neighbors_directed(n, dir)
//   }
// }

trait_template! {
  /// Access to the edges of each node.
  ///
  /// The edges are depending on the graph's edge type:
  ///
  /// - `Directed`: All edges from `a`.
  /// - `Undirected`: All edges connected to `a`.
  ///
  /// This is an extended version of the trait `IntoNeighbors`; the former
  /// only iterates over the target node identifiers, while this trait
  /// yeilds edge references (trait `[EdgeRef`]`).
  ///
  /// `[EdgeRef]`: trait.EdgeRef.html
  pub trait IntoEdges: IntoEdgeIterator + IntoNeighbors {
    @section type
    type Edges: Iterator<Item=Self::EdgeRef>;

    @section self
    fn edges(self, a: Self::NodeId) -> Self::Edges;
  }
}

// crate::IntoEdges! {delegate_impl []}

trait_template! {
  /// Access to all edges of each node, in the specified direction.
  ///
  /// The edges are, depending on the direction and the graph's edge type.
  ///
  /// - `Directed`, `Outgoing`: All edges from `a`.
  /// - `Directed`, `Incoming`: All edges to `a`.
  /// - `Undirected`, `Outgoing`: All edges connected to `a` with `a` being the source of each
  /// edge.
  /// - `Undirected`, `Incoming`: All edges connected to `a` with `a` being the taget of each edge.
  ///
  /// This is an extended version of the trait `IntoNeighborsDirected`; the former only iterates
  /// over the target node identifiers, while this trait yeilds `[EdgeRef]`
  ///
  /// `[EdgeRef]`: trait.EdgeRef.html
  pub trait IntoEdgesDirected: IntoEdges + IntoNeighborsDirected {
    @section type
    type EdgesDirected: Iterator<Item=Self::EdgeRef>;

    @section self
    fn edges_directed(self, a: Self::NodeId, dir: Direction) -> Self::EdgesDirected;
  }
}

// crate::IntoEdgesDirected! {delegate_impl []}

trait_template! {
  /// Access to the sequence of Graph's `NodeId`s.
  pub trait IntoNodeIdentifiers: GraphRef {
    @section type
    type NodeIdentifiers: Iterator<Item=Self::NodeId>;

    @section self
    fn node_identifiers(self) -> Self::NodeIdentifiers;
  }
}

// crate::IntoNodeIdentifiers! {delegate_impl []}

impl<'a, N, E: 'a, T, Idx> IntoNodeIdentifiers for &'a Graph<N, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
  type NodeIdentifiers = NodeIndices<Idx>;

  fn node_identifiers(self) -> NodeIndices<Idx> {
    Graph::node_indices(self)
  }
}

impl<N, E, T, Idx> NodeCount for Graph<N, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
  fn node_count(&self) -> usize {
    self.node_count()
  }
}

// #[cfg(feature = "stable_graph")]
// impl<'a, N, E: 'a, T, Idx> IntoNodeIdentifiers for &'a StableGraph<N, E, T, Idx>
// where
//   T: EdgeType,
//   Idx: Index,
// {
//   type NodeIdentifiers = stable_graph::NodeIndices<'a, N, Idx>;
//
//   fn node_identifiers(self) -> Self::NodeIdentifiers {
//     StableGraph::node_indices(self)
//   }
// }
//
// #[cfg(feature = "stable_graph")]
// impl<N, E, T, Idx> NodeCount for StableGraph<N, E, T, Idx>
// where
//   T: EdgeType,
//   Idx: Index,
// {
//   fn node_count(&self) -> usize {
//     self.node_count()
//   }
// }

// crate::IntoNeighborsDirected! {delegate_impl []}

trait_template! {
  /// Define associated data for nodes and edges
  pub trait Data : GraphBase {
    @section type
    type NodeWeight;
    type EdgeWeight;
  }
}

crate::Data! {delegate_impl []}
crate::Data! {delegate_impl [['a, G], G, &'a mut G, deref]}

impl<N, E, T, Idx> Data for Graph<N, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
  type NodeWeight = N;
  type EdgeWeight = E;
}

/// An `Edge` refeence.
///
/// `Edge` references are used by traits `IntoEdges` and `IntoEdgeRef`.
pub trait EdgeReference: Copy {
  type NodeId;
  type EdgeId;
  type Weight;

  /// The source node of the `Edge`.
  fn source(&self) -> Self::NodeId;

  /// The target node of the `Edge`.
  fn target(&self) -> Self::NodeId;

  /// A reference to the weight of the `Edge`.
  fn weight(&self) -> &Self::Weight;

  /// The `Edge`'s identifier.
  fn id(&self) -> Self::EdgeId;
}

impl<'a, N, E> EdgeReference for (N, N, &'a E)
where
  N: Copy,
{
  type NodeId = N;
  type EdgeId = (N, N);
  type Weight = E;

  fn source(&self) -> Self::NodeId {
    self.0
  }

  fn target(&self) -> Self::NodeId {
    self.1
  }

  fn weight(&self) -> &Self::Weight {
    self.2
  }

  fn id(&self) -> Self::EdgeId {
    (self.0, self.1)
  }
}

/// A `Node` reference.
pub trait NodeReference: Copy {
  type NodeId;
  type Weight;

  /// The `Node`'s identifier.
  fn id(&self) -> Self::NodeId;

  /// A reference to the weight of the `Node`.
  fn weight(&self) -> &Self::Weight;
}

trait_template! {
  /// Access to the sequence of the Graph's nodes.
  pub trait IntoNodeIterator: Data + IntoNodeIdentifiers {
    @section type
    type NodeRef: NodeReference<NodeId=Self::NodeId, Weight=Self::NodeWeight>;
    type NodeIterator: Iterator<Item=Self::NodeRef>;

    @section self
    fn node_iterator(self) -> Self::NodeIterator;
  }
}

// crate::IntoNodeIterator! {delegate_impl []}

impl<Id: Copy> NodeReference for (Id, ()) {
  type NodeId = Id;
  type Weight = ();

  fn id(&self) -> Self::NodeId {
    self.0
  }

  fn weight(&self) -> &Self::Weight {
    static DUMMY: () = ();
    &DUMMY
  }
}

impl<'a, Id: Copy, W> NodeReference for (Id, &'a W) {
  type NodeId = Id;
  type Weight = W;

  fn id(&self) -> Self::NodeId {
    self.0
  }

  fn weight(&self) -> &Self::Weight {
    self.1
  }
}

trait_template! {
  /// Access to the sequence of Graph's edges.
  pub trait IntoEdgeIterator: Data + GraphRef {
    @section type
    type EdgeRef: EdgeReference<NodeId=Self::NodeId, EdgeId=Self::EdgeId,
                                Weight=Self::EdgeWeight>;
    type EdgeIterator: Iterator<Item=Self::EdgeRef>;

    @section self
    fn edge_iterator(self) -> Self::EdgeIterator;
  }
}

// crate::IntoEdgeIterator! {delegate_impl []}

// #[cfg(feature = "graphmap")]
// impl<N, E, T> Data for GraphMap<N, E, T>
// where
//   N: Copy + PartialEq,
//   T: EdgeType,
// {
//   type NodeWeight = N;
//   type EdgeWeight = E;
// }

trait_template! {
  /// Edge kind property (directed or undirected edges).
  pub trait GraphProp: GraphBase {
    @section type
    /// The kind edges in the graph.
    type EdgeType: EdgeType;

    @section nodelegate
    fn is_directed(&self) -> bool {
      <Self::EdgeType>::is_directed()
    }
  }
}

crate::GraphProp! {delegate_impl []}

impl<N, E, T, Idx> GraphProp for Graph<N, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
  type EdgeType = T;
}

// #[cfg(feature = "stable_graph")]
// impl<N, E, T, Idx> GraphProp for StableGraph<N, E, T, Idx>
// where
//   T: EdgeType,
//   Idx: Index,
// {
//   type EdgeType = T;
// }

// #[cfg(feature = "graphmap")]
// impl<N, E, T> GraphProp for GraphMap<N, E, T>
// where
//   N: NodeTrait,
//   T: EdgeType,
// {
//   type EdgeType = T;
// }

// impl<'a, N: 'a, E: 'a, T, Idx> IntoEdgeIterator for &'a Graph<N, E, T, Idx>
// where
//   T: EdgeType,
//   Idx: Index,
// {
//   type EdgeRef = edge::EdgeRef<'a, E, Idx>;
//   type EdgeIterator = EdgeIterator<'a, E, Idx>;
//
//   fn edge_iterator(self) -> Self::EdgeIterator {
//     (*self).edge_iterator()
//   }
// }

impl<'a, N: 'a, E: 'a, T, Idx> IntoEdgeIterator for &'a Graph<N, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
  type EdgeRef = edge::EdgeRef<'a, E, Idx>;
  type EdgeIterator = EdgeIterator<'a, E, Idx>;

  fn edge_iterator(self) -> Self::EdgeIterator {
    (*self).edge_iterator()
  }
}

impl<'a, Idx, E> EdgeReference for edge::EdgeRef<'a, E, Idx>
where
  Idx: Index,
{
  type NodeId = NodeIndex<Idx>;
  type EdgeId = EdgeIndex<Idx>;
  type Weight = E;

  fn source(&self) -> Self::NodeId {
    self.node[0]
  }

  fn target(&self) -> Self::NodeId {
    self.node[1]
  }

  fn weight(&self) -> &Self::Weight {
    self.weight
  }

  fn id(&self) -> Self::EdgeId {
    self.index
  }
}

trait_template! {
  /// The graph's `NodeId`s map to indices.
  pub trait NodeIndexable: GraphBase {
    @section self
    /// Return an upper bound of the node indices in the graph
    /// (suitable for the size of a bitmap).
    fn node_bound(self: &Self) -> usize;
    /// Convert `a` to an integer index.
    fn to_index(self: &Self, a: Self::NodeId) -> usize;
    /// Convert `i` to a node index.
    fn from_index(self: &Self, i: usize) -> Self::NodeId;
  }
}

// crate::NodeIndexable! {delegate_impl []}

trait_template! {
  /// The graph's `NodeId`s map to indices.
  pub trait EdgeIndexable : GraphBase {
    @section self
    /// Return an upper bound of the edge indices in the graph
    /// (suitable for the size of a bitmap).
    fn edge_bound(self: &Self) -> usize;
    /// Convert `a` to an integer index.
    fn to_index(self: &Self, a: Self::EdgeId) -> usize;
    /// Convert `i` to an edge index.
    fn from_index(self: &Self, i: usize) -> Self::EdgeId;
  }
}

// crate::EdgeIndexable! {delegate_impl []}

trait_template! {
  /// A graph with known node count.
  pub trait NodeCount: GraphBase {
    @section self

      fn node_count(self: &Self) -> usize;
  }
}

// crate::NodeCount! {delegate_impl []}

trait_template! {
  /// The graph's `NodeId`s map to indices, in a range without holes.
  ///
  /// The Graph's node identifiers correspond to exactly the indices
  /// `0..self.node_bound()`.
  pub trait NodeCompactIndexable: NodeIndexable + NodeCount { }
}

// crate::NodeCompactIndexable! {delegate_impl []}

impl<N, E, T, Idx> NodeIndexable for Graph<N, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
  #[inline]
  fn node_bound(&self) -> usize {
    self.node_count()
  }

  #[inline]
  fn to_index(&self, idx: NodeIndex<Idx>) -> usize {
    idx.index()
  }

  #[inline]
  fn from_index(&self, idx: usize) -> Self::NodeId {
    NodeIndex::new(idx)
  }
}

impl<N, E, T, Idx> NodeCompactIndexable for Graph<N, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
}

/// A mapping for storing the visited status for `NodeId`'s `N`.
pub trait VisitMap<N> {
  /// Mark `a` as visited
  ///
  /// Return **true** if this is the first visit, false otherwise.
  fn visit(&mut self, a: N) -> bool;

  /// Return whether `a` has been visited before.
  fn is_visited(&self, a: N) -> bool;
}

impl<Idx> VisitMap<Idx> for FixedBitSet
where
  Idx: Index,
{
  fn visit(&mut self, a: Idx) -> bool {
    !self.put(a.index())
  }

  fn is_visited(&self, a: Idx) -> bool {
    self.contains(a.index())
  }
}

impl<N, S> VisitMap<N> for HashSet<N, S>
where
  N: Hash + Eq,
  S: BuildHasher,
{
  fn visit(&mut self, a: N) -> bool {
    self.insert(a)
  }

  fn is_visited(&self, a: N) -> bool {
    self.contains(&a)
  }
}

trait_template! {
  /// A graph that can create a map that tracks the visited status of its nodes.
  pub trait Visitable: GraphBase {
    @section type
    /// The associated map type.
    type Map: VisitMap<Self::NodeId>;
    @section self
    /// Create a new visitor map.
    fn visit_map(self: &Self) -> Self::Map;
    /// Reset the visitor map (and resize to new size of graph if needed).
    fn reset_map(self: &Self, map: &mut Self::Map);
  }
}

// create::Visitable! {delegate_impl []}

impl<N, E, T, Idx> GraphBase for Graph<N, E, T, Idx>
where
  Idx: Index,
{
  type NodeId = NodeIndex<Idx>;
  type EdgeId = EdgeIndex<Idx>;
}

impl<N, E, T, Idx> Visitable for Graph<N, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
  type Map = FixedBitSet;
  fn visit_map(&self) -> FixedBitSet {
    FixedBitSet::with_capacity(self.node_count())
  }

  fn reset_map(&self, map: &mut Self::Map) {
    map.clear();
    map.grow(self.node_count());
  }
}

//
// #[cfg(feature = "stable_graph")]
// impl<N, E, T, Idx> GraphBase for StableGraph<N, E, T, Idx>
// where
//   Idx: Index,
// {
//   type NodeId = NodeIndex<Idx>;
//   type EdgeId = EdgeIndex<Idx>;
// }
//
// #[cfg(feature = "stable_graph")]
// impl<N, E, T, Idx> Visitable for StableGraph<N, E, T, Idx>
// where
//   T: EdgeType,
//   Idx: Index,
// {
//   type Map = FixedBitSet;
//
//   fn visit_map(&self) -> FixedBitSet {
//     FixedBitSet::with_capacity(self.node_bound())
//   }
//
//   fn reset_map(&self, map: &mut Self::Map) {
//     map.clear();
//     map.grow(self.node_bound());
//   }
// }
//
// #[cfg(feature = "stable_graph")]
// impl<N, E, T, Idx> Data for StableGraph<N, E, T, Idx>
// where
//   T: EdgeType,
//   Idx: Index,
// {
//   type NodeWeight = N;
//   type EdgeWeight = E;
// }
//
// #[cfg(feature = "graphmap")]
// impl<N, E, T> GraphBase for GraphMap<N, E, T>
// where
//   N: Copy + PartialEq,
// {
//   type NodeId = N;
//   type EdgeId = (N, N);
// }
//
// #[cfg(feature = "graphmap")]
// impl<N, E, T> Visitable for GraphMap<N, E, T>
// where
//   N: Copy + Ord + Hash,
//   T: EdgeType,
// {
//   type Map = HashSet<N>;
//
//   fn visit_map(&self) -> HashSet<N> {
//     HashSet::with_capacity(self.node_count())
//   }
//
//   fn reset_map(&self, map: &mut Self::Map) {
//     map.clear();
//   }
// }

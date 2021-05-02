use crate::graph::{
  // edge::{Edge, EdgeRef, EdgeType},
  // index::{EdgeIndex, Index, NodeIndex},
  direction::Direction,
  Frozen,
};
use crate::{delegate_impl, trait_template};

pub mod format;
pub mod util;

// EdgeIter - EdgeRef
// EdgeIterator - EdgeReferences
// EdgeRef - EdgeReference
//

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

impl<'a, G> GraphBase for Frozen<'a, G>
where
  G: GraphBase,
{
  type NodeId = G::NodeId;
  type EdgeId = G::EdgeId;
}

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
        fn nieghbors(self: Self, a: Self::NodeId) -> Self::Neighbors;
    }
}

crate::IntoNeighbors! {delegate_impl []}

trait_template! {
    pub trait IntoNeighborsDirected: IntoNeighbors {
        @section type
        type NeighborsDirected: Iterator<Item=Self::NodeId>;
        @section self
        fn neighbors_directed(self, n: Self::NodeId, d: Direction) -> Self::NeighborsDirected;
    }
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

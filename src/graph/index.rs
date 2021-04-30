#![allow(dead_code)]

use std::{fmt, hash::Hash};

use super::{edge::EdgeType, Graph};

/// Graph's default index.
///
/// `u32` is the defaul index because it strikes a balance between size and
/// performace. This can always be changed to suitable values during performance
/// testing.
pub type DefaultIdx = u32;

/// Trait for node and ege indices.
pub trait Index: Copy + Default + Hash + Ord + fmt::Debug + 'static {
  fn new(x: usize) -> Self;
  fn index(&self) -> usize;
  fn max() -> Self;
}

// --- usize ---
impl Index for usize {
  #[inline(always)]
  fn new(x: usize) -> Self {
    x
  }

  #[inline(always)]
  fn index(&self) -> Self {
    *self
  }

  #[inline(always)]
  fn max() -> Self {
    ::std::usize::MAX
  }
}

// --- u8 ---
impl Index for u8 {
  #[inline(always)]
  fn new(x: usize) -> Self {
    x as u8
  }

  #[inline(always)]
  fn index(&self) -> usize {
    *self as usize
  }

  #[inline(always)]
  fn max() -> Self {
    ::std::u8::MAX
  }
}

// --- u16 ---
impl Index for u16 {
  #[inline(always)]
  fn new(x: usize) -> Self {
    x as u16
  }

  #[inline(always)]
  fn index(&self) -> usize {
    *self as usize
  }

  #[inline(always)]
  fn max() -> Self {
    ::std::u16::MAX
  }
}

// --- u32 ---
impl Index for u32 {
  #[inline(always)]
  fn new(x: usize) -> Self {
    x as u32
  }

  #[inline(always)]
  fn index(&self) -> usize {
    *self as usize
  }

  #[inline(always)]
  fn max() -> Self {
    ::std::u32::MAX
  }
}

/// NodeIndex - Index type for [`Node`]
///
/// [`Node`]: struct.Node
#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct NodeIndex<Idx = DefaultIdx>(Idx);

impl<Idx: Index> NodeIndex<Idx> {
  #[inline]
  pub fn new(x: usize) -> Self {
    NodeIndex(Index::new(x))
  }

  #[inline]
  pub fn index(self) -> usize {
    self.0.index()
  }

  #[inline]
  pub fn end() -> Self {
    NodeIndex(Index::max())
  }

  fn into_edge(self) -> EdgeIndex<Idx> {
    EdgeIndex(self.0)
  }
}

impl<Idx: Index> Index for NodeIndex<Idx> {
  fn index(&self) -> usize {
    self.0.index()
  }

  fn new(x: usize) -> Self {
    NodeIndex::new(x)
  }

  fn max() -> Self {
    NodeIndex(<Idx as Index>::max())
  }
}

// NodeIndex(sg:N<1234>)
impl<Idx: fmt::Debug> fmt::Debug for NodeIndex<Idx> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "NodeIndex(sg:N<{:?}>)", self.0)
  }
}

// sg:N<1234>
impl<Idx: fmt::Debug> fmt::Display for NodeIndex<Idx> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "sg:N<{:?}>", self.0)
  }
}

/// EdgeIndex - Index type for [`Edge`]
///
/// [`Edge`]: struct.Edge
#[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct EdgeIndex<Idx = DefaultIdx>(Idx);

impl<Idx: Index> EdgeIndex<Idx> {
  #[inline]
  pub fn new(x: usize) -> Self {
    EdgeIndex(Index::new(x))
  }

  #[inline]
  pub fn index(self) -> usize {
    self.0.index()
  }

  #[inline]
  pub fn end() -> Self {
    EdgeIndex(Index::max())
  }

  pub(crate) fn into_node(self) -> NodeIndex<Idx> {
    NodeIndex(self.0)
  }
}

impl<Idx: Index> From<Idx> for EdgeIndex<Idx> {
  fn from(index: Idx) -> Self {
    EdgeIndex(index)
  }
}

// EdgeIndex(sg:E<1234>)
impl<Idx: fmt::Debug> fmt::Debug for EdgeIndex<Idx> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "EdgeIndex(sg:E<{:?})", self.0)
  }
}

// sg:E<1234>
impl<Idx: fmt::Debug> fmt::Display for EdgeIndex<Idx> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "sg:E<{:?}>", self.0)
  }
}

/// Index the `Graph` by `NodeIndex` to access node weights.
///
/// **Panics** if the node doesn't exit.
impl<N, E, T, Idx> std::ops::Index<NodeIndex<Idx>> for Graph<N, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
  type Output = N;

  fn index(&self, index: NodeIndex<Idx>) -> &Self::Output {
    &self.nodes[index.index()].weight
  }
}

/// Index the `Graph` by `NodeIndex` to access node weights mutably.
///
/// **Panics** if the node doesn't exist.
impl<N, E, T, Idx> std::ops::IndexMut<NodeIndex<Idx>> for Graph<N, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
  fn index_mut(&mut self, index: NodeIndex<Idx>) -> &mut Self::Output {
    &mut self.nodes[index.index()].weight
  }
}

/// Index the `Graph` by `EdgeIndex` to access node weights.
///
/// **Panics** if the node doesn't exist.
impl<N, E, T, Idx> std::ops::Index<EdgeIndex<Idx>> for Graph<N, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
  type Output = E;

  fn index(&self, index: EdgeIndex<Idx>) -> &Self::Output {
    &self.edges[index.index()].weight
  }
}

/// Index the `Graph` by `EdgeIndex` to access node weights mutably.
///
/// **Panics** if the node doesn't exist.
impl<N, E, T, Idx> std::ops::IndexMut<EdgeIndex<Idx>> for Graph<N, E, T, Idx>
where
  T: EdgeType,
  Idx: Index,
{
  fn index_mut(&mut self, index: EdgeIndex<Idx>) -> &mut Self::Output {
    &mut self.edges[index.index()].weight
  }
}

/// A `GraphIndex` is a node or edge index.
pub trait GraphIndex: Copy {
  #[doc(hidden)]
  fn index(&self) -> usize;
  #[doc(hidden)]
  fn is_node_index() -> bool;
}

impl<Idx: Index> GraphIndex for NodeIndex<Idx> {
  #[inline]
  fn index(&self) -> usize {
    NodeIndex::index(*self)
  }

  #[inline]
  fn is_node_index() -> bool {
    true
  }
}

impl<Idx: Index> GraphIndex for EdgeIndex<Idx> {
  #[inline]
  fn index(&self) -> usize {
    EdgeIndex::index(*self)
  }

  #[inline]
  fn is_node_index() -> bool {
    false
  }
}

/// Create a new [`NodeIndex`].
///
/// [`NodeIndex`]: NodeIndex::new
pub fn node_index<I: Index>(index: usize) -> NodeIndex<I> {
  NodeIndex::new(index)
}

/// Create a new [`EdgeIndex`].
///
/// [`EdgeIndex`]: EdgeIndex::new
pub fn edge_index<I: Index>(index: usize) -> EdgeIndex<I> {
  EdgeIndex::new(index)
}

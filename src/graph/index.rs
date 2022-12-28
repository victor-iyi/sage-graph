#![allow(dead_code)]

use crate::index_t;
use std::{fmt, hash::Hash};

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

index_t!(usize);
index_t!(u8);
index_t!(u16);
index_t!(u32);

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
    write!(f, "EdgeIndex(sg:E<{:?}>)", self.0)
  }
}

// sg:E<1234>
impl<Idx: fmt::Debug> fmt::Display for EdgeIndex<Idx> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "sg:E<{:?}>", self.0)
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

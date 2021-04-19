mod error;
mod graph;
mod iterator;
mod macros;

pub use crate::error::{Error, ErrorKind, Result};
pub use crate::graph::{
  edge::{Edge, EdgeType},
  node::Node,
  DiGraph, Graph, UnDiGraph,
};

#[allow(unused_imports)]
pub(crate) use iterator::format;

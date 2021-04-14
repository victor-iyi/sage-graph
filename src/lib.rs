mod error;
mod graph;
mod macros;

pub use crate::error::{Error, ErrorKind, Result};
pub use crate::graph::{
  edge::{Edge, EdgeDirection},
  node::Node,
  DiGraph, Graph, UnDiGraph,
};

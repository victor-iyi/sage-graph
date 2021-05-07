#![allow(macro_expanded_macro_exports_accessed_by_absolute_paths)]

mod error;
mod graph;
mod iterator;
mod macros;
mod util;

pub use crate::error::{Error, ErrorKind, Result};
pub use crate::graph::{
  direction::{Directed, Direction, Undirected, DIRECTIONS},
  edge::{
    Edge, EdgeIndices, EdgeIterator, EdgeRef, EdgeType, EdgeWeightsMut, Edges,
    EdgesConnecting,
  },
  index::{EdgeIndex, Index, NodeIndex},
  neighbor::{Neighbors, Walker},
  node::Node,
  DiGraph, Graph, UnDiGraph,
};

pub use iterator::{EdgeReference, GraphBase, GraphRef, NodeReference};

#[allow(unused_imports)]
pub(crate) use util::{enumerate, rev, zip};

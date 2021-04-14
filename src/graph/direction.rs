#![allow(dead_code)]

use std::hash::Hash;

use crate::copyclone;

/// [`Direction`] which Graph's [`Edge`] flows.
///
/// [`Direction`]: enum.Direction
/// [`Edge`]: struct.Edge
pub const DIRECTIONS: [Direction; 2] =
  [Direction::Outgoing, Direction::Incoming];

/// Graph's [`Edge`] direction.
///
/// [`Edge`]: struct.Edge
#[derive(Debug, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Direction {
  /// An `Outgoing` edge is an outward edge *from* the current node.
  Outgoing = 0,
  /// An `Incoming` edge is an inbound edge *to* the current node.
  Incoming = 1,
}

copyclone!(Direction);

impl Direction {
  /// Returns the opposite `Direction`.
  #[inline]
  pub fn opposite(self) -> Direction {
    match self {
      Direction::Outgoing => Direction::Incoming,
      Direction::Incoming => Direction::Outgoing,
    }
  }

  /// Return `0` for `Outgoing` and `1` for `Incoming`.
  #[inline]
  pub fn index(self) -> usize {
    (self as usize) & 0x1
  }
}

/// Marker for a directed graph.
#[derive(Copy, Debug)]
pub enum Directed {}
copyclone!(Directed);

/// Marker for a directed graph.
#[derive(Copy, Debug)]
pub enum Undirected {}
copyclone!(Undirected);

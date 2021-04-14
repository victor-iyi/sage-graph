use std::{fmt, str::FromStr};

/// Sage Graph `Result` type.
pub type Result<T, E = Error> = std::result::Result<T, E>;

/// [`ErrorKind`] expresses different types of errors that could occur upon error
/// handling with [`Error`].
///
/// [`Error`]: struct.Error
/// [`ErrorKind`]: enum.ErrorKind
#[derive(Debug, PartialEq)]
pub enum ErrorKind {
  /// Error caused by traversing the graph.
  TraversalError,

  /// Unknown ID.
  UnknownId,

  /// Duplicate ID.
  DuplicateId,

  /// Missing edge.
  MissingEdge,

  /// Duplicate edge.
  DuplicateEdge,

  /// Generic error kind.
  Error,
}

/// Base error type.
pub struct Error {
  err: ErrorImpl,
}

impl Error {
  pub fn new(kind: ErrorKind, message: &str) -> Self {
    Self {
      err: ErrorImpl::new(kind, message),
    }
  }

  // pub fn from_str(message: &str) -> Self {
  //   Error::new(ErrorKind::Error, message)
  // }
}

impl Error {
  /// Return the `ErrorKind` of this error.
  pub fn kind(&self) -> &ErrorKind {
    &self.err.kind
  }

  /// Return the message or clue of the error.
  pub fn message(&self) -> &str {
    &self.err.msg
  }
}

impl FromStr for Error {
  type Err = Error;

  #[doc(hidden)]
  fn from_str(message: &str) -> Result<Error> {
    Ok(Error::new(ErrorKind::Error, message))
  }
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.err.msg)
  }
}

impl fmt::Debug for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("Error")
      .field("kind", &self.err.kind)
      .field("message", &self.err.msg)
      .finish()
  }
}

struct ErrorImpl {
  kind: ErrorKind,
  msg: String,
}

impl ErrorImpl {
  fn new(kind: ErrorKind, msg: &str) -> Self {
    Self {
      kind,
      msg: msg.to_string(),
    }
  }
}

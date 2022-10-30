/// Clone types.
#[macro_export]
macro_rules! copyclone {
  ($name:ident) => {
    impl Clone for $name {
      #[inline]
      fn clone(&self) -> Self {
        *self
      }
    }
  };
}

/// Clone fields.
#[macro_export]
macro_rules! clone_fields {
  ($name:ident, $($field:ident),+ $(,)*) => {
    fn clone(&self) -> Self {
      $name {
        // $field: self.$field
        $(
          $field : self.$field.clone()
        ),*
      }
    }
  };
}

/// Implement `Index` for primitive types.
#[macro_export]
macro_rules! index_t {
  ($ty:ty) => {
    // -- $ty --
    impl $crate::Index for $ty {
      #[inline(always)]
      fn new(x: usize) -> Self {
        x as $ty
      }

      #[inline(always)]
      fn index(&self) -> usize {
        *self as usize
      }

      #[inline(always)]
      fn max() -> Self {
        <$ty>::MAX
      }
    }
  }
}

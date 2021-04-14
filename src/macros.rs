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

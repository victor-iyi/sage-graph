/// Implement a trait by delegation. By default as if we are delegating
/// from `&G` to `G`.
#[macro_export]
macro_rules! delegate_impl {
  ([] $($rest:tt)*) => {
    delegate_impl! { [['a, G], G, &'a G, deref] $($rest)* }
  };
  ([[$($param:tt)*], $self_type:ident, $self_wrap:ty, $self_map:ident]
   pub trait $name:ident $(: $sup:ident)* $(+ $more_sup:ident)* {

     // "Escaped" associated types. Stripped before making the `trait`
     // itself, but forwarded when delegating impls.
     $(
       @escape [type $assoc_name_ext:ident]
       // Associated types. Forwarded.
     )*
     $(
        @section type
        $(
          $(#[$_assoc_attr:meta])*
          type $assoc_name:ident $(: $assoc_bound:ty)*;
        )*
      )*
      // Methods. Forwarded. Using $self_ma!(self) around the self argument.
      // Methods must use reciever `self` or explicit type like `self: &Self`
      // &self and &mut self are _not_ supported.
      $(
        @section self
        $(
          $(#[$_method_attr:meta])*
          fn $method_name:ident(self $(: $self_selftype:ty)* $(,$marg:ident : $marg_ty:ty)*) $(-> $mret:ty)?;
        )+
      )*
      // Arbitrary tail that is ignored when forwarding.
      $(
        @section nodelegate
        $($tail:tt)*
      )*
   }) => {
     impl<$($param)*> $name for $self_wrap where $self_type: $name {
       $(
         $(
           type $assoc_name = $self_type::$assoc_name;
         )*
       )*
       $(
         type $assoc_name_ext = $self_type::$assoc_name_ext;
       )*
       $(
         $(
           fn $method_name(self $(: $self_selftype)* $(,$marg: $marg_ty)*) $(-> $mret)? {
               $self_map!(self).$method_name($($marg),*)
           }
         )*
       )*
     }
  }
}

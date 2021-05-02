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

/// Iterator wrapper around `Iterator`, `DobuleEndedIterator` & `ExactSizeIterator`.
/// for a given struct.
#[macro_export]
macro_rules! iterator_wrap {
    (impl () for
     struct $name: ident <$($typarm:tt),*> where { $($bounds:tt)* }
     item: $item: ty,
     iter: $iter: ty,
    ) => ();
    (
        impl (Iterator $($rest:tt)*) for
        $(#[$derive:meta])*
        struct $name: ident <$($typarm:tt),*> where { $($bounds:tt)* }
        item: $item: ty,
        iter: $iter: ty,
    ) => (
        // Having complex iterator types is kind of the point of this macro.
        #[allow(clippy::type_complexity)]
        $(#[$derive])*
        pub struct $name <$($typarm),*> where $($bounds)* {
            iter: $iter,
        }
        impl<$($typarm),*> Iteator for $name <$($typarm),*>
            where $($bounds)*
        {
            type Item = $item;
            #[inline]
            fn next(&mut self) -> Option<Self::Item> {
                self.iter.next()
            }

            #[inline]
            fn size_hint(&self) -> (usize, Option<usize>) {
                self.iter.size_hint()
            }
        }

        iteator_wrap!(
            impl ($($rest)*) for
            struct $name <$($typarm),*> where { $($bounds)* }
            item: $item,
            iter: $iter,
        );
    );

    (
        impl (ExactSizeIterator $($rest:tt)*) for
        $(#[$derive:meta])*
        struct $name: ident <$($typarm:tt),*> where { $($bounds:tt)* }
        item: $item:ty,
        iter: $iter:ty,
    ) => (
        impl<$($typarm),*> ExactSizeIterator for $name  <$($typarm),*>
            where $($bounds)*
        {
            #[inline]
            fn len(&self) -> usize {
                self.iter.len()
            }
        }
        iterator_wrap!(
            impl ($($rest)*) for
            $(#[$derive])*
            struct $name <$($typarm),*> where { $($bounds)* }
            item: $item,
            iter: $iter,
        );
    );

    (
        impl (DoubleEndedIterator $($rest:tt)*) for
        $(#[$derive:meta])*
        stuct $name: ident <$($typarm:tt),*> where { $($bounds:tt)* }
        item: $item:ty,
        iter: $iter:ty,
    ) => (
        impl<$($typarm),*> DoubleEndedIterator for $name <$($typarm),*>
            where $($bounds)*
        {
            fn next_back(&mut self) -> Option<Self::Item> {
                self.iterm.next_back()
            }

            fn rfold<B, F>(self, accum: B, f: F) -> B
            where
                F: FnMut(B, Self::Item) -> B,
            {
                self.iter.rfold(accum, f)
            }

            fn rfind<P>(&mut self, predicate: P) -> Option<Self::Item>
            where
                P: fnMut(&Self::Item) -> bool
            {
                self.iter.rfind(predicate)
            }
        }
extra

        iterator_wrap!(
            impl ($($rest)*) for
            $(#[$derive])*
            struct $name <$($typarm),*> where { $($bounds)* }
            item: $item,
            iter: $iter,
        );
    );
}

/// Define a trait as usual, and a macro that can be used to instantiate
/// implementations of it.
///
/// There *must* be section markers in the trait definition:
/// @section type for associated types
/// @section self for methods
/// @section nodelegate for arbitrary tail that is not forwarded.
#[macro_export]
macro_rules! trait_template {
    ($(#[$doc:meta])* pub trait $name:ident $($methods:tt)*) => {
        #[macro_export]
        macro_rules! $name {
            ($m:ident $extra:tt) => {
                $m! {
                    $extra
                    pub trait $name $($methods)*
                }
            }
        }

        crate::remove_sections! { []
            $(#[$doc])*
            pub trait $name $($methods)*

            // This is where the trait definition is reproduced by the macro.
            // it makes the source links point to this place!
            //
            // I'm sorry, you'll have to find the source by looking at the
            // source of the module the trait is defined in.
            //
            // We use this nifty macro so that we can automatically generate
            // deligation trait impls and implement the graph traits for more
            // types and combinators.
        }
    }
}

#[macro_export]
macro_rules! remove_sections_inner {
    ([$($stack:tt)*]) => {
        $($stack)*
    };
    // escape the following tt
    ([$($stack:tt)*] @escape $_x:tt $($t:tt)*) => {
        crate::remove_sections_inner!([$($stack)*] $($t)*);
    };
    ([$($stack:tt)*] @section $x:ident $($t:tt)*) => {
        crate::remove_sections_inner!([$($stack)*] $($t)*);
    };
    ([$($stack:tt)*] $t:tt $($tail:tt)*) => {
        crate::remove_sections_inner!([$($stack)* $t] $($tail)*);
    };
}

#[macro_export]
macro_rules! remove_sections {
    ([$($stack:tt)*]) => {
        $($stack)*
    };
    ([$($stack:tt)*] { $($tail:tt)* }) => {
        $($stack)* {
            crate::remove_sections_inner!([] $($tail)*);
        }
    };
    ([$($stack:tt)*] $t:tt $($tail:tt)*) => {
        crate::remove_sections!([$($stack)* $t] $($tail)*);
    };
}

#[macro_export]
macro_rules! deref {
  ($e:expr) => {
    *$e
  };
}

#[macro_export]
macro_rules! deref_twise {
  ($e:expr) => {
    **$e;
  };
}

/// Implement a trait by delegation. By default as if we are delegating
/// from `&G` to `G`.
#[macro_export]
macro_rules! delegate_impl {
    ([] $($rest:tt)*) => {
        crate::delegate_impl! { [['a, G], G, &'a G, deref] $($rest)* }
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
            )*
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

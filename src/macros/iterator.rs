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

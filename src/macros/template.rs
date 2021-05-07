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

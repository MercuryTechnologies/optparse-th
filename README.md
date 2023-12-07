# `optparse-th`

This package is designed to provide a `TemplateHaskell` variant of `optparse-generic`.
The `Generic` instance for very large sum types becomes an extremely onerous thing to compile.
`TemplateHaskell` is able to generate the required code very quickly, which is much more efficient than going through a `Generic` pass that must be re-derived every time the module recompiles.


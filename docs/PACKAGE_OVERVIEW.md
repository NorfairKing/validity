# High-level package overview

Values of custom types usually have invariants imposed upon them.
The `validity` package provides the `Validity` type class, which makes these invariants explicit by providing a function to check whether the invariants hold.

The `validity-*` packages provides the `Validity` instances for commonly-used packages.

Property testing for functions involving types with invariants often requires writing generators that are aware of the validity of the values that they generate.
The `genvalidity` package provides a general framework to define these generators.
The `GenValid` type class provides functions to generate valid values.

The `genvalidity-*` packages provide the `GenValid` instances for commonly-used packages.

Property testing of functions involving types which instantiate `Validity` and `GenValid` can be generalised to highly generic functions.
The `genvalidity-property` and `genvalidity-hspec` provides a large library of combinators that allow for automatic property-test generation.
The `genvalidity-hspec` uses `TypeApplications` as a central part of its UI, but `genvalidity-property` does not require it.
The `genvalidity-hspec-*` packages provide automatic property testing functions for certain commonly-used packages.
The `genvalidity-sydtest*` do the same for sydtest.

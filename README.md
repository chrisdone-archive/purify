# purify

Reproducible builds for PureScript

## Usage

Make a `purify.yaml` file like this:

``` yaml
output-file: index.js
extra-deps:
- repo: https://github.com/purescript/purescript-console.git
  commit: v2.0.0
- repo: https://github.com/purescript/purescript-eff.git
  commit: v2.0.0
- repo: https://github.com/purescript/purescript-prelude.git
  commit: v2.0.0
```

(Modules can be explicitly listed or otherwise every module under
`src/` will be included in compilation.)

Then run `purify` in the directory.

Example output:

```
$ purify
Cloning purescript-console ...
Checking out purescript-console (v2.0.0) ...
Cloning purescript-eff ...
Checking out purescript-eff (v2.0.0) ...
Cloning purescript-prelude ...
Checking out purescript-prelude (v2.0.0) ...
Compiling 32 modules ...
Compiling Main
Compiling Data.Show
Compiling Data.NaturalTransformation
Compiling Data.Boolean
Compiling Control.Semigroupoid
Compiling Control.Category
Compiling Data.Void
Compiling Data.Unit
Compiling Data.Function
Compiling Data.Semiring
Compiling Data.HeytingAlgebra
Compiling Data.Eq
Compiling Data.Semigroup
Compiling Data.Functor
Compiling Data.Ring
Compiling Data.Ordering
Compiling Data.BooleanAlgebra
Compiling Control.Apply
Compiling Data.CommutativeRing
Compiling Data.Ord.Unsafe
Compiling Data.EuclideanRing
Compiling Data.Ord
Compiling Data.Field
Compiling Control.Applicative
Compiling Control.Bind
Compiling Data.Bounded
Compiling Control.Monad
Compiling Control.Monad.Eff
Compiling Prelude
Compiling Control.Monad.Eff.Class
Compiling Control.Monad.Eff.Unsafe
Compiling Control.Monad.Eff.Console
Bundling ...
Output bundled to index.js
$
```

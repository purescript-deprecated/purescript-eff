# DEPRECATED

As of PureScript 0.12 the default type for handling effects is `Effect` from [`purescript-effect`](https://github.com/purescript/purescript-effect). This differs from `Eff` by removing the row of effect types. This decision was made as getting the effect rows to line up was sometimes quite tricky, without providing a great deal of benefit.

There is also [`purescript-run`](https://github.com/natefaubion/purescript-run) now, which uses a similar effect row mechanic but provides true algebraic effect handling.

[The previous releases](https://github.com/purescript-deprecated/purescript-eff/releases) will continue to work for older libraries that still depend on them.

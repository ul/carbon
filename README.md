# Carbon

Yet another ClojureScript VDOM FRP library.

[![Clojars Project](http://clojars.org/carbon/latest-version.svg)](http://clojars.org/carbon)

## Overview

On the surface, Carbon is similar to Reagent but has a number of implementation
details taken differently. First of all, it is not React
but [inferno](https://github.com/infernojs/inferno) based, which is
lightweight and fast React alternative. Second main difference is reactive part
implementation. Carbon's reactive `cell` (cf. Reagent's `atom`) and `rx` (cf.
Reagent's `reaction`) are modelled
after [Javelin](https://github.com/hoplon/javelin)
and [freactive.core](https://github.com/aaronc/freactive.core) taking the best
from both. Carbon's reactive expressions provide glitch-free changes
propagation, fully dynamic dependency graph and they are GC-friendly.

## Contribution

... is welcomed and very much appreciated! Feel free to ping me with questions
and to make PRs.

## License

Copyright © 2015–2017 Ruslan Prokopchuk

Distributed under the Eclipse Public License either version 1.0 or (at your
option) any later version.

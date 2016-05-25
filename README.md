# Carbon

Yet another ClojureScript VDOM FRP library.

[![Clojars Project](http://clojars.org/carbon/latest-version.svg)](http://clojars.org/carbon)

## Overview

On the surface, Carbon is similar to Reagent but has a number of implementation
details taken differently. First of all, it is not React but
[virtual-dom](https://github.com/Matt-Esch/virtual-dom) based.
virtual-dom is not only more lightweight and fast, it allows us to perform
unique optimization: Carbon components are mounted to the parent's DOM node as
[Widgets](https://github.com/Matt-Esch/virtual-dom/blob/master/docs/widget.md)
with their own standalone virtual trees. It means that when you trigger
component update you don't touch its ancestors at all.
Second main difference is reactive part implementation.
Carbon's reactive `cell` (cf. Reagent's `atom`) and
`rx` (cf. Reagent's `reaction`) are modelled after
[Javelin](https://github.com/hoplon/javelin) and
[freactive.core](https://github.com/aaronc/freactive.core) taking the best from both.
Carbon's reactive expressions provide glitch-free changes propagation,
fully dynamic dependency graph and they are GC-friendly.

## Contribution

... is welcomed and very much appreciated! Feel free to ping me with questions and to make PRs.

## License

`carbon.vdom` based on the [vdom](https://github.com/exupero/vdom) by Eric Shull [LICENSE](https://github.com/exupero/vdom/blob/master/LICENSE)

Copyright © 2015 Ruslan Prokopchuk

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.

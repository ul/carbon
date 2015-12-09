# Carbon

Yet another ClojureScript VDOM FRP library.

[![Clojars Project](http://clojars.org/carbon/latest-version.svg)](http://clojars.org/carbon)

## Overview

Coming soon!

## Setup

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL. 

## License

`carbon.vdom` based on the [vdom](https://github.com/exupero/vdom) by Eric Shull [LICENSE](https://github.com/exupero/vdom/blob/master/LICENSE)

Copyright © 2015 Ruslan Prokopchuk

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.

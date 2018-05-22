# drty

A small imperative-looking language for experiementing with lexical scope.

## Usage

Pass a string to `drty.evaluator/evaluate-str`!

Functions are called with fn(x y) notation, mapping to Clojure fns under the
hood.

New bindings use the form `let x = y`. The only types are numbers and strings.

**You can (only!) open a new scope by placing expressions inside `ctx{ ...
}`**. Assignments will otherwise overwrite names in the current scope.

## License

Copyright © 2018 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

# core.monad

Monad and related protocols for Clojure

## Usage

```clojure
(m-do [x [1 2 3]]
      [y [1 2 3]]
      [:return (* x y)])
```

[![Build Status](https://travis-ci.org/farrellm/core.monad.svg?branch=master)](https://travis-ci.org/farrellm/core.monad)

## License

Copyright © Matthew Farrell

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

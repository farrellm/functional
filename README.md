# core.monad

Monad and related protocols for Clojure

## Usage

```clojure
(m-do [x [1 2 3]]
      [y [1 2 3]]
      [:return (* x y)])

(m-do [x (range 4)
       y (range 4)]
      [:let z (* x y)]
      [:guard (> z 3)]
      [:return z])
```

[![Build Status](https://travis-ci.org/farrellm/core.monad.svg?branch=master)](https://travis-ci.org/farrellm/core.monad)

## License

Copyright © Matthew Farrell

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

# slang

Small language with Lisp-like syntax. Implemented in [Clojure](http://clojure.org).

Usage
=====

Only small subset has been implemented so far. However, you can already play
with the `evals` function :)

```scheme
(use 'slang.core)

(evals '(def ans 42))
;=> 42

(evals '(def second (fun (x y) y)))
;=> #<some-fn-describing-string>

(evals '(second 1 ans))
;=> 42
```

Copyright © 2012 Mnicky ([mnicky.github.com](http://mnicky.github.com))

Distributed under the [MIT license](http://opensource.org/licenses/MIT).

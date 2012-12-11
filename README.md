# slang

Small language with Lisp-like syntax. Implemented in [Clojure](http://clojure.org).

Usage
=====

Only small subset has been implemented so far. However, you can already play
with the `evals` function :)

```clojure
(use 'slang.core)

(evals '(def ans 42))
;=> 42
(evals '(def second (fun (x y) y)))
;=> #<some-fn-describing-string>
(evals '(second 1 ans))
;=> 42

;; or more advanced things like this:
(evals '(def comp1 (fun (f g) (fun (x) (f (g x))))))
(evals '(def partial1 (fun (f arg1) (fun (arg2) (f arg1 arg2)))))
(evals '(def plus7 (partial1 + 7)))
(evals '(def mul10 (partial1 * 10)))
(evals '(def plus7_mul10 (comp1 plus7 mul10)))
(evals '(plus7_mul10 99))
;=> 997
```

Copyright © 2012 Mnicky ([mnicky.github.com](http://mnicky.github.com))

Distributed under the [MIT license](http://opensource.org/licenses/MIT).

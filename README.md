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
;=> #<some-fn-describing-string>cd
(evals '(second "give me" ans))
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

```clojure
;; current local environment can be accessed via the &env variable:
(evals '(do (bind (quote x) 144 &env)
            (lookup (quote x) &env)))
;=> 144
(evals '(exists? (quote x) &env))            ;; executed in the current local environment
;=> true
(evals '(exists? (quote x) (new-env)))       ;; in a new empty local environment
;=> false
(evals '(exists? (quote x) (new-env &env)))  ;; in a new local environment referencing the current one
;=> true
(evals '(do (unbind (quote x) &env)
            (lookup (quote x) &env)))
;=> nil

;; evals itself can be accessed from the language as well:
(evals '(evals (quote ((fun (x)
                         (* 7 x))
                       28))))
;=> 196
(evals '(evals (quote (do (def x 50) x))     ;; executed in a new local environment
               (new-env &env)))
;=> 50
(evals 'x)
;=> nil
(evals '(evals (quote (do (def x 202) x))))  ;; executed in the same environment
;=> 202
(evals 'x)
;=> 202
```

Copyright © 2012 Mnicky ([mnicky.github.com](http://mnicky.github.com))

Distributed under the [MIT license](http://opensource.org/licenses/MIT).

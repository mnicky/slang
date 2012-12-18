# Slang

Small language with Lisp-like syntax. Implemented in [Clojure](http://clojure.org).


Installation
============

* install [leiningen](https://github.com/technomancy/leiningen#leiningen)
* clone this repository: `git clone -b gc https://github.com/mnicky/slang`
* in the cloned repository, run: `lein uberjar`. This will create the _.jar_ file, named _slang-VERSION-standalone.jar_, in the _target/_ directory of the cloned repository.


Usage
=====
Invoking the Slang interpreter:

* run `java -jar slang-VERSION-standalone.jar` to start the Slang repl
* run `java -jar slang-VERSION-standalone.jar FILE` to execute the file with Slang source code


Syntax
======

## Forms

`(quote expr)` - Prevents the evaluation of _expr_.

`(if test then else)` - Ordinary if clause.

`(def name val)` - Binds the value _val_ to the _name_ in the current environment.

`(struct)` - Returns new structure.

`(set structname fieldname val)` - Sets the field _fieldname_ in the structure _structname_ to the _val_ and returns the structure.

`(get structname fieldname)` - Returns the value of the given field in the specified structure.

`(do exprs...)` - Evaluates all the expresions and returns the value of the last one.

`(for (i 1 10) expr)` - For cycle (runs the expression in the new local environment).

`(fun (args...) expr)` - Definition of the function.

`(funcname args...)` - Invocation of the function.


## Literals
 * numbers: `2`, `2.3`
 * strings: `"Hello"`
 * booleans: `true`, `false`
 * non-defined value: `nil`

## Useful functions

 * Math & logical: `+`, `-`, `*`, `/`, `=`, `<`, `>`, `<=`, `>=`, `and`, `or`
 * Lists: `car`, `cdr`, `cons`, `list?`, `symbol?`, `print`
 * Direct environment manipulation: `new-env`, `lookup`, `bind`, `unbind`, `exists?`

<br>
See [core.clj](https://github.com/mnicky/slang/blob/gc/src/slang/core.clj) for more information.

Examples
========

```clojure
;; Variables and functions:
(def ans 42)
;=> 42
(def second (fun (x y) y))
;=> #<some-fn-describing-string>
(second "give me" ans)
;=> 42

;; Structures:
(def person (struct))
;=> {}
(set person name "John Doe")
;=> {name John Doe}
(set person age 42)
;=> {age 42, name John Doe}
(get person age)
;=> 42

;; Flow control:
(if (= 1 2) "This is weird world!" "Everything's ok.")
;=> Everything's ok.
(for (i 1 5) (do (print i) "returned value"))
;1
;2
;3
;4
;5
;=> returned value

;; Higher order functions:
(def comp1 (fun (f g) (fun (x) (f (g x)))))
(def partial1 (fun (f arg1) (fun (arg2) (f arg1 arg2))))
(def plus7 (partial1 + 7))
(def mul10 (partial1 * 10))
(def plus7_mul10 (comp1 plus7 mul10))
(plus7_mul10 99)
;=> 997
```

It is possible to manipulate environments directly from the Slang:

```clojure
;; current local environment can be accessed via the &env variable:
(do (bind (quote x) 144 &env)
          (lookup (quote x) &env))
;=> 144
(exists? (quote x) &env)            ;; executed in the current local environment
;=> true
(exists? (quote x) (new-env))       ;; in a new empty local environment
;=> false
(exists? (quote x) (new-env &env))  ;; in a new local environment referencing the current one
;=> true
(do (unbind (quote x) &env)
            (lookup (quote x) &env))
;=> nil

;; interpreter itself can be accessed from the language as well, via the 'evals' function:
(evals (quote ((fun (x) (* 7 x)) 28)))
;=> 196
(evals (quote (do (def x 50) x))     ;; executed in a new local environment
       (new-env &env))
;=> 50
x
;=> nil
(evals (quote (do (def x 202) x)))  ;; executed in the same environment
;=> 202
x
;=> 202
```

<br>
Copyright © 2012 Mnicky ([mnicky.github.com](http://mnicky.github.com))

Distributed under the [MIT license](http://opensource.org/licenses/MIT).

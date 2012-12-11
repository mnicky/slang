(ns slang.core)

(def third (comp first next next))

(defn new-env
  ""
  ([]
    (new-env [] []))
  ([vars args]
    (new-env vars args nil))
  ([vars args env]
    (atom (merge {:outer-env env} (zipmap vars args)))))

(defn find-binding
  ""
  [sym env]
  (when env
    (let [val (get @env sym :not-found)]
      (if (= :not-found val)
        (recur sym (get :outer-env env))
        val))))

(defn add-binding
  ""
  [sym val env]
  (sym (swap! env assoc sym val)))

(defn add-clojure-fns
  ""
  [env]
  (doseq [fns {'+ +, '- -, '* *, '/ /, 'T true, 'F false}]
    (add-binding (key fns) (val fns) env))
  env)

(def global-env (add-clojure-fns (new-env)))

(defn evals
  ""
  ([exp]
    (evals exp global-env))
  ([exp env]
    (cond
      (symbol? exp)          (find-binding exp env)
      (not (list? exp))      exp
      (= 'quote (first exp)) (second exp)                                                         ;; (quote exp)
      (= 'if (first exp))    (let [[test then else] (rest exp)]                                   ;; (if test then else)
                               (evals (if (evals test env) then else) env))
      (= 'def (first exp))   (add-binding (second exp) (evals (third exp) env) env)               ;; (def name val)
      (= 'do (first exp))    (last (map #(evals % env) (rest exp)))                               ;; (do exp...)
      (= 'fun (first exp))   (fn [& args] (evals (third exp) (new-env (second exp), args, env)))  ;; (fun (vars...) expr)
      :else                  (let [exprs (doall (map #(evals % env) exp))]                        ;; (funcname exprs...)
                               (apply (first exprs) (rest exprs))))))


;FIXME:
; (evals '(def p1 (fun (x) (+ 1 x))))
; (evals '(p1 2))
;=> NullPointerException   clojure.core/apply (core.clj:601)


(comment
; design proposal?

(quote w)

(def s "string")
(def n 10)

(def x (struct (a 20 b 30)))
(def y (struct ()))
(def z (struct))

(def y.a 20)
(def y.b 30)

(do (def a 1)
    (def b 2)
    (+ a b))

(print y.a)

(print (= x.a y.a))
(print (= x y))

(for (i 1 10)
  (do
    (print x.a)
    (def y (struct))))

(def max
  (fn (a b)
    (if (> a b)
      a
      b)))

(max n 20)

)

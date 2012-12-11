(ns slang.core)

(def third (comp first next next))

(defn new-env
  "Create a new environment containing 'vals' binded to 'syms' and
  having 'env' as its outer environment."
  ([]
    (new-env [] []))
  ([syms vals]
    (new-env syms vals nil))
  ([syms vals env]
    (atom (merge {:outer-env env} (zipmap syms vals)))))

(defn figure
  "Resolve 'sym' in the environment 'env' and its outer environments and
  return its value or nil if not found."
  [sym env]
  (when env
    (let [val (get @env sym :not-found)]
      (if (= :not-found val)
        (recur sym (get @env :outer-env))
        val))))

(defn bind
  "Bind 'sym' to the value 'val' in the environment 'env' and return 'val'."
  [sym val env]
  (sym (swap! env assoc sym val)))

(defn unbind
  "Remove binding for 'sym' from the environment 'env'."
  [sym env]
  (swap! env dissoc sym))

(defn add-clojure-binds
  "Add bindings for a few clojure functions to environment 'env' and return it."
  [env]
  (doseq [binds {'+ +, '- -, '* *, '/ /, '= =, '< <, '> >}]
    (bind (key binds) (val binds) env))
  env)

(defonce global-env (add-clojure-binds (new-env)))

(defn evals
  "Evaluate expression 'exp' in the environment 'env'."
  ([exp]
    (evals exp global-env))
  ([exp env]
    (cond
      (symbol? exp)          (figure exp env)
      (not (list? exp))      exp
      (= 'quote (first exp)) (second exp)                                                       ;; (quote exp)
      (= 'if (first exp))    (let [[test then else] (rest exp)]                                 ;; (if test then else)
                               (evals (if (evals test env) then else) env))
      (= 'def (first exp))   (bind (second exp) (evals (third exp) env) env)             ;; (def name val)
      (= 'do (first exp))    (last (map #(evals % env) (rest exp)))                             ;; (do exp...)
      (= 'fun (first exp))   (fn [& args] (evals (third exp) (new-env (second exp) args env)))  ;; (fun (vars...) expr)
      :else                  (let [exprs (doall (map #(evals % env) exp))]                      ;; (funcname exprs...)
                               (apply (first exprs) (rest exprs))))))



;; design proposal? ---------------
(comment

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
    (def y i)
    (print y)))

(def max
  (fn (a b)
    (if (> a b)
      a
      b)))

(max n 20)

)

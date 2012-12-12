(ns slang.core)

(def third (comp first next next))
(def fourth (comp first next next next))

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
  ([]
    (add-clojure-binds (new-env)))
  ([env]
  (doseq [binds {'+ + '- - '* * '/ / '= = '< < '> > '<= <= '>= >=
                 'car first 'cdr rest 'cons cons 'list? list? 'symbol? symbol?
                 'new-env new-env 'figure figure 'bind bind 'unbind unbind
                 'print println}]
    (bind (key binds) (val binds) env))
  env))

;; global environment
(defonce global-env (add-clojure-binds (new-env)))

(defn evals
  "Evaluate expression 'exp' in the environment 'env'."
  ([exp]
    (evals exp global-env))
  ([exp env]
    ;(bind '&exp exp env) ;; will this be useful with macros?
    (cond
      (= '&env exp)          env                                                                         ;; environment itself ;)
      (symbol? exp)          (figure exp env)                                                            ;; variable reference
      (not (list? exp))      exp                                                                         ;; constant literals
      (= 'quote (first exp)) (second exp)                                                                ;; (quote exp)
      (= 'if (first exp))    (evals (if (evals (second exp) env) (third exp) (fourth exp)) env)          ;; (if test then else)
      (= 'def (first exp))   (bind (second exp) (evals (third exp) env) env)                             ;; (def name val)
      (= 'do (first exp))    (last (map #(evals % env) (rest exp)))                                      ;; (do exp...)
      (= 'fun (first exp))   (fn [& args] (evals (third exp) (new-env (second exp) args env)))           ;; (fun (vars...) expr)
      :else                  (apply (evals (first exp) env) (doall (map #(evals % env) (rest exp)))))))  ;; (funcname exprs...)


;; add evals to the environment
(bind 'evals evals global-env)

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

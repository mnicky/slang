(ns slang.core)

;;== garbage collector ======================================================

(defn empty-heap-el
  "Return new empty element of the heap iwth the giben position."
  [idx]
  {:type nil :val nil :marked false :idx idx})

(defn make-heap
  "Return heap of the specified 'size'."
  ([]
    (make-heap 100))
  ([size]
    (atom
      {:free (into clojure.lang.PersistentQueue/EMPTY (range 0 size))
       :mem (into [] (map empty-heap-el (range size)))
       :size size})))

;; global heap
(defonce global-heap (make-heap))

(defn mark
  "Mark all elements of the 'heap' that are referenced from the environment
  'env' or some of its outer environments."
  [env heap]
  (when env
    (swap! heap (fn [h] (reduce #(assoc-in %1 [:mem %2 :marked] true) h (vals (dissoc @env :outer-env)))))
    (recur (:outer-env @env) heap)))

(defn sweep-idx
  "Return 'heap' with the element at index 'idx' sweeped, if not marked."
  [heap idx]
  (if (nil? (get-in heap [:mem idx :type]))
    heap
    (if (true? (get-in heap [:mem idx :marked]))
      (assoc-in heap [:mem idx :marked] false)
      (-> heap (assoc-in [:mem idx] (empty-heap-el idx))
               (assoc :free (conj (:free heap) idx))))))

(defn sweep
  "Sweer all non marked elements from the 'heap'."
  [heap]
  (swap! heap #(reduce sweep-idx % (range (:size %)))))

(defn gc
  "Perform garbage collection on 'heap' using information about references in the environment 'env'."
  [env heap]
  (mark env heap)
  (sweep heap))

(defn ensure-free-mem
  "Ensure that 'heap' has free memory by performing gc if heap's full."
  [env heap]
  {:post [(not (empty? (:free @heap)))]}
  (when (empty? (:free @heap))
    (gc env heap)))

(defn get-type
  "Return the type of the value."
  [val]
  (if (coll? val) :coll :val))

(defn put-on-heap
  "Put value 'val' on the 'heap' in the environment 'env'."
  ([val env]
    (put-on-heap val env global-heap))
  ([val env heap]
    (ensure-free-mem env heap)
    (let [free-slot (peek (:free @heap))
          type (get-type val)]
      (swap! heap #(-> % (assoc-in [:mem free-slot :val] val)
                         (assoc-in [:mem free-slot :type] type)
                         (update-in [:free] pop)))
      free-slot)))

(defn get-from-heap
  "Return value from 'heap' by its reference index 'idx' or nil if not found."
  ([idx]
    (get-from-heap idx global-heap))
  ([idx heap]
    (get-in @heap [:mem idx :val])))

;;== interpreter ============================================================

(def third (comp first next next))
(def fourth (comp first next next next))

(defn new-env
  "Create a new environment containing 'vals' binded to 'syms' and
  having 'env' as its outer environment."
  ([]
    (new-env nil))
  ([env]
    (new-env [] [] env))
  ([syms vals]
    (new-env syms vals nil))
  ([syms vals env]
    (atom (merge {:outer-env env} (zipmap syms vals)))))

(defn exists?
  "Return whether the 'sym' is bound to some value in the environment 'env'."
  [sym env]
  (if env
    (if (contains? @env sym)
      true
      (recur sym (get @env :outer-env)))
    false))

(defn lookup
  "Resolve 'sym' in the environment 'env' and its outer environments and
  return its value or nil if not found."
  [sym env]
  (when env
    (let [idx (get @env sym :not-found)]
      (if (= :not-found idx)
        (recur sym (get @env :outer-env))
        (get-from-heap idx)))))

(defn bind
  "Bind 'sym' to the value 'val' in the environment 'env' and return 'val'."
  [sym val env]
  (swap! env assoc sym (put-on-heap val env))
  val)

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
                 'new-env new-env 'lookup lookup 'bind bind 'unbind unbind
                 'exists? exists? 'print println}]
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
      (= '&env exp)     env               ;; the environment itself ;)
      (symbol? exp)     (lookup exp env)  ;; variable reference
      (not (list? exp)) exp               ;; constant literal
      :else (case (first exp)
              quote (second exp)                                                          ;; (quote exp)
              if    (evals (if (evals (second exp) env) (third exp) (fourth exp)) env)    ;; (if test then else)
              def   (bind (second exp) (evals (third exp) env) env)                       ;; (def name val)
              do    (last (map #(evals % env) (rest exp)))                                ;; (do exp...)
              fun   (fn [& args] (evals (third exp) (new-env (second exp) args env)))     ;; (fun (vars...) expr)
              (apply (evals (first exp) env) (doall (map #(evals % env) (rest exp)))))))) ;; (funcname exprs...)


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

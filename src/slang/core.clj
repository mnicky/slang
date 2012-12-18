(ns slang.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

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
(defonce global-heap (make-heap 100))

(defn mark-idx
  "Mark element with given 'idx' on the 'heap'."
  [idx heap]
  (swap! heap #(assoc-in % [:mem idx :marked] true))
  (when (= :coll (get-in @heap [:mem idx :type]))
    (doseq [field-idx (vals (get-in @heap [:mem idx :val]))]
      (mark-idx field-idx heap))))

(defn mark
  "Mark all elements of the 'heap' that are referenced from the environment
  'env' or some of its outer environments."
  [env heap]
  (when env
    (doseq [idx (vals (dissoc @env :outer-env))]
      (mark-idx idx heap))
    (recur (:outer-env @env) heap)))

(defn sweep-idx
  "Return 'heap' with the element at index 'idx' sweeped, if it's not marked
  as referenced."
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
  (sweep heap)
  'Done.)

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
    ;(println "--put-on-heap" val env "->" (peek (:free @heap)))
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
    ;(println "--get-from-heap" idx "->" (get-in @heap [:mem idx :val]))
    (get-in @heap [:mem idx :val])))

;;== interpreter ============================================================

(def third (comp first next next))
(def fourth (comp first next next next))

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
  ;(println "--lookup" sym env "->" (get @env sym :not-found))
  (when env
    (let [idx (get @env sym :not-found)]
      (if (= :not-found idx)
        (recur sym (get @env :outer-env))
        (get-from-heap idx)))))

(defn bind
  "Bind 'sym' to the value 'val' in the environment 'env' and return 'val'."
  [sym val env]
  ;(println "--bind" sym val env "->" val)
  (swap! env assoc sym (put-on-heap val env))
  val)

(defn unbind
  "Remove binding for 'sym' from the environment 'env'."
  [sym env]
  (swap! env dissoc sym))

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
    (let [inner-env (atom {:outer-env env})]
      (doseq [ref (zipmap syms vals)]
        (bind (key ref) (val ref) inner-env))
      inner-env)))

;; Structure type
(defrecord Structure [])

(defn getf
  "Returns the value of the 'field' in given structure 'st'."
  [st field]
  (get-from-heap (get st field)))

(defn setf
  "Returns the structure 'st' with value of the given 'field' set to the 'val'.
  The binding is done in the environment 'env'."
  [st field val env]
  (assoc st field (put-on-heap val env)))

(defn new-struct
  "Returns new empty structure."
  []
  (Structure.))

;; printing the 'structure' type in the repl
(defmethod print-method Structure
  [st w]
  (.write w "{")
  (doseq [el (butlast st)] (.write w (str (key el) " " (get-from-heap (val el)) ", ")))
  (when-let [last-el (last st)]
    (.write w (str (key last-el) " " (get-from-heap (val last-el)))))
  (.write w "}"))

(defn with-init-binds
  "Return the environment 'env' with some initial bindings added."
  ([]
    (with-init-binds (new-env)))
  ([env]
  (doseq [binds {'+ + '- - '* * '/ / '= = '< < '> > '<= <= '>= >=
                 'car first 'cdr rest 'cons cons 'list? list? 'symbol? symbol?
                 'print println
                 'new-env new-env 'lookup lookup 'bind bind 'unbind unbind 'exists? exists?
                 'struct new-struct}]
    (bind (key binds) (val binds) env))
  env))

;; global environment
(defonce global-env (with-init-binds (new-env)))

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
              quote (second exp)                                                          ;; (quote expr)
              if    (evals (if (evals (second exp) env) (third exp) (fourth exp)) env)    ;; (if test then else)
              def   (bind (second exp) (evals (third exp) env) env)                       ;; (def name val)
              get   (getf (lookup (second exp) env) (third exp))                          ;; (get structname fieldname)
              set   (bind (second exp) (setf (lookup (second exp) env)                    ;; (set structname fieldname val)
                                              (third exp)
                                              (evals (fourth exp) env)
                                              env) env)
              do    (last (map #(evals % env) (rest exp)))                                ;; (do exprs...)
              for   (loop [local-env (new-env env) args (second exp)]                     ;; (for (i 1 10) expr)
                      (bind (first args) (second args) local-env)
                      (if (< (lookup (first args) local-env) (third args))
                        (do (evals (third exp) local-env)
                            (recur local-env
                                   (list (first args) (inc (second args)) (third args))))
                        (evals (third exp) local-env)))
              fun   (fn [& args] (evals (third exp) (new-env (second exp) args env)))     ;; (fun (args...) expr)
              (apply (evals (first exp) env) (doall (map #(evals % env) (rest exp)))))))) ;; (funcname args...)

;; add evals to the environment
(bind 'evals evals global-env)

;;== repl etc. ==============================================================

(defn safe-evals
  "Evals that prints "
  [exp]
  (try
    (evals exp)
    (catch Exception e (.printStackTrace e))))

(defn repl
  "Run the repl."
  []
  (def ^:dynamic *exit-repl* false)
  (bind 'exit #(do (alter-var-root #'*exit-repl* (constantly true)) "Bye!") global-env)
  (println "This is Slang. Invoke (exit) to quit the repl.")
  (while (false? *exit-repl*)
    (print "slang=> ")
    (flush)
    (println (safe-evals (read *in* false "Invoke (exit) to quit the repl.")))))

(defn exec-file
  "Interpret the file with given 'path' (line by line)."
  [path]
  (with-open [r (io/reader path)]
    (doseq [line (line-seq r)]
      (evals (read-string line)))))

(defn -main
  "Interpret file (path given as the first cmd argument) or run repl
  if no cmd line argument given."
  [& args]
  (if args
    (exec-file (first args))
    (repl)))

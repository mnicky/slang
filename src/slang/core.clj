(ns slang.core)




(comment

(def s "string")
(def n 10)

(def x (struct [a 20 b 30]))
(def y (struct []))
(def z (struct))

(set y.a 20)
(set y.b 30)

(print x.a)

(print (= x.a y.a))
(print (= x y))

(for [i [1 10]]
  (print x.a)
  (def y (struct)))

(def ahoj (fn [a b]
            (if (> a 0)
              (ahoj 10 b))))

(ahoj 10 x)

)

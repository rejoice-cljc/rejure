(ns rejure.lang.symbol
  (:refer-clojure :exclude [info])
  #?(:cljs (:require-macros [rejure.lang.symbol])))
 
#?(:clj
   (do
     (defmacro info
       "Get def symbol, value, and metadata."
       [x]
       `(let [obj# (var ~x)]
          {:symbol '~x
           :value  (deref obj#)
           :meta   (meta obj#)}))

     (comment
       (do
         (def x "foo")
         (info x)))
     
     (defmacro vecplace
       "Replace vector of symbols with their resolved info. 
        Can optionally pass `f` callback to transform each symbol's outputs."
       ([v] `(vecplace identity ~v))
       ([f v]
        (reduce
         (fn [acc x]
           (conj acc (if (vector? x) `(vecplace ~f ~x) `(~f (info ~x)))))
         []
         v)))

     (comment
       (do
         (def x "foo")
         (def y "bar")
         (vecplace
          [x [y]])))))

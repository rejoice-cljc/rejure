(ns rejure.store.context
  #?(:cljs (:require ["recoil" :as recoil]))
  #?(:clj (:require [rejure.lang.symbol :as sym]))
  #?(:cljs (:require-macros [rejure.store.context])))

#?(:cljs
   (do
     (defn store-value->recoil-value
       "Create recoil atom or selector for given key `k` and value `v`."
       [k v]
       (cond
         (or (nil? v) (string? v) (number? v)) (recoil/atom #js {:key (keyword k)
                                                                 :default v})
         (map? v) (recoil/atom #js {:key (keyword k)
                                    :default (:default v)})
         :else v))

     (defn create-context*
       "Create store context from `stores-map`."
       [stores-map]
       (reduce-kv
        (fn [acc k {:keys [state dispatch-fn]}]
          {:init-map  (merge (:init-map acc)
                             (reduce-kv
                              (fn [acc k v]
                                (if (and (map? v) (:initial v))
                                  (assoc acc k (:initial v))
                                  acc))
                              {}
                              state))
           :state-map (merge (:state-map acc)
                             (reduce-kv
                              (fn [acc sk sv]
                                (assoc acc sk (store-value->recoil-value sk sv)))
                              {}
                              state))
           :dispatch-map (assoc (:dispatch-map acc) k dispatch-fn)})
        {:init-map    {}
         :state-map   {}
         :distach-map {}}
        stores-map))

     (comment
       (create-context*
        {::store1 {:state {::all1 []}
                   :dispatch-fn identity}})

       (create-context*
        {::store1 {:state {::all1 []}
                   :dispatch-fn identity}
         ::store2 {:state {::all2 []}
                   :dispatch-fn identity}}))))

#?(:clj
   (do
     (defmacro store-syms->stores-map
       "Converts vector of store symbol names to map of their namespace key and referenced value."
       [store-syms]
       `(sym/deduce
         (fn [acc# info#]
           (assoc acc# (ns-name (get-in info# [:meta :ns])) (:value info#)))
         {}
         ~store-syms))

     (comment
       (def x :foo)
       (store-syms->stores-map
        [x]))

     (defmacro create-context
       [store-syms]
       `(let [stores-map# (store-syms->stores-map ~store-syms)]
          (create-context* stores-map#)))))


(comment
  (def store {:state {} :dispatch-fn identity})
  (create-context
   [store]))

(ns rejure.store.context
  #?(:cljs (:require
            ["react" :as react]
            ["recoil" :as recoil]))
  #?(:clj (:require [rejure.lang.symbol :as sym]))
  #?(:cljs (:require-macros [rejure.store.context])))

;; == store context components == 

#?(:cljs 
   (do
     (def h* react/createElement)
     (def context (react/createContext {}))

     (defn call-inital-states
       "Calls all initial states defined in `store` :init-map."
       [store interface]
       (.all js/Promise
             (mapv (fn [[k f]]
                     (let [state (get-in store [:values-map k])]
                       (-> (.resolve js/Promise (f))
                           (.then (fn [x] (.set interface state x))))))
                   (:init-map store))))

     (defn- StateInitializer [props]
       (let [store (react/useContext context)
             [ready set-ready] (react/useState false)
             init-state (recoil/useRecoilCallback
                         (fn [interface]
                           (fn [] (call-inital-states store interface)))
                         #js [])]
         (react/useEffect
          (fn []
            (-> (init-state)
                (.then (fn [] (set-ready true))))
            js/undefined)
          #js [])
         (and ready (.-children props))))

     (defn provider [^js props]
       (h*
        (.-Provider context)
        #js {:value (.-store props)}
        (h*
         recoil/RecoilRoot
         nil
         (h*
          StateInitializer
          nil
          (.-children props)))))))

;; == store context factory == 

#?(:cljs
   (do
     (defn- store-value->recoil-value
       "Create recoil atom or selector for given key `k` and value `v`."
       [k v]
       (cond
         (or (nil? v) (string? v) (number? v)) (recoil/atom #js {:key (keyword k)
                                                                 :default v})
         (map? v) (recoil/atom #js {:key (keyword k)
                                    :default (:default v)})
         :else v))

     (defn create-store*
       "Create store from `stores-map`."
       [stores-map]
       (reduce-kv
        (fn [acc k {:keys [values dispatch-fn]}]
          {:init-map  (merge (:init-map acc)
                             (reduce-kv
                              (fn [acc k v]
                                (if (and (map? v) (:initial v))
                                  (assoc acc k (:initial v))
                                  acc))
                              {}
                              values))
           :values-map (merge (:values-map acc)
                             (reduce-kv
                              (fn [acc sk sv]
                                (assoc acc sk (store-value->recoil-value sk sv)))
                              {}
                              values))
           :dispatch-map (assoc (:dispatch-map acc) k dispatch-fn)})
        {:init-map    {}
         :values-map   {}
         :distach-map {}}
        stores-map))

     (comment
       (create-store*
        {::state1 {:values {::all1 []}
                   :dispatch-fn identity}})

       (create-store*
        {::state1 {:values {::all1 []}
                   :dispatch-fn identity}
         ::states2 {:values {::all2 []}
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

     (defmacro create-store
       [store-syms]
       `(let [stores-map# (store-syms->stores-map ~store-syms)]
          (create-store* stores-map#)))))

(comment
  (def store {:values {} :dispatch-fn identity})
  (create-store
   [store]))

(ns rejure.store.context
  #?(:cljs (:require
            ["react" :as react]
            ["recoil" :as recoil]))
  #?(:clj (:require [rejure.symbol :as sym]))
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
     (defn- primitive?
       "Checks whether value `x` is a primitive type.
        For collections, only vectors are allowed, as any map field can be its an atom value."
       [x]
       (or (nil? x) (string? x) (number? x) (vector? x)))

     (defn- atom? "Checks whether store value `x` can be a recoil atom type."
       [x]
       (or (primitive? x) (and (map? x) (get x :initial))))

     (defn- selector? "Checks whether store value `x` can be a recoil selector type."
       [x]
       (and (map? x) (get x :deps)))

     (defn- value-atom->recoil-atom "Converts store atom value to recoil atom."
       [k v]
       (recoil/atom
        #js {:key (keyword k)
             :default (if (primitive? v) v (:default v))}))

     (defn get-data-source-output
       [x output]
       (cond
         (keyword? output) (get x output)
         (fn? output) (output x)
         :else output))

     (defn- call-data-source
       [data-sources deps-map query]
       (let [source-key  (first query)
             source-args (second query)]
         (if-let [f (get data-sources source-key)]
           (f deps-map source-args)
           (throw (ex-info "Missing data-source" {:key source-key})))))

     (defn- value-selector->recoil-selector "Converts store selector value to recoil selector."
       [k m data-sources]
       (let [{:keys [deps query output]} m]
         (recoil/selector
          #js {:key (keyword k)
               :get (fn [ctx]
                      (let [deps-map (reduce (fn [acc k] (assoc acc k (.get ctx k))) deps)]
                        (-> (->> (call-data-source data-sources deps-map query)
                                 (.resolve js/Promise))
                            (.then #(get-data-source-output % output)))))})))

     (defn- store-value->recoil-value
       "Create recoil atom or selector for given key `k` and value `v`."
       [k x data-sources]
       (cond
         (or (primitive? x) (atom? x)) (value-atom->recoil-atom k x)
         (selector? x) (value-selector->recoil-selector k x data-sources)
         :else (throw (ex-info "Invalid store state." {:key k  :value x}))))

     (comment
       (store-value->recoil-value :user nil {})

       (store-value->recoil-value
        :user {:deps [:session/token]
               :query [:api {:auth/user [:user/id]}]}
        {:api (fn [_ eql] eql)}))

     (defn create-store*
       "Create store from `state-map` and `data-sources`."
       [state-map data-sources]
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
                                 (assoc acc sk (store-value->recoil-value sk sv data-sources)))
                               {}
                               values))
           :dispatch-map (assoc (:dispatch-map acc) k dispatch-fn)})
        {:init-map     {}
         :values-map   {}
         :distach-map  {}}
        state-map))

     (comment
       (create-store*
        {::state1 {:values {::all1 []}
                   :dispatch-fn identity}}
        {})

       (create-store*
        {::state1 {:values {::all1 []}
                   :dispatch-fn identity}
         ::states2 {:values {::all2 []}
                    :dispatch-fn identity}}
        {}))))

#?(:clj
   (do
     (defmacro store-syms->state-map
       "Converts vector of store symbol names to map of their namespace key and referenced value."
       [store-syms]
       `(sym/deduce
         (fn [acc# info#]
           (assoc acc# (keyword (ns-name (get-in info# [:meta :ns]))) (:value info#)))
         {}
         ~store-syms))

     (comment
       (def x :foo)
       (store-syms->state-map
        [x]))

     (defmacro create-store
       [store-syms data-sources]
       `(let [state-map# (store-syms->state-map ~store-syms)]
          (create-store* state-map# ~data-sources)))))


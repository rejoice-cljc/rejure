(ns rejure.store.manage
  (:require
   ["react" :as react]
   ["recoil" :as recoil]
   [cljs.core.async :as async]
   [rejure.store.context :as stc]))

(def event-chan (async/chan))

(defn dispatcher "Multi-dispatch dispacther fn."
  [multi _state _input] multi)

(defn use-keys [ks]
  (let [store (react/useContext stc/context)]
    (reduce
     (fn [acc k]
       (let [state (get-in store [:state-map k])]
         (assoc acc k (recoil/useRecoilValue state))))
     {}
     ks)))

(defn use-dispatch []
  (let [store (react/useContext stc/context)]
    (recoil/useRecoilCallback
     (fn [^js interface]
       (fn [multi input]
         (let [state-key  (first multi)
               store-key  (keyword (namespace state-key))
               state      (get-in store [:state-map state-key])
               dispatch   (get-in store [:dispatch-map store-key])]
           (-> (.getPromise (.-snapshot interface) state)
               (.then (fn [curr]
                        (.set interface state (dispatch multi curr input))
                        (async/put! event-chan [multi input])))))))
     #js [])))

(defn use-keys+dispatch [ks]
  [(use-keys ks) (use-dispatch)])

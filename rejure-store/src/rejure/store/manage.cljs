(ns rejure.store.manage
  (:require
   ["react" :as react]
   ["recoil" :as recoil]
   [cljs.core.async :as async]
   [rejure.store.context :as stc]))

(def event-chan (async/chan))

;; todo 
;; - remove :values for single :state (?)
;; -put dispatch key in _ctx  map

(defmulti reducer
  (fn dispatcher [_curr [key _input]] 
    (if (and (vector? key) (= (count key) 2))
     key
     (throw (ex-info "Invalid multi reducer." {:key key})))))

(defmulti watcher
  (fn dispatcher [_ctx [key _prev _next]]
    key))

(defmulti action
  (fn dispatcher [_ctx [key _input]]
    key))

(defn use-keys [ks]
  ;; todo use recoil callback rather than looping over value hook (?)
  (let [store (react/useContext stc/context)]
    (reduce
     (fn [acc k]
       (let [state (get-in store [:values-map k])]
         (assoc acc k (recoil/useRecoilValue state))))
     {}
     ks)))

;; todo here have to figure out namespcae dispatch key stuff
;; todo trying to access missing keys here. need to enforce conventions.
(defn use-dispatch []
  (let [store (react/useContext stc/context)]
    (recoil/useRecoilCallback
     (fn [^js interface]
       (letfn
        [(dispatch [multi input]
                   (let [context {:dispatch dispatch}
                         ;; todo no always state 
                         state-key  (if (vector? multi) (first multi) multi)
                         state      (get-in store [:values-map state-key])
                         reducer-fn (get-method reducer multi)
                         action-fn  (get-method action multi)
                         watcher-fn  (get-method watcher state-key)]
                     (cond
                       reducer-fn (-> (.getPromise (.-snapshot interface) state)
                                   (.then
                                    (fn [^js curr-val]
                                      (let [next-val (reducer-fn curr-val [multi input])]
                                       (.set interface state next-val)
                                       (when watcher-fn (watcher-fn context [multi curr-val next-val]))
                                       (async/put! event-chan [multi input])))))
                       action-fn (action-fn context [multi input])
                       :else (throw (ex-info "State action not found." {:key state-key})))))]
         dispatch))
     #js [])))

(defn use-keys+dispatch [ks]
  [(use-keys ks) (use-dispatch)])

(ns rejure.store.manage-test
  (:require ["@testing-library/react-hooks" :as th]
            [rejure.store.context :as stc]
            [rejure.store.manage :as stm]
            [cljs.core.async :refer [go]]
            [cljs.core.async.interop :refer [<p!]]
            [cljs.test :refer [deftest testing is async]]))

(defn await-rerender [^js rh]
  (-> (.waitForNextUpdate rh)
      (.then #(identity rh))))

(defn await-rerender-value [^js rh]
  (-> (await-rerender rh)
      (.then #(-> % .-result .-current))))

(defn render-store-hook [store hook]
  (letfn [(StoreWrapper [props]
            (stc/provider
             #js {:store (stc/create-store* store)
                  :children (.-children props)}))]
    (let [rh (th/renderHook hook #js {:wrapper StoreWrapper})]
      (await-rerender rh))))

;; == Tests ==

(deftest reducer-dispatch-test []
  (testing "reducer dispatch"
    (let [store {:rejure.store.manage-test {:values {::num 1}}}]
      (remove-all-methods stm/reducer)

      (defmethod stm/reducer [::num :set]
        [_ [_ n]] n)

      (async
       done
       (go
         (let [rh (<p! (render-store-hook store #(stm/use-keys+dispatch [::num])))
               [{n ::num} dispatch] (-> rh .-result .-current)]
           (is (= n 1) "Initial reducer value.")
           (dispatch [::num :set] 2)
           (let [[{n ::num} _] (<p! (await-rerender-value rh))]
             (is (= n 2) "Set new reducer value."))
           (done)))))))

(deftest watcher-dispatch-test []
  (testing "watcher dispatch: basic n reducer"
    (let [store {:rejure.store.manage-test {:values {::num 1}}}
          remote-store (atom {})]
      (remove-all-methods stm/reducer)
      (remove-all-methods stm/watcher)

      (defmethod stm/reducer [::num :set]
        [_ [_ n]] n)
      
      (defmethod stm/watcher ::num
        [_ [_ _ n]] 
        (swap! remote-store assoc ::num n))

      (async
       done
       (go
         (let [rh (<p! (render-store-hook store #(stm/use-keys+dispatch [::num])))
               [_ dispatch] (-> rh .-result .-current)]
           (dispatch [::num :set] 2)
           (let [[{n ::num} _] (<p! (await-rerender-value rh))]
             (is (= n 2) "Set new reducer value.")
             (is (= (::num @remote-store) 2) "Calls watcher."))
           (done)))))))

(deftest action-dispatch-test []
  (testing "action dispatch"
    (let [store {:rejure.store.manage-test {:values {::token nil}}}]
      (remove-all-methods stm/reducer)
      (remove-all-methods stm/action)

      (defmethod stm/reducer [::token :set]
        [_ [_ token]] token)

      (defmethod stm/action ::login
        [{:keys [dispatch]} [_ token]]
        (dispatch [::token :set] token))

      (async
       done
       (go
         (let [rh (<p! (render-store-hook store #(stm/use-keys+dispatch [::token])))
               [{token ::token} dispatch] (-> rh .-result .-current)]
           (is (= token nil) "Initial token.")
           (dispatch ::login "my-bcrypt-token")
           (let [[{token ::token} _] (<p! (await-rerender-value rh))]
             (is (= token "my-bcrypt-token") "Calls dispatch and sets value.")))
         (done))))))

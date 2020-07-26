(ns rejure.router.context
  (:require
   #?@(:cljs [[clojure.string :as str]])
   #?@(:clj [[rejure.lang.symbol :as sym]]))
  #?(:cljs (:require-macros [rejure.router.context])))

#?(:cljs 
   (do
     (defn- normalize-path  "Normalize route path by removing any extra forward slashes."
       [s]
       (str/replace s #"/{2,}" "/"))

     (defn- get-route-subns "Returns subnamespace path of `sym` relative to app's view directory."
       [subns sym]
       (if-let [match (re-find subns (str sym))]
         (second match)
         (throw (ex-info "Unable to match route." {:sym sym  :subns subns}))))

     (defn- get-route-parent-keys "Gets all parent keys from given child route key `k`."
       [k]
       (mapv
        keyword
        (reduce
         (fn [acc curr]
           (conj acc (if (> (count acc) 0) (str (str/join acc) "." curr) curr)))
         []
         (butlast (str/split (name k) #".")))))

     (defn- resolve-route-path
       "Resolves full page path of route based on `rt-parent-keys` and `rt-info`.
        Ensures that parent routes exists in routes map `rts-map`."
       [rts-map rt-parent-keys rt-info]
       (normalize-path
        (str
         (reduce
          (fn [acc k]
            (if-let [parent (get rts-map k)]
              (str acc "/" (get-in parent [:meta :route/path]))
              (throw (ex-info "Unable to resolve parent route." {:key k}))))
          ""
          rt-parent-keys)
         "/"
         (get-in rt-info [:meta :route/path]))))

     (defn routes-info->routes-paths
       "Gets map of all route paths from vector of routes info `rts-info`."
       [cfg rts-info]
       (reduce
        (fn [acc {:keys [meta] :as rt-info}]
          (let [rt-key (keyword (get-route-subns (:route-subns cfg) (:ns meta)))
                rt-parent-keys  (get-route-parent-keys rt-key)]
            (assoc acc rt-key (resolve-route-path acc rt-parent-keys rt-info))))
        {}
        ;; note: routes info must be sorted so that accum map passed to children already has its parent route paths.
        (sort-by :symbol (flatten rts-info))))

     (defn routes-info->routes-config
       "Creates React Router routes configuraiton from vector of routes info `rts-info`.
        Requires element factory `el-fac` for turning route components into elements."
       [el-fac rts-info]
       (reduce
        (fn [acc x]
          (letfn [(route
                    [{:keys [meta value]}]
                    {:path (:route/path meta)
                     :element (el-fac value)})]
            (conj acc (if (vector? x)
                        (let [parent (first x)
                              children (rest x)]
                          (if-not children
                            (route parent)
                            (assoc (route parent) :children (routes-info->routes-config el-fac children))))
                        (route x)))))
        []
        rts-info))))

#?(:clj
   (defmacro create-context
     [rts-syms cfg]
     `(let [rts-info# (sym/vecplace ~rts-syms)]
        {:react-router/config (routes-info->routes-config ~(:create-element cfg) rts-info#)
         :paths (routes-info->routes-paths ~cfg rts-info#)})))

 (comment
   (def ^{:route/path "/"} rt :route)
   (create-context
    [rt]
    {:create-element identity
     :route-subns #"rejure.router.(.*)"}))

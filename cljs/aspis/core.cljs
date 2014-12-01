(ns aspis.core
  (:require-macros
    [aspis.core :refer [open-queue!]])
  (:require
    [goog.object]))

(def ^:private *initfns* (array))

(defn ^:export init! []
  (open-queue! *initfns*))

(defn on-browser? []
  (exists? js/document))

(defn on-server? []
  (not (on-browser?)))

(if (on-browser?)
  (enable-console-print!)
  (set-print-fn! js/print))

(defn extend-props [props extra-props]
  (let [js-props (js-obj)
        extended-props (atom {})]
    (goog.object.extend js-props props extra-props)
    (goog.object.forEach js-props
      (fn [elt idx obj]
        (swap! extended-props assoc (keyword idx) elt)))
    @extended-props))

(defn- component-force-update [component reference old-value new-value]
  (when (.isMounted component)
    (.forceUpdate component)))

(defn- atom? [value]
  (and value (satisfies? cljs.core/IWatchable value)))

(def aspis-mixin
  (js-obj
    "componentDidMount"
    (fn []
      (this-as this
        (doseq [arg (aget this "props" "cljs-refs")]
          (cond
            (atom? arg)
              (add-watch arg this component-force-update)
            (and (string? arg) (atom? (aget this "props" arg)))
              (add-watch (aget this "props" arg) this component-force-update)
            :else nil))))
    "componentWillReceiveProps"
    (fn [next-props]
      (this-as this
        (doseq [arg (aget this "props" "cljs-refs")]
          (when (and (string? arg) (not= (aget this "props" arg) (aget next-props arg)))
            (when (atom? (aget this "props" arg))
              (remove-watch (aget this "props" arg) this))
            (when (atom? (aget next-props arg))
              (add-watch (aget next-props arg) this component-force-update))))))
    "componentWillUnmount"
    (fn []
      (this-as this
        (doseq [arg (aget this "props" "cljs-refs")]
          (cond
            (atom? arg)
              (remove-watch arg this)
            (and (string? arg) (atom? (aget this "props" arg)))
              (remove-watch (aget this "props" arg) this)
            :else nil))))))

(defn- adjust-children [argmap]
  (if (and (contains? argmap :children)
           (or (seq? (:children argmap)) (sequential? (:children argmap))))
    (let [children (seq (:children argmap))]
      (if (= 1 (count children))
        (assoc argmap :children (first children))
        (assoc argmap :children (to-array children))))
    argmap))

(defn to-react-children [children]
  (cond
    (and (array? children) (= 1 (.-length children)))
      (aget children 0)
    (array? children)
      children
    (or (seq? children) (sequential? children))
      (let [seq-children (seq children)]
        (if (= 1 (count seq-children))
          (first seq-children)
          (to-array seq-children)))
    :else children))

(defn- parse-arglist [arglist]
  (->
    (if (map? (first arglist))
      (if (not-empty (rest arglist))
        (assoc (first arglist) :children (rest arglist))
        (first arglist))
      (let [first-keyword?  (comp keyword? first)
            pairs           (partition-all 2 arglist)
            props           (->> pairs (take-while first-keyword?) (map vec) (into {}))
            children        (->> pairs (drop-while first-keyword?) (mapcat identity))]
        (if (not-empty children)
          (assoc props :children children)
          props)))
    (adjust-children)))

(defn- argmap-to-props [argmap]
  (->
    (fn [o k v]
      (if (atom? v)
        (throw (js/Error. (str "Passed in cell for " k))))
      (condp = k
        :css      (aset o "style" (clj->js v))
        :html     (aset o "dangerouslySetInnerHTML" (js-obj "__html" (str v)))
        :class    (aset o "className" (.classSet js/React.addons (clj->js v)))
                  (aset o (name k) v))
      o)
    (reduce-kv (js-obj) argmap)))

(defn- tag [tag arglist]
  (let [argmap (parse-arglist arglist)]
    (when (:toggle argmap true)
      (let [props (argmap-to-props argmap)]
        (when (and (not (array? (aget props "children")))
                 (not (nil? (aget props "children")))
                 (not (string? (aget props "children")))
                 (not (.isValidElement js/React (aget props "children"))))
          (.log js/console props)
          (throw "Bad children!"))
        (.createElement js/React tag props)))))

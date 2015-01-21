(ns aspis.core
  (:require-macros
    [aspis.core :as a])
  (:require
    [goog.object]))

(a/defqueue *initfns*)

(defn on-browser? []
  (exists? js/document))

(defn on-server? []
  (not (on-browser?)))

(if (on-browser?)
  (enable-console-print!)
  (set-print-fn! js/print))

(defn extend-props [props extra-props]
  (let [js-props (js-obj)]
    (goog.object.extend js-props (clj->js props) (clj->js extra-props))
    js-props))

(defn- component-force-update [component reference old-value new-value]
  (when (.isMounted component)
    (.forceUpdate component)))

(defn atom? [value]
  (and value (satisfies? cljs.core/IWatchable value)))

(defn- guarded-add-watch [watchable k f]
  (when (atom? watchable)
    (add-watch watchable k f)))

(defn- guarded-remove-watch [watchable k]
  (when (atom? watchable)
    (remove-watch watchable k)))

(def aspis-mixin
  (js-obj
    "componentDidMount"
    (fn []
      (this-as this
        (doseq [arg (aget this "global-refs")]
          (guarded-add-watch arg this component-force-update))
        (doseq [arg (aget this "props-refs")]
          (guarded-add-watch (aget this "props" arg) this component-force-update))
        (doseq [arg (aget this "state-refs")]
          (guarded-add-watch (aget this "state" arg) this component-force-update))))
    "componentWillReceiveProps"
    (fn [next-props]
      (this-as this
        (doseq [arg (aget this "props-refs")]
          (when (not= (aget this "props" arg)
                      (aget next-props arg))
            (guarded-remove-watch (aget this "props" arg) this)
            (guarded-add-watch (aget next-props arg) this component-force-update)))))
    "componentWillUnmount"
    (fn []
      (this-as this
        (doseq [arg (aget this "global-refs")]
          (guarded-remove-watch arg this))
        (doseq [arg (aget this "props-refs")]
          (guarded-remove-watch (aget this "props" arg) this))
        (doseq [arg (aget this "state-refs")]
          (guarded-remove-watch (aget this "state" arg) this))))))

(defn- to-react-children [children]
  (if (or (seq? children) (sequential? children))
    (to-array (seq children))
    children))

(defn- classes-to-className [classes]
  (loop [className ""
         remaining-classes (seq classes)]
    (if (empty? remaining-classes)
      className
      (recur (str className " " (first remaining-classes)) (rest remaining-classes)))))

(defn- styles-to-style [styles]
  (loop [style (js-obj)
         remaining-styles (seq styles)]
    (if (empty? remaining-styles)
      style
      (do
        (if-let [first-style (first remaining-styles)]
          (goog.object.extend style (clj->js first-style)))
        (recur style (rest remaining-styles))))))

(defn- args-to-props [args]
  (let [first-keyword?  (comp keyword? first)
        pairs           (partition-all 2 args)
        props           (->> pairs (take-while first-keyword?) (map vec) (into {}))
        children        (->> pairs (drop-while first-keyword?) (mapcat identity))
        props           (if (not-empty children)
                          (assoc props :children children)
                          props)]
    (reduce-kv
      (fn [p k v]
        (condp = k
          :css      (aset p "styles" [v])
          :style    (aset p "styles" [v])
          :class    (aset p "classes" [v])
          :children (aset p "children" (to-react-children v))
          (aset p (name k) v))
        p)
      (or (:merge props) (js-obj))
      (dissoc props :merge))))

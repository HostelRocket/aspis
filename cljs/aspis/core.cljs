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

(defn- component-force-update [component reference old-value new-value]
  (when (.isMounted component)
    (.forceUpdate component)))

(defn atom? [value]
  (and value (satisfies? cljs.core/IWatchable value)))

(defn with-props [element props]
  (.cloneWithProps js/React.addons element (clj->js props)))

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
    (let [child-array (to-array (seq children))]
      (if (= 1 (.-length child-array))
        (aget child-array 0)
        child-array))
    children))

(defn- class-to-className [klass]
  (cond
    (nil? klass)
      ""
    (string? klass)
      klass
    (map? klass)
      (.classSet js/React.addons (clj->js klass))
    :else
      (.classSet js/React.addons klass)))

(defn- classes-to-className [classes]
  (.join (to-array classes) " "))

(defn- styles-to-style [styles]
  (loop [style (js-obj)
         remaining-styles (seq styles)]
    (if (empty? remaining-styles)
      style
      (do
        (if-let [first-style (first remaining-styles)]
          (goog.object.extend style (clj->js first-style)))
        (recur style (rest remaining-styles))))))

(defmulti set-property! (fn [props pname pvalue] pname))
(defmethod set-property! :default [p key value] (aset p (name key) value))
(defmethod set-property! :css [p _ value] (aset p "style" (clj->js value)))
(defmethod set-property! :style [p _ value] (aset p "style" (clj->js value)))
(defmethod set-property! :styles [p _ value] (aset p "style" (styles-to-style value)))
(defmethod set-property! :class [p _ value] (aset p "className" (class-to-className value)))
(defmethod set-property! :classes [p _ value] (aset p "className" (classes-to-className value)))
(defmethod set-property! :children [p _ value] (aset p "children" (to-react-children value)))

(defn- args-to-props [orig-args]
  (let [props (js-obj)]
    (loop [args orig-args]
      (let [k (first args)]
        (cond
          (nil? k)
            nil
          (keyword? k)
            (do
              (set-property! props k (second args))
              (recur (next (next args))))
          :else
            (aset props "children" (to-array args)))))
    props))

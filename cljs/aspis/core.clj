(ns aspis.core
  (:require
    [clojure.string :as string]
    [clojure.walk   :as walk]
    [cljs.analyzer  :as a]))

(def ^:private react-elements
  '[a abbr address area article aside audio b base bdi bdo big blockquote body br button canvas caption cite code col colgroup data datalist dd del details dfn div dl dt em embed fieldset figcaption figure footer form h1 h2 h3 h4 h5 h6 head header hr html i iframe img input ins kbd keygen label legend li link main mark menu menuitem meter nav noscript object ol optgroup option output p param pre progress q rp rt ruby s samp script section select small source span strong style sub summary sup table tbody td textarea tfoot th thead title tr track u ul video wbr circle defs ellipse g line linearGradient path pattern polygon polyline radialGradient rect stop svg tspan])

(def ^:private react-attributes
  (set
    '[accept acceptCharset accessKey action allowFullScreen allowTransparency alt async autoComplete autoPlay cellPadding cellSpacing charSet checked classID className cols colSpan content contentEditable contextMenu controls coords crossOrigin data dateTime defer dir disabled download draggable encType form formAction formEncType formMethod formNoValidate formTarget frameBorder height hidden href hrefLang htmlFor httpEquiv icon id label lang list loop manifest marginHeight marginWidth max maxLength media mediaGroup method min multiple muted name noValidate open pattern placeholder poster preload radioGroup readOnly rel required role rows rowSpan sandbox scope scrolling seamless selected shape size sizes span spellCheck src srcDoc srcSet start step style tabIndex target title type useMap value width wmode autoCapitalize autoCorrect property itemProp itemScope itemType cx cy d dx dy fill fillOpacity fontFamily fontSize fx fy gradientTransform gradientUnits markerEnd markerMid markerStart offset opacity patternContentUnits patternUnits points preserveAspectRatio r rx ry spreadMethod stopColor stopOpacity stroke strokeDasharray strokeLinecap strokeOpacity strokeWidth textAnchor transform version viewBox x1 x2 x y1 y2 y]))

(def ^:private react-dom-syms
  (set (conj react-elements
             'html-map 'html-meta 'html-time
             'svg-mask 'svg-var 'svg-text
             'CSSTransitionGroup 'TransitionGroup)))

(defn- insert-this [env form bindings]
  (if (seq? form)
    (let [expanded (a/macroexpand-1 env form)]
      (if (and (sequential? expanded)
               (= 'fn* (first expanded)))
        (let [arities (rest expanded)
              fname   (when (symbol? (first arities)) [(first arities)])
              arities (if fname (rest arities) arities)
              arities (if (vector? (first arities)) [arities] arities)
              arities (map (fn [[bind & body]] (list bind `(cljs.core/this-as ~(symbol "this") (let [~@bindings] ~@body)))) arities)]
          (cons 'fn* (concat fname arities)))
        form))
    form))

(defn props-bindings [props]
  (concat
    (when (contains? props :as)
      [(symbol (name (:as props))) `(cljs.core/aget ~(symbol "this") "props")])
    (mapcat
      (fn [[k v]]
        [k `(cljs.core/aget ~(symbol "this") "props" ~(name v))])
      (reduce
        #(assoc %1 (symbol (name %2)) (keyword (name %2)))
        (dissoc props :keys :strs :syms :or :as)
        (concat (:keys props) (:strs props) (:syms props))))))

(defn default-props [props]
  `(fn []
     (cljs.core/js-obj
       ~@(mapcat (fn [[k v]] [(name k) v]) (:or props)))))

(defn state-bindings [initial-state]
  (mapcat
    (fn [k]
      [(symbol (name k)) `(cljs.core/aget ~(symbol "this") "state" ~(name k))])
    (take-nth 2 initial-state)))

(defn get-initial-state [props initial-state]
  (let [state (partition 2 initial-state)
        state-atoms (mapcat (fn [[k v]] [(name k) `(clojure.core/atom ~v)]) state)]
    `(fn []
      (cljs.core/this-as ~(symbol "this")
        (let [~@(props-bindings props)]
          (cljs.core/js-obj ~@state-atoms))))))

(defmacro defmixin [mixin-name & forms]
  (let [methods
        (->>
          forms
          (partition 2)
          (map (fn [[k v]] [(name k) (insert-this &env v [])])))]
  `(def ~(vary-meta mixin-name assoc :export true)
    (cljs.core/js-obj
      ~@(apply concat methods)))))

(defmacro defelem [class-name & forms]
  (let [global-refs (atom #{})
        prop-refs   (atom #{})
        state-refs  (atom #{})
        [props state forms]
        (if (map? (first forms))
          (if (vector? (second forms))
            [(first forms) (second forms) (drop 2 forms)]
            [(first forms) nil (rest forms)])
          [nil nil forms])
        props-names (into {} (map vec (partition 2 (props-bindings props))))
        state-names (into {} (map vec (partition 2 (state-bindings state))))
        spec
        (->>
          (if (keyword? (first forms))
            forms
            (cons :render forms))
          (partition 2)
          (map (fn [[k v]] [(name k) v]))
          (into {})
          (reduce-kv (fn [m k v] (assoc m k (insert-this &env v (state-bindings state)))) {}))
        spec
        (walk/prewalk
          (fn [x]
            (cond
              (and
                (list? x)
                (contains? react-dom-syms (first x)))
                  (cons (symbol "aspis.core" (name (first x))) (rest x))
              (and (list? x)
                   (or
                     (= (symbol "clojure.core" "deref") (first x))
                     (= (symbol "deref") (first x))))
                (cond
                  (and (symbol? (second x)) (= "this" (namespace (second x))))
                    (let [[k1 k2] (string/split (name (second x)) #"\.")]
                      (cond
                        (= "props" k1)
                          (swap! prop-refs conj k2)
                        (= "state" k1)
                          (swap! state-refs conj k2))
                      x)
                  (and (symbol? (second x)) (contains? props-names (second x)))
                    (do
                      (swap! prop-refs conj (nth (get props-names (second x)) 3))
                      `(clojure.core/deref ~(symbol (name (second x)))))
                  (and (symbol? (second x)) (contains? state-names (second x)))
                    (do
                      (swap! state-refs conj (name (second x)))
                      `(clojure.core/deref ~(symbol (name (second x)))))
                  :else
                    (do
                      (swap! global-refs conj (second x))
                      x))
              (and (symbol? x) (= "this" (namespace x)))
                `(cljs.core/aget ~(symbol "this")
                  ~@(clojure.string/split (name x) #"\."))
              :else x))
          spec)
        methods
        (merge
          spec
          (when state
            { "getInitialState" (get-initial-state props state) })
          (when (contains? props :or)
            { "getDefaultProps" (default-props props) })
          { "render"
            `(fn []
              (cljs.core/this-as ~(symbol "this")
                (let [~@(props-bindings props)
                      ~@(state-bindings state)]
                  ~(get spec "render")))) })
        hooks
        (merge
          methods
          { "mixins" `(cljs.core/array aspis.core/aspis-mixin ~@(get methods "mixins"))
            "displayName" (str (ns-name *ns*) "." class-name) }
          (when (not-empty @global-refs)
            { "global-refs" (vec @global-refs) })
          (when (not-empty @prop-refs)
            { "props-refs" (vec @prop-refs) })
          (when (not-empty @state-refs)
            { "state-refs" (vec @state-refs) }))
        class-def
        `(let [cls# (.createClass js/React
                      (cljs.core/js-obj
                        ~@(apply concat hooks)))]
            (def ~(vary-meta class-name assoc :export true)
              (fn [& args#]
                (.createElement js/React cls# (aspis.core/args-to-props args#)))))]
    class-def))

(defmacro defqueue [queue-name & body]
  `(def ~queue-name
     (let [queue# (cljs.core/array)]
       (aset queue# "init!" (fn [] ~@body))
       queue#)))

(defmacro enqueue [queue & body]
  `(do
     (when-let [init!# (aget ~queue "init!")]
       (aset ~queue "init!" nil)
       (init!#))
     (.push ~queue (fn [] ~@body))))

(def ^:private passthru-queue
  '(js* "{ push: function() { for(var i = 0; i < arguments.length; i++) { if(typeof arguments[i] === 'function') { arguments[i](); } } } }"))

(defmacro open! [queue]
  `(let [old-queue# ~queue]
     (set! ~queue ~passthru-queue)
     (.apply (aget ~queue "push") ~queue old-queue#)))

(defmacro set-state! [this & kvs]
  `(.setState ~this
      (cljs.core/js-obj
        ~@(mapcat (fn [[k v]] [(name k) v]) (partition 2 kvs)))))

(defn- to-js [form]
  (cond
    (string? form) form
    (number? form) form
    (map? form)    `(cljs.core/js-obj ~@(mapcat (fn [[k v]] [(name k) v]) form))
    :else          `(cljs.core/clj->js ~form)))

(defmulti set-property! (fn [props pname pvalue] pname))
(defmethod set-property! :default [p key value] (assoc p (name key) value))
(defmethod set-property! :html [p _ value] (assoc p "dangerouslySetInnerHTML" `(cljs.core/js-obj "__html" (str ~value))))
(defmethod set-property! :css [p _ value] (assoc p "style" (to-js value)))
(defmethod set-property! :style [p _ value] (assoc p "style" (to-js value)))
(defmethod set-property! :styles [p _ value]
  (assoc p "style"
    (if (vector? value)
      `(aspis.core/styles-to-style (cljs.core/array ~@value))
      `(aspis.core/styles-to-style ~value))))
(defmethod set-property! :class [p _ value]
  (assoc p "className"
    (if (string? value)
      value
      `(aspis.core/class-to-className ~value))))
(defmethod set-property! :classes [p _ value]
  (assoc p "className"
    (if (vector? value)
      `(.join (cljs.core/array ~@value) " ")
      `(aspis.core/classes-to-className ~value))))
(defmethod set-property! :children [p _ value]
  (assoc p "children"
    (cond
      (and (vector? value) (= 1 (count value)))
        (first value)
      (vector? value)
        `(cljs.core/array ~@value)
      :else
        `(aspis.core/to-react-children ~value))))

(defn- args-to-props [args]
  (let [first-keyword?  (comp keyword? first)
        pairs           (partition-all 2 args)
        props           (->> pairs (take-while first-keyword?) (map vec) (into {}))
        children        (->> pairs (drop-while first-keyword?) (mapcat identity))
        props           (if (not-empty children)
                          (assoc props :children (vec children))
                          props)
        react-props     (reduce-kv set-property! {} props)]
    `(cljs.core/js-obj ~@(mapcat identity react-props))))

(defn element [tag-name args]
  (cond
    (empty? args)
      `(.createElement js/React ~tag-name nil)
    (keyword? (first args))
      `(.createElement js/React ~tag-name ~(args-to-props args))
    :else
      `(.createElement js/React ~tag-name nil ~@args)))

(defmacro CSSTransitionGroup [& args] (element 'js/React.addons.CSSTransitionGroup args))
(defmacro TransitionGroup [& args] (element 'js/React.addons.TransitionGroup args))

(defmacro a [& args] (element "a" args))
(defmacro abbr [& args] (element "abbr" args))
(defmacro address [& args] (element "address" args))
(defmacro area [& args] (element "area" args))
(defmacro article [& args] (element "article" args))
(defmacro aside [& args] (element "aside" args))
(defmacro audio [& args] (element "audio" args))
(defmacro b [& args] (element "b" args))
(defmacro base [& args] (element "base" args))
(defmacro bdi [& args] (element "bdi" args))
(defmacro bdo [& args] (element "bdo" args))
(defmacro big [& args] (element "big" args))
(defmacro blockquote [& args] (element "blockquote" args))
(defmacro body [& args] (element "body" args))
(defmacro br [& args] (element "br" args))
(defmacro button [& args] (element "button" args))
(defmacro canvas [& args] (element "canvas" args))
(defmacro caption [& args] (element "caption" args))
(defmacro circle [& args] (element "circle" args))
(defmacro cite [& args] (element "cite" args))
(defmacro code [& args] (element "code" args))
(defmacro col [& args] (element "col" args))
(defmacro colgroup [& args] (element "colgroup" args))
(defmacro data [& args] (element "data" args))
(defmacro datalist [& args] (element "datalist" args))
(defmacro dd [& args] (element "dd" args))
(defmacro defs [& args] (element "defs" args))
(defmacro del [& args] (element "del" args))
(defmacro details [& args] (element "details" args))
(defmacro dfn [& args] (element "dfn" args))
(defmacro div [& args] (element "div" args))
(defmacro dl [& args] (element "dl" args))
(defmacro dt [& args] (element "dt" args))
(defmacro ellipse [& args] (element "ellipse" args))
(defmacro em [& args] (element "em" args))
(defmacro embed [& args] (element "embed" args))
(defmacro fieldset [& args] (element "fieldset" args))
(defmacro figcaption [& args] (element "figcaption" args))
(defmacro figure [& args] (element "figure" args))
(defmacro footer [& args] (element "footer" args))
(defmacro form [& args] (element "form" args))
(defmacro g [& args] (element "g" args))
(defmacro h1 [& args] (element "h1" args))
(defmacro h2 [& args] (element "h2" args))
(defmacro h3 [& args] (element "h3" args))
(defmacro h4 [& args] (element "h4" args))
(defmacro h5 [& args] (element "h5" args))
(defmacro h6 [& args] (element "h6" args))
(defmacro head [& args] (element "head" args))
(defmacro header [& args] (element "header" args))
(defmacro hr [& args] (element "hr" args))
(defmacro html [& args] (element "html" args))
(defmacro html-map [& args] (element "html-map" args))
(defmacro html-meta [& args] (element "html-meta" args))
(defmacro html-time [& args] (element "html-time" args))
(defmacro i [& args] (element "i" args))
(defmacro iframe [& args] (element "iframe" args))
(defmacro img [& args] (element "img" args))
(defmacro input [& args] (element "input" args))
(defmacro ins [& args] (element "ins" args))
(defmacro kbd [& args] (element "kbd" args))
(defmacro keygen [& args] (element "keygen" args))
(defmacro label [& args] (element "label" args))
(defmacro legend [& args] (element "legend" args))
(defmacro li [& args] (element "li" args))
(defmacro line [& args] (element "line" args))
(defmacro linearGradient [& args] (element "linearGradient" args))
(defmacro link [& args] (element "link" args))
(defmacro main [& args] (element "main" args))
(defmacro mark [& args] (element "mark" args))
(defmacro menu [& args] (element "menu" args))
(defmacro menuitem [& args] (element "menuitem" args))
(defmacro meter [& args] (element "meter" args))
(defmacro nav [& args] (element "nav" args))
(defmacro noscript [& args] (element "noscript" args))
(defmacro object [& args] (element "object" args))
(defmacro ol [& args] (element "ol" args))
(defmacro optgroup [& args] (element "optgroup" args))
(defmacro option [& args] (element "option" args))
(defmacro output [& args] (element "output" args))
(defmacro p [& args] (element "p" args))
(defmacro param [& args] (element "param" args))
(defmacro path [& args] (element "path" args))
(defmacro pattern [& args] (element "pattern" args))
(defmacro polygon [& args] (element "polygon" args))
(defmacro polyline [& args] (element "polyline" args))
(defmacro pre [& args] (element "pre" args))
(defmacro progress [& args] (element "progress" args))
(defmacro q [& args] (element "q" args))
(defmacro radialGradient [& args] (element "radialGradient" args))
(defmacro rect [& args] (element "rect" args))
(defmacro rp [& args] (element "rp" args))
(defmacro rt [& args] (element "rt" args))
(defmacro ruby [& args] (element "ruby" args))
(defmacro s [& args] (element "s" args))
(defmacro samp [& args] (element "samp" args))
(defmacro script [& args] (element "script" args))
(defmacro section [& args] (element "section" args))
(defmacro select [& args] (element "select" args))
(defmacro small [& args] (element "small" args))
(defmacro source [& args] (element "source" args))
(defmacro span [& args] (element "span" args))
(defmacro stop [& args] (element "stop" args))
(defmacro strong [& args] (element "strong" args))
(defmacro style [& args] (element "style" args))
(defmacro sub [& args] (element "sub" args))
(defmacro summary [& args] (element "summary" args))
(defmacro sup [& args] (element "sup" args))
(defmacro svg [& args] (element "svg" args))
(defmacro svg-mask [& args] (element "svg-mask" args))
(defmacro svg-text [& args] (element "svg-text" args))
(defmacro svg-var [& args] (element "svg-var" args))
(defmacro table [& args] (element "table" args))
(defmacro tbody [& args] (element "tbody" args))
(defmacro td [& args] (element "td" args))
(defmacro textarea [& args] (element "textarea" args))
(defmacro tfoot [& args] (element "tfoot" args))
(defmacro th [& args] (element "th" args))
(defmacro thead [& args] (element "thead" args))
(defmacro title [& args] (element "title" args))
(defmacro tr [& args] (element "tr" args))
(defmacro track [& args] (element "track" args))
(defmacro tspan [& args] (element "tspan" args))
(defmacro u [& args] (element "u" args))
(defmacro ul [& args] (element "ul" args))
(defmacro video [& args] (element "video" args))
(defmacro wbr [& args] (element "wbr" args))

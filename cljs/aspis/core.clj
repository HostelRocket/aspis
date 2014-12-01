(ns aspis.core
  (:require
    [clojure.walk   :as walk]
    [cljs.analyzer  :as a]))

(def ^:private react-elements
  '[a abbr address area article aside audio b base bdi bdo big blockquote body br button canvas caption cite code col colgroup data datalist dd del details dfn div dl dt em embed fieldset figcaption figure footer form h1 h2 h3 h4 h5 h6 head header hr html i iframe img input ins kbd keygen label legend li link main mark menu menuitem meter nav noscript object ol optgroup option output p param pre progress q rp rt ruby s samp script section select small source span strong style sub summary sup table tbody td textarea tfoot th thead title tr track u ul video wbr circle defs ellipse g line linearGradient path pattern polygon polyline radialGradient rect stop svg tspan])

(def ^:private react-dom-syms
  (set (conj react-elements
             'html-map 'html-meta 'html-time
             'svg-mask 'svg-var 'svg-text
             'CSSTransitionGroup 'TransitionGroup)))

(defn- insert-this [env form]
  (if (seq? form)
    (let [expanded (a/macroexpand-1 env form)]
      (if (and (sequential? expanded)
               (= 'fn* (first expanded)))
        (let [arities (rest expanded)
              fname   (when (symbol? (first arities)) [(first arities)])
              arities (if fname (rest arities) arities)
              arities (if (vector? (first arities)) [arities] arities)
              arities (map (fn [[bind & body]] (list bind `(cljs.core/this-as ~(symbol "this") ~@body))) arities)]
          (cons 'fn* (concat fname arities)))
        form))
    form))

(defmacro defelem [class-name & forms]
  (let [references (atom #{})
        spec
        (->>
          (if (keyword? (first forms))
            forms
            (cons :render forms))
          (partition 2)
          (map (fn [[k v]] [(name k) (insert-this &env v)]))
          (into {})
          (walk/prewalk
            (fn [x]
              (cond
                (contains? react-dom-syms x)
                  (symbol "aspis.core" (name x))
                (and (list? x)
                     (or
                       (= (symbol "clojure.core" "deref") (first x))
                       (= (symbol "deref") (first x))))
                  (do
                    (if (and (symbol? (second x)) (= "this" (namespace (second x))))
                      (when (.startsWith (name (second x)) "props.")
                        (swap! references conj (subs (name (second x)) 6)))
                      (swap! references conj (second x)))
                    x)
                (and (symbol? x) (= "this" (namespace x)))
                  `(aget ~(symbol "this")
                     ~@(clojure.string/split (name x) #"\."))
                :else x))))
        methods (assoc spec
                  "render"
                  `(fn []
                     (cljs.core/this-as ~(symbol "this")
                        ~(get spec "render"))))
        hooks (merge
                methods
                {"mixins" '(cljs.core/array aspis.core/aspis-mixin)
                 "displayName" (str (ns-name *ns*) "." class-name)})
        class-def
        `(let [cls# (.createClass js/React
                      (cljs.core/js-obj
                        ~@(apply concat hooks)))]
           (def ~(vary-meta class-name assoc :export true)
             (fn [& args#]
               (let [argmap#  (aspis.core/parse-arglist args#)
                     props#   (cljs.core/reduce-kv
                                (fn [o# k# v#]
                                  (cljs.core/aset o# (cljs.core/name k#) v#) o#) (cljs.core/js-obj)
                                argmap#)]
                 (aset props# "cljs-refs" ~(vec @references))
                 (.createElement js/React cls# props#)))))]
    class-def))

(defmacro with-init! [& body]
  `(.push aspis.core/*initfns* (fn [] ~@body)))

(def ^:private passthru-queue
  '(js* "{ push: function() { for(var i = 0; i < arguments.length; i++) { if(typeof arguments[i] === 'function') { arguments[i](); } } } }"))

(defmacro open-queue! [queue]
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

(defn- args-to-proplist [args]
  (let [first-keyword?  (comp keyword? first)
        pairs           (partition-all 2 args)
        props           (->> pairs (take-while first-keyword?) (map vec) (into {}))
        children        (->> pairs (drop-while first-keyword?) (mapcat identity))
        props           (if (not-empty children)
                          (assoc props :children
                            (if (= 1 (count children))
                              (first children)
                              (vec children)))
                          props)]
    (mapcat
      (fn [[k v]]
        (condp = k
          :css    ["style" (to-js v)]
          :html   ["dangerouslySetInnerHTML" `(cljs.core/js-obj "__html" (str ~v))]
          :class  ["className"
                   (if (map? v)
                     `(.classSet js/React.addons ~(to-js v))
                     (to-js v))]
          :children ["children"
                     (cond
                       (string? v)
                         v
                       (and (seq? v) (= 'aspis.core/to-react-children (first v)))
                         v
                       (and (seq? v)
                            (symbol? (first v))
                            (= "aspis.core" (namespace (first v)))
                            (contains? react-dom-syms (symbol (name (first v)))))
                         v
                      :else
                       `(aspis.core/to-react-children ~v))]
          [(name k) v]))
      props)))

(defn element [tag-name args]
  (cond
    (nil? args)
      `(.createElement js/React ~tag-name nil)
    (keyword? (first args))
      `(.createElement js/React ~tag-name (cljs.core/js-obj ~@(args-to-proplist args)))
    (or (string? (first args)) (number? (first args)))
      `(.createElement js/React ~tag-name nil ~@args)
    :else
      `(aspis.core/tag ~tag-name ~(vec args))))

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

(ns aspis-test
  (:require
    [aspis.core :as a :include-macros true]))

(def seconds (atom 0))

(a/defelem ticker
  :getInitialState
  (fn [] #js {:half-seconds (atom 0)})
  :componentWillMount
  (fn []
    (.setInterval js/window #(swap! this/state.half-seconds inc) 500))
  :render
  (div
    (p :children ["Half seconds elapsed: " (span @this/state.half-seconds)])
    (p :merge this/props :children this/props.children)))

(a/defelem banner
  (div
    (h1 :classes
        [
          (if (odd? @seconds) "red") ;; conditional classes
          "underline" ;; unconditional classes
        ]
        :styles
        [
          (if (even? @seconds) {:color "green"}) ;; conditional styles
          {:backgroundColor "yellow"} ;; unconditional styles as CLJS map
          #js {:border "1px dotted #aaa"} ;; styles as JS object
        ]
      (or this/props.message "Hello, world!"))
    (ticker :class "underline" "Am I underlined" "?")))

(.setInterval js/window #(swap! seconds inc) 1000)
(.render js/React (banner :message "¡Buenos dias, mundo!") (.getElementById js/document "content"))

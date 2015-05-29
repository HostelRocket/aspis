(ns aspis-test
  (:require
    [aspis.core :as a :include-macros true]))

(def seconds (atom 0))

(a/defelem ticker
           {:keys [children] :as props}
           [half-seconds 0]
  :componentWillMount
  (fn []
    (.setInterval js/window #(swap! half-seconds inc) 500))
  :render
  (div
    (p :children ["Half seconds elapsed: " (span @half-seconds)])
    (div :styles [{:color "red"} this/props.style] "Should not be red")
    (a/with-props (p :children children) props)))

(a/defelem banner
           {sec :seconds
            :keys [message]
            :or {message "Nothing passed in"}}
  (div
    (h1 :classes
        [(if (odd? @sec) "red")
         "underline"]
        :styles
        [(if (even? @sec) {:color "green"})
         {:backgroundColor "yellow"}
         #js {:border "1px dotted #aaa"}]
      message)
    (ticker :class "underline" :css {:color "blue"} "Am I underlined" "?")))

(.setInterval js/window #(swap! seconds inc) 1000)
(.render js/React (banner :seconds seconds) (.getElementById js/document "content"))

(ns aspis-test
  (:require
    [aspis.core :as a :include-macros true]))

(def seconds (atom 0))

(a/defelem banner
  (div
    (h1 :css {:color "green"}
      (or this/props.message "Hello, world!"))
    (p "Seconds elapsed: " @seconds)))

(.setInterval js/window #(swap! seconds inc) 1000)
(.render js/React (banner :message "¡Buenos dias, mundo!") (.getElementById js/document "content"))

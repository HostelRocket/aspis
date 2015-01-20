(defproject hostelrocket/aspis "0.4"
  :description "ClojureScript framework for React.js webapps"
  :url "https://github.com/HostelRocket/aspis"
  :license
  { :name "Eclipse Public License"
    :url "http://www.eclipse.org/legal/epl-v10.html" }

  :dependencies
  [ [org.clojure/clojure "1.6.0"]
    [org.clojure/core.incubator "0.1.3"] ]
  :source-paths ["cljs"]

  :profiles
  { :dev
    { :plugins
      [ [lein-cljsbuild "1.0.3"] ]
      :dependencies
      [ [org.clojure/clojure "1.6.0"]
        [org.clojure/clojurescript "0.0-2665"] ] }}

  :cljsbuild
  { :builds
    { :test-ws
      { :source-paths ["cljs" "test"]
        :compiler
        { :output-to "target/test.ws.js"
          :output-dir "target/ws"
          :externs
          [ "react-with-addons.js" ]
          :closure-warnings
          { :externs-validation :off
            :non-standard-jsdoc :off }
          :optimizations :whitespace
          :pretty-print true }}
      :test-adv
      { :source-paths ["cljs" "test"]
        :compiler
        { :output-to "target/test.adv.js"
          :output-dir "target/adv"
          :externs
          [ "react-with-addons.js" ]
          :closure-warnings
          { :externs-validation :off
            :non-standard-jsdoc :off }
          :optimizations :advanced
          :static-fns true
          :pretty-print false
          :elide-asserts true }}}})

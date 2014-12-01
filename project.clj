(defproject hostelrocket/aspis "0.3-SNAPSHOT"
  :description "ClojureScript framework for React.js webapps"
  :url "https://github.com/HostelRocket/aspis"
  :license
  { :name "Eclipse Public License"
    :url "http://www.eclipse.org/legal/epl-v10.html" }

  :dependencies [[org.clojure/clojure "1.6.0"]]
  :source-paths ["cljs"]

  :profiles
  { :dev
    { :plugins
      [ [lein-cljsbuild "1.0.3"] ]
      :dependencies
      [ [org.clojure/clojure "1.6.0"]
        [org.clojure/clojurescript "0.0-2371"] ] }}

  :cljsbuild
  { :builds
    { :aspis-test
      { :source-paths ["cljs" "test"]
        :compiler
        { :output-to "target/test.js"
          :output-dir "target"
          :externs
          [ "react-with-addons.js" ]
          :closure-warnings
          { :externs-validation :off
            :non-standard-jsdoc :off }
          :optimizations :whitespace
          :pretty-print true }}}})

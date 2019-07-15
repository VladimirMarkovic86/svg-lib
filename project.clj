(defproject org.clojars.vladimirmarkovic86/svg-lib "0.1.8"
  :description "Scalable Vector Graphics library"
  :url "http://github.com/VladimirMarkovic86/svg-lib"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.339"]
                 [org.clojars.vladimirmarkovic86/htmlcss-lib "0.1.7"]
                 [org.clojars.vladimirmarkovic86/js-lib "0.1.17"]
                 [org.clojars.vladimirmarkovic86/utils-lib "0.4.11"]
                 ]

  :min-lein-version "2.0.0"
  
  :source-paths ["src/cljs"]

  :plugins [[lein-cljsbuild  "1.1.7"]
            [lein-doo "0.1.11"]
            ]

  :cljsbuild
    {:builds
      {:test
        {:source-paths ["src/cljs" "test/cljs"]
         :compiler     {:main svg-lib.test-runner
                        :optimizations :whitespace
                        :output-dir "resources/public/assets/js/out/test"
                        :output-to "resources/public/assets/js/test.js"}}
       }}
 )


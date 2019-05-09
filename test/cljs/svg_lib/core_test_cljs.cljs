(ns svg-lib.core-test-cljs
  (:require [clojure.test :refer-macros [deftest is testing]]
            [utils-lib.core :as utils]
            [svg-lib.chart.core :refer [axis-segment calculate-segment-value
                                        generate-polylines generate-polyline
                                        build-line-chart-clj-map find-x-y-min-max
                                        format-segment-value bar-axis-segment
                                        bar-min-max-iterate-coordinates
                                        find-bar-x-y-min-max generate-bar
                                        generate-bars build-bar-chart-clj-map
                                        calculate-clip-path generate-pie-slices
                                        build-pie-chart-clj-map]]))

(deftest test-calculate-segment-value
  (testing "Test calculate segment value"
    
    (let [lower-limit nil
          higher-limit nil
          result (calculate-segment-value
                   lower-limit
                   higher-limit)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [lower-limit 16
          higher-limit 33
          result (calculate-segment-value
                   lower-limit
                   higher-limit)]
      
      (is
        (= result
           20)
       )
      
     )
    
    (let [lower-limit 166
          higher-limit 333
          result (calculate-segment-value
                   lower-limit
                   higher-limit)]
      
      (is
        (= result
           200)
       )
      
     )
    
   ))

(deftest test-axis-segment
  (testing "Test axis segment"
    
    (let [min-value nil
          max-value nil
          width nil
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [min-value 0
          max-value 500
          width 300
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[0 0]
            [100 60]
            [200 120]
            [300 180]
            [400 240]
            [500 300]])
       )
      
     )
    
    (let [min-value 200
          max-value 500
          width 300
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[200 0] [300 100] [400 200] [500 300]])
       )
      
     )
    
    (let [min-value 0
          max-value 50
          width 300
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[0 0] [10 60] [20 120] [30 180] [40 240] [50 300]])
       )
      
     )
    
    (let [min-value 0
          max-value 500
          width 30
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[0 0]])
       )
      
     )
    
    (let [min-value 0
          max-value 5000
          width 300
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[0 0] [1000 60] [2000 120] [3000 180] [4000 240] [5000 300]])
       )
      
     )
    
    (let [min-value 0
          max-value 5000000
          width 300
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[0 0]
            [1000000 60]
            [2000000 120]
            [3000000 180]
            [4000000 240]
            [5000000 300]])
       )
      
     )
    
    (let [min-value 0
          max-value 400
          width 300
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[0 0] [100 75] [200 150] [300 225] [400 300]])
       )
      
     )
    
    (let [min-value 0
          max-value 300
          width 300
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[0 0] [100 100] [200 200] [300 300]])
       )
      
     )
    
    (let [min-value 0
          max-value 200
          width 300
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[0 0] [50 75] [100 150] [150 225] [200 300]])
       )
      
     )
    
    (let [november-2018 (js/Date.
                          "2018-11-01")
          july-2019 (js/Date.
                      "2019-07-01")
          min-value (.getTime
                      november-2018)
          max-value (.getTime
                      july-2019)
          width 500
          result (axis-segment
                   min-value
                   max-value
                   width)]
      
      (is
        (= result
           [[1541030400000 0]
            [1545030400000 100]
            [1549030400000 200]
            [1553030400000 300]
            [1557030400000 400]
            [1561030400000 500]])
       )
      
     )
    
   ))

(deftest test-find-x-y-min-max
  (testing "Test find x y min max"
    
    (let [dot-values nil
          result (find-x-y-min-max
                   dot-values)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [dot-values []
          result (find-x-y-min-max
                   dot-values)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [dot-values [1 2 3]
          result (find-x-y-min-max
                   dot-values)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [dot-values [[1 1] [2 2] [3 3]]
          result (find-x-y-min-max
                   dot-values)]
      
      (is
        (= result
           [1 3 1 3])
       )
      
     )
    
    (let [dot-values [[20 100] [40 200] [60 100]]
          result (find-x-y-min-max
                   dot-values)]
      
      (is
        (= result
           [20 60 100 200])
       )
      
     )
    
   ))

(deftest test-format-segment-value
  (testing "Test format segment value"
    
    (let [segment-value nil
          segment-value-type nil
          selected-language nil
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [segment-value 1
          segment-value-type nil
          selected-language nil
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "1")
       )
      
     )
    
    (let [segment-value -1
          segment-value-type nil
          selected-language nil
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "-1")
       )
      
     )
    
    (let [segment-value 1000
          segment-value-type nil
          selected-language nil
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "1K")
       )
      
     )
    
    (let [segment-value -1000
          segment-value-type nil
          selected-language nil
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "-1K")
       )
      
     )
    
    (let [segment-value 1000
          segment-value-type nil
          selected-language "serbian"
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "1H")
       )
      
     )
    
    (let [segment-value -1000
          segment-value-type nil
          selected-language "serbian"
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "-1H")
       )
      
     )
    
    (let [segment-value 1000000
          segment-value-type nil
          selected-language nil
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "1M")
       )
      
     )
    
    (let [segment-value -1000000
          segment-value-type nil
          selected-language nil
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "-1M")
       )
      
     )
    
    (let [january-2019 (js/Date.
                         "2019-01-01")
          segment-value january-2019
          segment-value-type "date"
          selected-language nil
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "2019-01-01")
       )
      
     )
    
    (let [january-2019 (js/Date.
                         "2019-01-01")
          segment-value january-2019
          segment-value-type "date"
          selected-language "serbian"
          result (format-segment-value
                   segment-value
                   segment-value-type
                   selected-language)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "01.01.2019")
       )
      
     )
    
   ))

(deftest test-generate-polyline
  (testing "Test generate polyline"
    
    (let [dot-values nil
          dots-as-circle nil
          calculate-x-coordinate nil
          x-left-offset nil
          height nil
          y-bottom-offset nil
          calculate-y-coordinate nil
          itr nil
          result (generate-polyline
                   dot-values
                   dots-as-circle
                   calculate-x-coordinate
                   x-left-offset
                   height
                   y-bottom-offset
                   calculate-y-coordinate
                   itr)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [dot-values [[20 30] [30 30] [40 50]
                      [50 30] [60 30] [70 30]]
          dots-as-circle (atom [])
          calculate-x-coordinate (fn [param] param)
          x-left-offset 0
          height 0
          y-bottom-offset 0
          calculate-y-coordinate (fn [param] param)
          itr 0
          result (generate-polyline
                   dot-values
                   dots-as-circle
                   calculate-x-coordinate
                   x-left-offset
                   height
                   y-bottom-offset
                   calculate-y-coordinate
                   itr)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (map?
          result)
       )
      
      (is
        (= result
           {:el "polyline",
            :events nil,
            :attrs {:points "20,-30 30,-30 40,-50 50,-30 60,-30 70,-30 ",
                    :class "chart-line",
                    :id "chart-line-0"},
            :dynamic-attrs nil,
            :cont nil})
       )
      
     )
    
   ))

(deftest test-generate-polylines
  (testing "Test generate polylines"
    
    (let [dot-values nil
          multi-line nil
          calculate-x-coordinate nil
          x-left-offset nil
          height nil
          y-bottom-offset nil
          calculate-y-coordinate nil
          result (generate-polylines
                   dot-values
                   multi-line
                   calculate-x-coordinate
                   x-left-offset
                   height
                   y-bottom-offset
                   calculate-y-coordinate)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [dot-values [[20 30] [30 30] [40 50]
                      [50 30] [60 30] [70 30]]
          multi-line nil
          calculate-x-coordinate (fn [param] param)
          x-left-offset 0
          height 0
          y-bottom-offset 0
          calculate-y-coordinate (fn [param] param)
          [polyline-vector-result
           dots-as-circle-result] (generate-polylines
                                    dot-values
                                    multi-line
                                    calculate-x-coordinate
                                    x-left-offset
                                    height
                                    y-bottom-offset
                                    calculate-y-coordinate)]
      
      (is
        (vector?
          polyline-vector-result)
       )
      
      (is
        (= polyline-vector-result
           [{:el "polyline",
             :events nil,
             :attrs {:points "20,-30 30,-30 40,-50 50,-30 60,-30 70,-30 ",
                     :class "chart-line",
                     :id "chart-line-0"},
             :dynamic-attrs nil,
             :cont nil}])
       )
      
      (is
        (vector?
          dots-as-circle-result)
       )
      
      (is
        (= (count
             dots-as-circle-result)
           6)
       )
      
     )
    
   ))

(deftest test-build-line-chart-clj-map
  (testing "Test build line chart clj map"
    
    (let [dot-values nil
          result (build-line-chart-clj-map
                   nil)]
      (is
        (nil?
          result)
       )
            
     )
    
    (let [dot-values [[20 100] [30 110] [40 200]
                      [50 140] [60 100]]
          result (build-line-chart-clj-map
                   {:dot-values dot-values})]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (map?
          result)
       )
      
     )
    
    (let [november-2018 (js/Date.
                          "2018-11-01")
          december-2018 (js/Date.
                          "2018-12-01")
          january-2019 (js/Date.
                         "2019-01-01")
          february-2019 (js/Date.
                         "2019-02-01")
          march-2019 (js/Date.
                       "2019-03-01")
          april-2019 (js/Date.
                       "2019-04-01")
          may-2019 (js/Date.
                     "2019-05-01")
          jun-2019 (js/Date.
                     "2019-06-01")
          july-2019 (js/Date.
                      "2019-07-01")
          result (build-line-chart-clj-map
                   {:dot-values [[november-2018 1]
                                 [december-2018 2]
                                 [january-2019 3]
                                 [february-2019 4]
                                 [march-2019 5]
                                 [april-2019 6]
                                 [may-2019 7]
                                 [jun-2019 8]
                                 [july-2019 9]]
                    :x-value-type "date"
                    :main-title "Main title"
                    :x-axis-title "X title"
                    :y-axis-title "Y title"
                    :horizontal-grid-lines true})]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (map?
          result)
       )
      
     )
    
   ))

(deftest test-bar-axis-segment
  (testing "Test bar axis segment"
    
    (let [bar-labels nil
          width nil
          result (bar-axis-segment
                   bar-labels
                   width)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [bar-labels []
          width 200
          result (bar-axis-segment
                   bar-labels
                   width)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [bar-labels ["data 1" "data 2" "data 3"]
          width 200
          result (bar-axis-segment
                   bar-labels
                   width)]
      
      (is
        (= result
           [["data 1" 33]
            ["data 2" 99]
            ["data 3" 165]])
       )
      
     )
    
    (let [bar-labels ["data 1" "data 2" "data 3"
                      "data 4" "data 5"]
          width 200
          result (bar-axis-segment
                   bar-labels
                   width)]
      
      (is
        (= result
           [["data 1" 20]
            ["data 2" 60]
            ["data 3" 100]
            ["data 4" 140]
            ["data 5" 180]
            ])
       )
      
     )
    
   ))

(deftest test-bar-min-max-iterate-coordinates
  (testing "Test bar min max iterate coordinates"
    
    (let [bar-values nil
          axis-min nil
          axis-max nil
          result (bar-min-max-iterate-coordinates
                   bar-values
                   axis-min
                   axis-max)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [bar-values []
          axis-min (atom
                     (aget
                       js/Number
                       "MAX_SAFE_INTEGER"))
          axis-max (atom
                     (aget
                       js/Number
                       "MIN_SAFE_INTEGER"))
          result (bar-min-max-iterate-coordinates
                   bar-values
                   axis-min
                   axis-max)]
      
      (is
        (= @axis-min
           (aget
             js/Number
             "MAX_SAFE_INTEGER"))
       )
      
      (is
        (= @axis-max
           (aget
             js/Number
             "MIN_SAFE_INTEGER"))
       )
      
     )
    
    (let [bar-values [20 50 30]
          axis-min (atom
                     (aget
                       js/Number
                       "MAX_SAFE_INTEGER"))
          axis-max (atom
                     (aget
                       js/Number
                       "MIN_SAFE_INTEGER"))
          result (bar-min-max-iterate-coordinates
                   bar-values
                   axis-min
                   axis-max)]
      
      (is
        (= @axis-min
           20)
       )
      
      (is
        (= @axis-max
           50)
       )
      
     )
    
   ))

(deftest test-find-bar-x-y-min-max
  (testing "Test find bar x y min max"
    
    (let [bar-values nil
          multi-bars nil
          bar-values-on-x-axis nil
          result (find-bar-x-y-min-max
                   bar-values
                   multi-bars
                   bar-values-on-x-axis)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [bar-values []
          multi-bars nil
          bar-values-on-x-axis nil
          result (find-bar-x-y-min-max
                   bar-values
                   multi-bars
                   bar-values-on-x-axis)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [bar-values [10 20 30]
          multi-bars nil
          bar-values-on-x-axis nil
          result (find-bar-x-y-min-max
                   bar-values
                   multi-bars
                   bar-values-on-x-axis)]
      
      (is
        (= result
           [0 (aget
                js/Number
                "MIN_SAFE_INTEGER")
            0 30])
       )
      
     )
    
    (let [bar-values [10 20 30]
          multi-bars nil
          bar-values-on-x-axis true
          result (find-bar-x-y-min-max
                   bar-values
                   multi-bars
                   bar-values-on-x-axis)]
      
      (is
        (= result
           [0 30
            0 (aget
                js/Number
                "MIN_SAFE_INTEGER")])
       )
      
     )
    
    (let [bar-values [[10 20 30]
                      [30 40 50]]
          multi-bars true
          bar-values-on-x-axis true
          result (find-bar-x-y-min-max
                   bar-values
                   multi-bars
                   bar-values-on-x-axis)]
      
      (is
        (= result
           [0 50
            0 (aget
                js/Number
                "MIN_SAFE_INTEGER")])
       )
      
     )
    
    (let [bar-values [[10 20 30]
                      [30 40 50]]
          multi-bars true
          bar-values-on-x-axis false
          result (find-bar-x-y-min-max
                   bar-values
                   multi-bars
                   bar-values-on-x-axis)]
      
      (is
        (= result
           [0 (aget
                js/Number
                "MIN_SAFE_INTEGER")
            0 50])
       )
      
     )
    
   ))

(deftest test-generate-bar
  (testing "Test generate bar"
    
    (let [bar-value nil
          chart-bars nil
          bar-values-on-x-axis nil
          calculate-x-coordinate nil
          calculate-y-coordinate nil
          calculate-width nil
          calculate-height nil
          itr nil
          number-of-bars nil
          value-type nil
          selected-language nil
          result (generate-bar
                   bar-value
                   bar-peek-texts
                   chart-bars
                   bar-values-on-x-axis
                   calculate-x-coordinate
                   calculate-y-coordinate
                   calculate-width
                   calculate-height
                   itr
                   number-of-bars
                   value-type
                   selected-language)]
      
      (is
        (nil?
          chart-bars)
       )
      
     )
    
    (let [bar-value [10 30]
          chart-bars (atom [])
          bar-values-on-x-axis nil
          calculate-x-coordinate (fn []
                                   10)
          calculate-y-coordinate (fn []
                                   10)
          calculate-width (fn []
                            10)
          calculate-height (fn []
                             10)
          itr 0
          number-of-bars 1
          value-type nil
          selected-language nil
          result (generate-bar
                   bar-value
                   chart-bars
                   bar-values-on-x-axis
                   calculate-x-coordinate
                   calculate-y-coordinate
                   calculate-width
                   calculate-height
                   itr
                   number-of-bars
                   value-type
                   selected-language)
          [element1
           element2] @chart-bars
          element1 (dissoc
                     element1
                     :events)
          element2 (dissoc
                     element2
                     :events)]
      
      (is
        (= element1
           {:el "rect",
            :attrs {:width "5",
                    :height "10",
                    :x "13",
                    :y "10",
                    :class "chart-bar chart-bar-0"},
            :dynamic-attrs nil,
            :cont nil})
       )
      
      (is
        (= element2
           {:el "rect",
            :attrs {:width "5",
                    :height "10",
                    :x "13",
                    :y "10",
                    :class "chart-bar chart-bar-0"},
            :dynamic-attrs nil,
            :cont nil})
       )
      
     )
    
   ))

(deftest test-generate-bars
  (testing "Test generate bars"
    
    (let [bar-values nil
          multi-bars nil
          bar-values-on-x-axis nil
          calculate-x-coordinate nil
          calculate-y-coordinate nil
          calculate-width nil
          calculate-height nil
          value-type nil
          selected-language nil
          result (generate-bars
                   bar-values
                   multi-bars
                   bar-values-on-x-axis
                   calculate-x-coordinate
                   calculate-y-coordinate
                   calculate-width
                   calculate-height
                   value-type
                   selected-language)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [bar-values [10 20]
          multi-bars nil
          bar-values-on-x-axis nil
          calculate-x-coordinate (fn []
                                   10)
          calculate-y-coordinate (fn []
                                   10)
          calculate-width (fn []
                            10)
          calculate-height (fn []
                             10)
          value-type nil
          selected-language nil
          result (generate-bars
                   bar-values
                   multi-bars
                   bar-values-on-x-axis
                   calculate-x-coordinate
                   calculate-y-coordinate
                   calculate-width
                   calculate-height
                   value-type
                   selected-language)
          [element1
           element2] result
          element1 (dissoc
                     element1
                     :events)
          element2 (dissoc
                     element2
                     :events)]
      
      (is
        (= element1
           {:el "rect",
            :attrs {:width "5",
                    :height "10",
                    :x "13",
                    :y "10",
                    :class "chart-bar chart-bar-0"},
            :dynamic-attrs nil,
            :cont nil})
       )
      
      (is
        (= element2
           {:el "rect",
            :attrs {:width "5",
                    :height "10",
                    :x "13",
                    :y "10",
                    :class "chart-bar chart-bar-0"},
            :dynamic-attrs nil,
            :cont nil})
       )
      
     )
    
    (let [bar-values [[10 20]
                      [30 40]]
          multi-bars true
          bar-values-on-x-axis nil
          calculate-x-coordinate (fn []
                                   10)
          calculate-y-coordinate (fn []
                                   10)
          calculate-width (fn []
                            10)
          calculate-height (fn []
                             10)
          value-type nil
          selected-language nil
          result (generate-bars
                   bar-values
                   multi-bars
                   bar-values-on-x-axis
                   calculate-x-coordinate
                   calculate-y-coordinate
                   calculate-width
                   calculate-height
                   value-type
                   selected-language)
          [element1
           element2
           element3
           element4] result
          element1 (dissoc
                     element1
                     :events)
          element2 (dissoc
                     element2
                     :events)
          element3 (dissoc
                     element3
                     :events)
          element4 (dissoc
                     element4
                     :events)]
      
      (is
        (= element1
           {:el "rect",
            :attrs {:width "2",
                    :height "10",
                    :x "13",
                    :y "10",
                    :class "chart-bar chart-bar-0"},
            :dynamic-attrs nil,
            :cont nil})
       )
      
      (is
        (= element2
           {:el "rect",
            :attrs {:width "2",
                    :height "10",
                    :x "13",
                    :y "10",
                    :class "chart-bar chart-bar-0"},
            :dynamic-attrs nil,
            :cont nil})
       )
      
      (is
        (= element3
           {:el "rect",
            :attrs {:width "2",
                    :height "10",
                    :x "15",
                    :y "10",
                    :class "chart-bar chart-bar-1"},
            :dynamic-attrs nil,
            :cont nil})
       )
      
      (is
        (= element4
           {:el "rect",
            :attrs {:width "2",
                    :height "10",
                    :x "15",
                    :y "10",
                    :class "chart-bar chart-bar-1"},
            :dynamic-attrs nil,
            :cont nil})
       )
      
     )
    
   ))

(deftest test-build-bar-chart-clj-map
  (testing "Test build bar chart clj map"
    
    (let [configuration-map nil
          result (build-bar-chart-clj-map
                   configuration-map)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [configuration-map {:bar-values [10 50]
                             :bar-labels ["data 1" "data 5"]
                             :bar-values-on-x-axis true
                             :value-type "number"
                             :multi-bars false
                             :legend {:bar-names ["bar 1"]
                                      :position "right"}
                             :main-title "Bar main title"
                             :x-axis-title "Bar x title"
                             :y-axis-title "Bar y title"
                             :horizontal-grid-lines false
                             :vertical-grid-lines true
                             :svg-width 500
                             :svg-height 500
                             :x-maximum nil
                             :y-maximum nil
                             :selected-language "english"}
          result (build-bar-chart-clj-map
                   configuration-map)]
      
      (let [result-content (:cont result)
            result-modified (dissoc
                              result
                              :cont)
            result-content (utils/remove-index-from-vector
                             result-content
                             15)
            result-content (utils/remove-index-from-vector
                             result-content
                             14)]
        
        (is
          (= result-modified
             {:el "svg",
              :events nil,
              :attrs {:width "500", :height "500"},
              :dynamic-attrs nil})
         )
        
        (is
          (= result-content
             [{:el "line",
	              :events nil,
	              :attrs {:x1 100, :x2 400, :y1 450, :y2 450, :class "x-axis"},
	              :dynamic-attrs nil,
	              :cont nil}
	             {:el "line",
	              :events nil,
	              :attrs {:x1 100, :x2 100, :y1 50, :y2 450, :class "y-axis"},
	              :dynamic-attrs nil,
	              :cont nil}
	             {:el "line",
	              :events nil,
	              :attrs {:x1 100,
	                      :x2 100,
	                      :y1 50,
	                      :y2 455,
	                      :class "x-axis-value-line"},
	              :dynamic-attrs nil,
	              :cont nil}
	             {:el "text",
	              :events nil,
	              :attrs {:x 97, :y 470, :class "x-axis-value-text"},
	              :dynamic-attrs nil,
	              :cont "0"}
	             {:el "line",
	              :events nil,
	              :attrs {:x1 200,
	                      :x2 200,
	                      :y1 50,
	                      :y2 455,
	                      :class "x-axis-value-line"},
	              :dynamic-attrs nil,
	              :cont nil}
	             {:el "text",
	              :events nil,
	              :attrs {:x 192, :y 470, :class "x-axis-value-text"},
	              :dynamic-attrs nil,
	              :cont "20"}
	             {:el "line",
	              :events nil,
	              :attrs {:x1 300,
	                      :x2 300,
	                      :y1 50,
	                      :y2 455,
	                      :class "x-axis-value-line"},
	              :dynamic-attrs nil,
	              :cont nil}
	             {:el "text",
	              :events nil,
	              :attrs {:x 292, :y 470, :class "x-axis-value-text"},
	              :dynamic-attrs nil,
	              :cont "40"}
	             {:el "line",
	              :events nil,
	              :attrs {:x1 400,
	                      :x2 400,
	                      :y1 50,
	                      :y2 455,
	                      :class "x-axis-value-line"},
	              :dynamic-attrs nil,
	              :cont nil}
	             {:el "text",
	              :events nil,
	              :attrs {:x 392, :y 470, :class "x-axis-value-text"},
	              :dynamic-attrs nil,
	              :cont "60"}
	             {:el "line",
	              :events nil,
	              :attrs {:x1 95,
	                      :x2 105,
	                      :y1 350,
	                      :y2 350,
	                      :class "y-axis-value-line"},
	              :dynamic-attrs nil,
	              :cont nil}
	             {:el "text",
	              :events nil,
	              :attrs {:x 95, :y 356, :class "y-axis-value-text"},
	              :dynamic-attrs nil,
	              :cont ""}
	             {:el "line",
	              :events nil,
	              :attrs {:x1 95,
	                      :x2 105,
	                      :y1 150,
	                      :y2 150,
	                      :class "y-axis-value-line"},
	              :dynamic-attrs nil,
	              :cont nil}
	             {:el "text",
	              :events nil,
	              :attrs {:x 95, :y 156, :class "y-axis-value-text"},
	              :dynamic-attrs nil,
	              :cont ""}
	             {:el "text",
	              :events nil,
	              :attrs {:x "250",
	                      :y "20",
	                      :text-anchor "middle",
	                      :class "main-title"},
	              :dynamic-attrs nil,
	              :cont "Bar main title"}
	             {:el "text",
	              :events nil,
	              :attrs {:x "250",
	                      :y "500",
	                      :text-anchor "middle",
	                      :class "x-axis-title"},
	              :dynamic-attrs nil,
	              :cont "Bar x title"}
	             {:el "text",
	              :events nil,
	              :attrs {:x "10",
	                      :y "250",
	                      :transform "rotate(-90 30,250)",
	                      :text-anchor "middle",
	                      :class "y-axis-title"},
	              :dynamic-attrs nil,
	              :cont "Bar y title"}
	             {:el "circle",
	              :events nil,
	              :attrs {:cx "435", :cy "235", :r "5", :id "legend-line-0"},
	              :dynamic-attrs nil,
	              :cont nil}
	             {:el "text",
	              :events nil,
	              :attrs {:x "415", :y "255", :fill "#08c"},
	              :dynamic-attrs nil,
	              :cont "bar 1"}])
         )
        
       )
      
     )
    
   ))

(deftest test-calculate-clip-path
  (testing "Test calculate clip path"
    
    (let [x-start nil
          y-start nil
          x-end nil
          y-end nil
          radius nil
          result (calculate-clip-path
                   x-start
                   y-start
                   x-end
                   y-end
                   radius)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [x-start 200
          y-start 0
          x-end 141
          y-end 141
          radius 200
          result (calculate-clip-path
                   x-start
                   y-start
                   x-end
                   y-end
                   radius)]
      
      (is
        (= result
           [[200 0] [0 0] [141 141] [200 200] [200 0]])
       )
      
     )
    
    (let [x-start 200
          y-start 0
          x-end -141
          y-end 141
          radius 200
          result (calculate-clip-path
                   x-start
                   y-start
                   x-end
                   y-end
                   radius)]
      
      (is
        (= result
           [[200 0] [0 0] [-141 141] [-200 200] [0 200] [200 200] [200 0]])
       )
      
     )
    
    (let [x-start -141
          y-start 141
          x-end -141
          y-end -141
          radius 200
          result (calculate-clip-path
                   x-start
                   y-start
                   x-end
                   y-end
                   radius)]
      
      (is
        (= result
           [[-141 141] [0 0] [-141 -141] [-200 -200] [-200 0] [-200 200] [0 200]])
       )
      
     )
    
    (let [x-start -141
          y-start -141
          x-end 141
          y-end -141
          radius 200
          result (calculate-clip-path
                   x-start
                   y-start
                   x-end
                   y-end
                   radius)]
      
      (is
        (= result
           [[-141 -141]
            [0 0]
            [141 -141]
            [200 -200]
            [0 -200]
            [-200 -200]
            [-200 0]])
       )
      
     )
    
    (let [x-start -141
          y-start -141
          x-end 200
          y-end 0
          radius 200
          result (calculate-clip-path
                   x-start
                   y-start
                   x-end
                   y-end
                   radius)]
      
      (is
        (= result
           [[-141 -141] [0 0] [200 0] [200 -200] [0 -200] [-200 -200] [-200 0]])
       )
      
     )
    
   ))

(deftest test-generate-pie-slices
  (testing "Test generate pie slices"
    
    (let [pie-values nil
          radius nil
          cx nil
          cy nil
          pie-id-num nil
          width nil
          height nil
          value-type nil
          selected-language nil
          result (generate-pie-slices
                   pie-values
                   radius
                   cx
                   cy
                   pie-id-num
                   width
                   height
                   value-type
                   selected-language)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [pie-values [10 20 30]
          radius 200
          cx 225
          cy 225
          pie-id-num "pie-id"
          width 500
          height 500
          value-type "percentage"
          selected-language nil
          [clip-path-vector
           svg-use-list] (generate-pie-slices
                           pie-values
                           radius
                           cx
                           cy
                           pie-id-num
                           width
                           height
                           value-type
                           selected-language)
          svg-use-vector (into
                           []
                           svg-use-list)]
      
      (is
        (= clip-path-vector
           [{:el "clipPath",
             :events nil,
             :attrs {:id "slice-pie-id-0"},
             :dynamic-attrs nil,
             :cont {:el "polygon",
                    :events nil,
                    :attrs {:points "425,225 225,225 325,52 425,25 425,225 "},
                    :dynamic-attrs nil,
                    :cont nil}}
            {:el "clipPath",
             :events nil,
             :attrs {:id "slice-pie-id-1"},
             :dynamic-attrs nil,
             :cont {:el "polygon",
                    :events nil,
                    :attrs {:points "325,52 225,225 25,225 25,25 225,25 425,25 425,225 "},
                    :dynamic-attrs nil,
                    :cont nil}}
            {:el "clipPath",
             :events nil,
             :attrs {:id "slice-pie-id-2"},
             :dynamic-attrs nil,
             :cont {:el "polygon",
                    :events nil,
                    :attrs {:points "25,225 225,225 425,225 425,425 225,425 25,425 25,225 25,25 225,25 "},
                    :dynamic-attrs nil,
                    :cont nil}}])
       )
      
      (let [[{el-1 :el
              {clip-path-1 :clip-path
               class-1 :class} :attrs}
             {el-2 :el
              {clip-path-2 :clip-path
               class-2 :class} :attrs}
             {el-3 :el
              {clip-path-3 :clip-path
               class-3 :class} :attrs}] svg-use-vector]
        
        (is
          (= el-1
             "use")
         )
        
        (is
          (= clip-path-1
             "url(#slice-pie-id-2)")
         )
        
        (is
          (= class-1
             "pie pie-2")
         )
        
        (is
          (= el-2
             "use")
         )
        
        (is
          (= clip-path-2
             "url(#slice-pie-id-1)")
         )
        
        (is
          (= class-2
             "pie pie-1")
         )
        
        (is
          (= el-3
             "use")
         )
        
        (is
          (= clip-path-3
             "url(#slice-pie-id-0)")
         )
        
        (is
          (= class-3
             "pie pie-0")
         )
        
       )
      
     )
    
   ))

(deftest test-build-pie-chart-clj-map
  (testing "Test build pie chart clj map"
    
    (let [configuration-map {:pie-values nil
                             :value-type nil
                             {:bar-names nil
                              :position nil} :legend
                             :main-title nil
                             :svg-width nil
                             :svg-height nil
                             :selected-language nil}
          result (build-pie-chart-clj-map
                   configuration-map)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [configuration-map {:pie-values [10 50 100]
                             :value-type nil
                             {:bar-names nil
                              :position nil} :legend
                             :main-title nil
                             :svg-width nil
                             :svg-height nil
                             :selected-language nil}
          result (build-pie-chart-clj-map
                   configuration-map)
          {result-el :el
           result-attrs :attrs
           result-cont :cont} result]
      
      (is
        (= result-el
           "svg")
       )
      
      (is
        (= result-attrs
           {:width "500", :height "500"})
       )
      
      (let [[{el-1 :el
              {cx-1 :cx
               cy-1 :cy
               r-1 :r} :attrs}
             {el-2 :el
              {el-2-1 :el
               {points-2 :points} :attrs} :cont}
             {el-3 :el
              {el-3-1 :el
               {points-3 :points} :attrs} :cont}
             {el-4 :el
              {el-4-1 :el
               {points-4 :points} :attrs} :cont}] result-cont]
        
        (is
          (= el-1
             "circle")
         )
        
        (is
          (= cx-1
             "250")
         )
        
        (is
          (= cy-1
             "250")
         )
        
        (is
          (= r-1
             "200")
         )
        
        (is
          (= el-2
             "clipPath")
         )
        
        (is
          (= el-2-1
             "polygon")
         )
        
        (is
          (= points-2
             "450,250 250,250 435,176 450,50 450,250 ")
         )
        
        (is
          (= el-3
             "clipPath")
         )
        
        (is
          (= el-3-1
             "polygon")
         )
        
        (is
          (= points-3
             "435,176 250,250 109,109 50,50 250,50 450,50 450,250 ")
         )
        
        (is
          (= el-4
             "clipPath")
         )
        
        (is
          (= el-4-1
             "polygon")
         )
        
        (is
          (= points-4
             "109,109 250,250 450,250 450,450 250,450 50,450 50,250 50,50 250,50 ")
         )
        
       )
             
     )
    
   ))


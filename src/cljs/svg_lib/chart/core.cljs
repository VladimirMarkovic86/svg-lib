(ns svg-lib.chart.core
  (:require [htmlcss-lib.core :refer [gen div svg line text
                                      polyline circle polygon
                                      rect clip-path svg-use]]
            [js-lib.core :as md]
            [utils-lib.core :as utils]))

(defn calculate-segment-value
  "Calculates segment value"
  [lower-limit
   higher-limit]
  (when (and lower-limit
             (number?
               lower-limit)
             higher-limit
             (number?
               higher-limit))
    (let [number-of-digits (- (count
                                (str
                                  higher-limit))
                              1)
          number-base (atom 1)]
      (dotimes [i number-of-digits]
        (swap!
          number-base
          *
          10))
      (let [lower-limit-left-over (- @number-base
                                     (mod
                                       lower-limit
                                       @number-base))
            new-lower-limit (+ lower-limit
                               lower-limit-left-over)
            higher-limit-left-over (mod
                                     higher-limit
                                     @number-base)
            new-higher-limit (- higher-limit
                                higher-limit-left-over)
            generate-range (range
                             new-lower-limit
                             (inc
                               new-higher-limit)
                             @number-base)
            segment-value-i (last
                              generate-range)
            lower-limit-base (long
                               (/ lower-limit
                                  @number-base))
            higher-limit-base (long
                                (/ higher-limit
                                   @number-base))
            segment-base (long
                           (/ (+ higher-limit-base
                                 lower-limit-base
                                 1)
                              2))
            segment-value-ii (* segment-base
                                @number-base)
            result (atom segment-value-i)]
        (when (contains?
                #{1 2 5}
                segment-base)
          (reset!
            result
            segment-value-ii))
        @result))
   ))

(defn axis-segment
  "Calculate segments in pixels and round value for every segment"
  [min-value
   max-value
   width]
  (when (and min-value
             (number?
               min-value)
             max-value
             (number?
               max-value)
             width
             (number?
               width))
    (let [segment-value-length (- max-value
                                  min-value)
          lower-limit (long
                        (/ (* segment-value-length
                              50)
                           width))
          higher-limit (long
                         (/ (* segment-value-length
                               100)
                            width))
          segment-value (calculate-segment-value
                          lower-limit
                          higher-limit)
          segment-value-range (range
                                min-value
                                (inc
                                  max-value)
                                segment-value)
          segment-pixel-value (long
                                (/ width
                                   (long
                                     (/ segment-value-length
                                        segment-value))
                                 ))
          segment-pixel-value-range (range
                                      0
                                      (inc
                                        width)
                                      segment-pixel-value)
          result (atom [])
          map-result (map
                       (fn [segment-value-p
                            segment-pixel-value-p]
                         (swap!
                           result
                           conj
                           [segment-value-p
                            segment-pixel-value-p]))
                       segment-value-range
                       segment-pixel-value-range)]
      (last
        map-result))
   ))

(defn min-max-iterate-coordinates
  "Iterates through coordinates"
  [dot-values
   x-min
   x-max
   y-min
   y-max]
  (when (and dot-values
             (vector?
               dot-values)
             (not
               (empty?
                 dot-values)
              )
             x-min
             (instance?
               Atom
               x-min)
             x-max
             (instance?
               Atom
               x-max)
             y-min
             (instance?
               Atom
               y-min)
             y-max
             (instance?
               Atom
               y-max))
    (doseq [coordinate-pair dot-values]
      (let [[x y] (when (and coordinate-pair
                             (vector?
                               coordinate-pair)
                             (= (count
                                  coordinate-pair)
                                2))
                    coordinate-pair)]
        (let [x-as-number (if (instance?
                                js/Date
                                x)
                            (.getTime
                              x)
                            x)
              y-as-number (if (instance?
                                js/Date
                                y)
                            (.getTime
                              y)
                            y)]
          (when (< x-as-number
                   @x-min)
            (reset!
              x-min
              x-as-number))
          (when (< @x-max
                   x-as-number)
            (reset!
              x-max
              x-as-number))
          (when (< y-as-number
                   @y-min)
            (reset!
              y-min
              y-as-number))
          (when (< @y-max
                   y-as-number)
            (reset!
              y-max
              y-as-number))
         ))
     ))
 )

(defn find-x-y-min-max
  "Finds minimum and maximum of x and y axis"
  [dot-values
   & [multi-line]]
  (when (and dot-values
             (vector?
               dot-values)
             (not
               (empty?
                 dot-values))
         )
    (let [x-min (atom (aget
                        js/Number
                        "MAX_SAFE_INTEGER"))
          x-max (atom (aget
                        js/Number
                        "MIN_SAFE_INTEGER"))
          y-min (atom (aget
                        js/Number
                        "MAX_SAFE_INTEGER"))
          y-max (atom (aget
                        js/Number
                        "MIN_SAFE_INTEGER"))]
      (if multi-line
        (doseq [line-vector dot-values]
          (min-max-iterate-coordinates
            line-vector
            x-min
            x-max
            y-min
            y-max))
        (min-max-iterate-coordinates
          dot-values
          x-min
          x-max
          y-min
          y-max))
      (when (and @x-min
                 @x-max
                 @y-min
                 @y-max)
        [@x-min @x-max
         @y-min @y-max]))
   ))

(defn format-segment-value
  "Formats segment value
   10 -> 10
   1000 -> 1K
   1000000 -> 1M"
  [segment-value
   & [segment-value-type
      selected-language]]
  (let [result (atom "")
        segment-value-type (or segment-value-type
                               "number")]
    (when (and segment-value
               (number?
                 segment-value)
               (= segment-value-type
                  "number"))
      (let [number-of-digits (if (neg?
                                   segment-value)
                               (dec
                                 (count
                                   (str
                                     segment-value))
                                )
                               (count
                                 (str
                                   segment-value))
                              )]
        (when (< number-of-digits
                 4)
          (swap!
            result
            str
            segment-value))
        (when (< 3
                 number-of-digits
                 7)
          (let [formated-number (utils/round-decimals
                                  (double
                                    (/ segment-value
                                       1000))
                                  (if (< 0
                                         (mod segment-value
                                              1000))
                                    1
                                    0))]
            (swap!
              result
              str
              formated-number
              (if (= selected-language
                     "serbian")
                "H"
                "K"))
           ))
        (when (< 6
                 number-of-digits
                 10)
          (let [formated-number (utils/round-decimals
                                  (double
                                    (/ segment-value
                                       1000000))
                                  (if (< 0
                                         (mod segment-value
                                              1000000))
                                    1
                                    0))]
            (swap!
              result
              str
              formated-number
              "M"))
         ))
     )
    (when (and segment-value
               (string?
                 segment-value)
               (= segment-value-type
                  "string"))
      (swap!
        result
        str
        segment-value))
    (when (and segment-value
               (or (string?
                     segment-value)
                   (number?
                     segment-value))
               (= segment-value-type
                  "percentage"))
      (swap!
        result
        str
        segment-value
        "%"))
    (when (and segment-value
               (= segment-value-type
                  "date"))
      (let [current-date (js/Date.)]
        (.setTime
          current-date
          segment-value)
        (swap!
          result
          str
          (let [date-part (let [date-part (str
                                            (.getDate
                                              current-date))
                                date-part-number-of-digits (count
                                                             date-part)]
                            (if (< date-part-number-of-digits
                                   2)
                              (str
                                "0"
                                date-part)
                              date-part))
                month-part (let [month-part (str
                                              (inc
                                                (.getMonth
                                                  current-date))
                                             )
                                 month-part-number-of-digits (count
                                                               month-part)]
                             (if (< month-part-number-of-digits
                                    2)
                               (str
                                 "0"
                                 month-part)
                               month-part))
                year-part (.getFullYear
                            current-date)]
            (if (= selected-language
                   "serbian")
              (str
                date-part
                "."
                month-part
                "."
                year-part)
              (str
                year-part
                "-"
                month-part
                "-"
                date-part))
           ))
       ))
    @result))

(defn append-point-details
  "Append text with details about hovered point"
  [{x-detail :x-detail
    x-position :x-position
    x-value-type :x-value-type
    y-detail :y-detail
    y-position :y-position
    y-value-type :y-value-type
    background-class :background-class
    selected-language :selected-language}
   element
   event]
  (let [svg (.closest
              element
              "svg")
        text-element-content (if y-detail
                               (str
                                 "("
                                 (format-segment-value
                                   x-detail
                                   x-value-type
                                   selected-language)
                                 ","
                                 (format-segment-value
                                   y-detail
                                   y-value-type
                                   selected-language)
                                 ")")
                               (str
                                 (format-segment-value
                                   x-detail
                                   x-value-type
                                   selected-language))
                              )
        details-length (long
                         (/ (* (count
                                 text-element-content)
                               11)
                            2))
        x (- x-position
             details-length)
        y (- y-position
             30)
        text-element (gen
                       (text
                         text-element-content
                         {:id "point-details"
                          :x x
                          :y y}))]
    (md/append-element
      svg
      text-element)
    (let [text-el (md/query-selector-on-element
                    svg
                    "#point-details")
          width (aget
                  (.getBBox
                    text-el)
                  "width")
          height (aget
                   (.getBBox
                     text-el)
                   "height")
          x-pos x
          y-pos y
          padding-offset 5
          x-left-limit (- x-pos
                          padding-offset)
          x-right-limit (+ x-pos
                           width
                           padding-offset)
          y-lower-limit (+ y-pos
                           padding-offset
                           3)
          y-higher-limit (- y-pos
                            height
                            padding-offset)
          x-arrow-left-base (- x-position
                               10)
          x-arrow-right-base (+ x-position
                                10)
          x-arrow-point x-position
          y-arrow-base y-lower-limit
          y-arrow-point (+ y-lower-limit
                           10)
          polygon-dots (str
                         x-left-limit "," y-lower-limit " "
                         x-arrow-left-base "," y-arrow-base " "
                         x-arrow-point "," y-arrow-point " "
                         x-arrow-right-base "," y-arrow-base " "
                         x-right-limit "," y-lower-limit " "
                         x-right-limit "," y-higher-limit " "
                         x-left-limit "," y-higher-limit " ")
          background-element (gen
                               (polygon
                                 nil
                                 {:id "point-details-background"
                                  :class (if (and background-class
                                                  (string?
                                                    background-class))
                                           background-class
                                           "point-details-background")
                                  :points polygon-dots}))]
      (md/remove-element
        text-el)
      (md/append-element
        svg
        background-element)
      (md/append-element
        svg
        text-element))
   ))

(defn remove-point-details
  "Remove text with details about unhovered point"
  [evt-p
   element
   event]
  (let [svg (.closest
              element
              "svg")
        point-details-element (md/query-selector-on-element
                                svg
                                "#point-details")
        point-details-background-element (md/query-selector-on-element
                                           svg
                                           "#point-details-background")]
    (md/remove-element
      point-details-element)
    (md/remove-element
      point-details-background-element))
 )

(defn generate-polyline
  "Generates single polyline"
  [dot-values
   dots-as-circle
   calculate-x-coordinate
   x-left-offset
   height
   y-bottom-offset
   calculate-y-coordinate
   itr
   & [x-value-type
      y-value-type
      selected-language]]
  (when (and dot-values
             (vector?
               dot-values)
             (not
               (empty?
                 dot-values))
             dots-as-circle
             (instance?
               Atom
               dots-as-circle)
             (vector?
               @dots-as-circle)
             calculate-x-coordinate
             (fn?
               calculate-x-coordinate)
             x-left-offset
             (number?
               x-left-offset)
             height
             (number?
               height)
             y-bottom-offset
             (number?
               y-bottom-offset)
             calculate-y-coordinate
             (fn?
               calculate-y-coordinate)
             itr
             (number?
               itr))
    (let [dots-as-string-a (atom "")]
      (doseq [[x y] dot-values]
        (swap!
          dots-as-circle
          conj
          (circle
            nil
            {:cx (str
                   (+ (calculate-x-coordinate
                        x)
                      x-left-offset))
             :cy (- height
                    y-bottom-offset
                    (calculate-y-coordinate
                      y))
             :r "5"
             :class (str
                      "point point-"
                      itr)}
            {:onpointerenter {:evt-fn append-point-details
                              :evt-p {:x-detail x
                                      :x-position (+ (calculate-x-coordinate
                                                       x)
                                                     x-left-offset)
                                      :x-value-type x-value-type
                                      :y-detail y
                                      :y-position (- height
                                                     y-bottom-offset
                                                     (calculate-y-coordinate
                                                       y))
                                      :y-value-type y-value-type
                                      :selected-language selected-language}}
             :onpointerleave {:evt-fn remove-point-details}}))
        (swap!
          dots-as-string-a
          str
          (+ (calculate-x-coordinate
               x)
             x-left-offset)
          ","
          (- height
             y-bottom-offset
             (calculate-y-coordinate
               y))
          " "))
      (polyline
        nil
        {:points @dots-as-string-a
         :class "chart-line"
         :id (str
               "chart-line-" itr)})
     ))
 )

(defn generate-polylines
  "Generates polylines out of line vectors"
  [dot-values
   multi-line
   calculate-x-coordinate
   x-left-offset
   height
   y-bottom-offset
   calculate-y-coordinate
   & [x-value-type
      y-value-type
      selected-language]]
  (when (and dot-values
             (vector?
               dot-values)
             (not
               (empty?
                 dot-values))
             calculate-x-coordinate
             (fn?
               calculate-x-coordinate)
             x-left-offset
             (number?
               x-left-offset)
             height
             (number?
               height)
             y-bottom-offset
             (number?
               y-bottom-offset)
             calculate-y-coordinate
             (fn?
               calculate-y-coordinate))
    (let [dots-as-circle (atom [])
          ploylines-vector (atom [])]
      (if multi-line
        (let [itr (atom 0)]
          (doseq [line-vector dot-values]
            (swap!
              ploylines-vector
              conj
              (generate-polyline
                line-vector
                dots-as-circle
                calculate-x-coordinate
                x-left-offset
                height
                y-bottom-offset
                calculate-y-coordinate
                @itr
                x-value-type
                y-value-type
                selected-language))
            (swap!
              itr
              inc))
         )
        (swap!
          ploylines-vector
          conj
          (generate-polyline
            dot-values
            dots-as-circle
            calculate-x-coordinate
            x-left-offset
            height
            y-bottom-offset
            calculate-y-coordinate
            0
            x-value-type
            y-value-type
            selected-language))
       )
     [@ploylines-vector
      @dots-as-circle]))
 )

(defn build-line-chart-clj-map
  "Builds line chart clojure map of html elements"
  [{dot-values :dot-values
    x-value-type :x-value-type
    y-value-type :y-value-type
    multi-line :multi-line
    {line-names :line-names
     legend-position :position} :legend
    main-title :main-title
    x-axis-title :x-axis-title
    y-axis-title :y-axis-title
    horizontal-grid-lines :horizontal-grid-lines
    vertical-grid-lines :vertical-grid-lines
    svg-width :svg-width
    svg-height :svg-height
    x-minimum :x-minimum
    y-minimum :y-minimum
    x-maximum :x-maximum
    y-maximum :y-maximum
    selected-language :selected-language}]
  (when (and dot-values
             (vector?
               dot-values)
             (not
               (empty?
                 dot-values))
         )
    (let [width (or svg-width
                    500)
          height (or svg-height
                     500)
          basic-offset 50
          x-axis-title-offset (if (and y-axis-title
                                       (string?
                                         y-axis-title))
                                50
                                0)
          legend-position (or legend-position
                              "top")
          [top-legend-offset
           right-legend-offset
           bottom-legend-offset
           left-legend-offset] (if (and line-names
                                        (vector?
                                          line-names)
                                        (not
                                          (nil?
                                            line-names))
                                    )
                                 [(if (= legend-position
                                         "top")
                                    50
                                    0)
                                  (if (= legend-position
                                         "right")
                                    50
                                    0)
                                  (if (= legend-position
                                         "bottom")
                                    50
                                    0)
                                  (if (= legend-position
                                         "left")
                                    65
                                    0)]
                                 [0 0 0 0])
          line-names-vector (when (and line-names
                                       (vector?
                                         line-names)
                                       (not
                                         (empty?
                                           line-names))
                                   )
                              (let [line-names-vector-a (atom [])
                                    itr (atom 0)
                                    lines-number (count
                                                   line-names)
                                    top-bottom-x-start (long
                                                         (/ (- width
                                                               (* lines-number
                                                                  80))
                                                            2))
                                    top-y 40
                                    bottom-y (- height
                                                30)
                                    left-right-y-start (long
                                                         (/ (- height
                                                               (* lines-number
                                                                  30))
                                                            2))
                                    left-x 30
                                    right-x (- width
                                               65)]
                                (doseq [line-name line-names]
                                  (let [cx (if (contains?
                                                 #{"top"
                                                   "bottom"}
                                                 legend-position)
                                             (+ top-bottom-x-start
                                                (* @itr
                                                   80))
                                             (if (= legend-position
                                                    "right")
                                               right-x
                                               (when (= legend-position
                                                        "left")
                                                 left-x))
                                            )
                                        cy (if (= legend-position
                                                  "top")
                                             top-y
                                             (if (= legend-position
                                                    "bottom")
                                               bottom-y
                                               (when (contains?
                                                       #{"left"
                                                         "right"}
                                                       legend-position)
                                                 (+ left-right-y-start
                                                    (* @itr
                                                       30))
                                                ))
                                            )]
                                    (swap!
                                      line-names-vector-a
                                      conj
                                      (circle
                                        nil
                                        {:cx (str
                                               cx)
                                         :cy (str
                                               cy)
                                         :r "5"
                                         :id (str
                                               "legend-line-" @itr)})
                                      (text
                                        line-name
                                        {:x (str
                                              (- cx
                                                 20))
                                         :y (str
                                              (+ cy
                                                 20))
                                         :fill "#08c"}))
                                   )
                                  (swap!
                                    itr
                                    inc))
                                @line-names-vector-a))
          x-left-offset (+ basic-offset
                           x-axis-title-offset
                           left-legend-offset)
          x-right-offset (+ basic-offset
                            right-legend-offset)
          y-top-offset (+ basic-offset
                          top-legend-offset)
          y-bottom-offset (+ basic-offset
                             bottom-legend-offset)
          x-axis-width (- width
                          (+ x-left-offset
                             x-right-offset))
          x-axis-height 1
          y-axis-height (- height
                           (+ y-top-offset
                              y-bottom-offset))
          y-axis-width 1
          [x-min x-max
           y-min y-max] (find-x-y-min-max
                          dot-values
                          multi-line)
          x-min (- x-min
                   (mod x-min
                        10))
          y-min (- y-min
                   (mod y-min
                        10))
          x-max (+ x-max
                   (- 10
                      (mod x-max
                           10))
                 )
          y-max (+ y-max
                   (- 10
                      (mod y-max
                           10))
                 )
          x-min (if (and x-minimum
                         (number?
                           x-minimum))
                  x-minimum
                  x-min)
          x-max (if (and x-maximum
                         (number?
                           x-maximum))
                  x-maximum
                  x-max)
          y-min (if (and y-minimum
                         (number?
                           y-minimum))
                  y-minimum
                  y-min)
          y-max (if (and y-maximum
                         (number?
                           y-maximum))
                  y-maximum
                  y-max)
          x-axis-segments (axis-segment
                            x-min
                            x-max
                            x-axis-width)
          x-axis-value-lines (atom [])
          void (doseq [[segment-value
                        segment-pixels] x-axis-segments]
                 (swap!
                   x-axis-value-lines
                   conj
                   (line
                     nil
                     {:x1 (+ segment-pixels
                             x-left-offset)
                      :x2 (+ segment-pixels
                             x-left-offset)
                      :y1 (if vertical-grid-lines
                            y-top-offset
                            (- height
                               y-bottom-offset
                               5))
                      :y2 (+ (- height
                                y-bottom-offset)
                             5)
                      :class "x-axis-value-line"})
                   (let [x-axis-text-x (- (+ segment-pixels
                                             x-left-offset
                                             1)
                                          (long
                                            (/ (* (count
                                                    (format-segment-value
                                                      segment-value
                                                      x-value-type
                                                      selected-language))
                                                  9)
                                               2))
                                        )
                         x-axis-text-y (+ (- height
                                             y-bottom-offset)
                                          20)
                         x-axis-text-attributes {:x x-axis-text-x
                                                 :y x-axis-text-y
                                                 :class "x-axis-value-text"}
                         x-axis-text-attributes (if (= x-value-type
                                                       "date")
                                                  (assoc
                                                    x-axis-text-attributes
                                                    :transform
                                                      (str
                                                        "rotate(-20 "
                                                        (+ x-axis-text-x
                                                           60)
                                                        ","
                                                        (- x-axis-text-y
                                                           10)
                                                        ")"))
                                                  x-axis-text-attributes)]
                     (text
                       (format-segment-value
                         segment-value
                         x-value-type
                         selected-language)
                       x-axis-text-attributes))
                  ))
          y-axis-segments (axis-segment
                            y-min
                            y-max
                            y-axis-height)
          y-axis-value-lines (atom [])
          void (doseq [[segment-value
                        segment-pixels] y-axis-segments]
                 (swap!
                   y-axis-value-lines
                   conj
                   (line
                     nil
                     {:x1 (- x-left-offset
                             5)
                      :x2 (if horizontal-grid-lines
                            (+ x-left-offset
                               x-axis-width)
                            (+ x-left-offset
                               5))
                      :y1 (- height
                             y-bottom-offset
                             segment-pixels)
                      :y2 (- height
                             y-bottom-offset
                             segment-pixels)
                      :class "y-axis-value-line"})
                   (let [y-axis-text-x (- x-left-offset
                                          (long
                                            (* (count
                                                 (format-segment-value
                                                   segment-value
                                                   y-value-type
                                                   selected-language))
                                               9))
                                          5)
                         y-axis-text-y (+ (- height
                                             y-bottom-offset
                                             segment-pixels)
                                          6)
                         y-text-attributes {:x y-axis-text-x
                                            :y y-axis-text-y
                                            :class "y-axis-value-text"}
                         y-text-attributes (if (= y-value-type
                                                  "date")
                                             (assoc
                                               y-text-attributes
                                               :transform
                                                 (str
                                                   "rotate(-70 "
                                                   (+ y-axis-text-x
                                                      55)
                                                   ","
                                                   (- y-axis-text-y
                                                      26)
                                                   ")"))
                                             y-text-attributes)]
                     (text
                       (format-segment-value
                         segment-value
                         y-value-type
                         selected-language)
                       y-text-attributes))
                  ))
          x-axis (line
                   nil
                   {:x1 x-left-offset
                    :x2 (- width
                           x-right-offset)
                    :y1 (- height
                           y-bottom-offset)
                    :y2 (- height
                           y-bottom-offset)
                    :class "x-axis"})
          y-axis (line
                   nil
                   {:x1 x-left-offset
                    :x2 x-left-offset
                    :y1 y-top-offset
                    :y2 (- height
                           y-bottom-offset)
                    :class "y-axis"})
          [x-s-min
           x-p-min] (first
                      x-axis-segments)
          [x-s-max
           x-p-max] (last
                      x-axis-segments)
          x-s-length (- x-s-max
                        x-s-min)
          x-p-length (- x-p-max
                        x-p-min)
          calculate-x-coordinate (fn [x-s-current]
                                   (- x-p-length
                                      (long
                                        (* (/ x-p-length
                                              x-s-length)
                                           (- x-s-length
                                              (- x-s-current
                                                 x-s-min))
                                         ))
                                    ))
          [y-s-min
           y-p-min] (first
                      y-axis-segments)
          [y-s-max
           y-p-max] (last
                      y-axis-segments)
          y-s-length (- y-s-max
                        y-s-min)
          y-p-length (- y-p-max
                        y-p-min)
          calculate-y-coordinate (fn [y-s-current]
                                   (- y-p-length
                                      (long
                                        (* (/ y-p-length
                                              y-s-length)
                                           (- y-s-length
                                              (- y-s-current
                                                 y-s-min))
                                         ))
                                    ))
          [chart-lines
           dots-as-circle] (generate-polylines
                             dot-values
                             multi-line
                             calculate-x-coordinate
                             x-left-offset
                             height
                             y-bottom-offset
                             calculate-y-coordinate
                             x-value-type
                             y-value-type
                             selected-language)
          chart-content (atom [])]
      (swap!
        chart-content
        conj
        x-axis
        y-axis)
      (doseq [x-line-text @x-axis-value-lines]
        (swap!
          chart-content
          conj
          x-line-text))
      (doseq [y-line-text @y-axis-value-lines]
        (swap!
          chart-content
          conj
          y-line-text))
      (doseq [chart-line chart-lines]
        (swap!
          chart-content
          conj
          chart-line))
      (doseq [dots-circle dots-as-circle]
        (swap!
          chart-content
          conj
          dots-circle))
      (when (and main-title
                 (string?
                   main-title))
        (swap!
          chart-content
          conj
          (text
            main-title
            {:x (str
                  (long
                    (/ width
                       2))
                 )
             :y "20"
             :text-anchor "middle"
             :class "main-title"}))
       )
      (when (and x-axis-title
                 (string?
                   x-axis-title))
        (swap!
          chart-content
          conj
          (text
            x-axis-title
            {:x (str
                  (long
                    (/ width
                       2))
                 )
             :y (str
                  (- height
                     bottom-legend-offset))
             :text-anchor "middle"
             :class "x-axis-title"}))
       )
      (when (and y-axis-title
                 (string?
                   y-axis-title))
        (swap!
          chart-content
          conj
          (text
            y-axis-title
            {:x "10"
             :y (str
                  (+ (long
                       (/ height
                          2))
                     left-legend-offset))
             :transform (str
                          "rotate(-90 30,"
                          (long
                            (/ height
                               2))
                          ")")
             :text-anchor "middle"
             :class "y-axis-title"}))
       )
      (doseq [line-name line-names-vector]
        (swap!
          chart-content
          conj
          line-name))
      (svg
        @chart-content
        {:width (str
                  width)
         :height (str
                   height)})
     ))
 )

(defn render-line-chart
  "Renders line chart html elements from chart map"
  [chart-configuration]
  (gen
    (build-line-chart-clj-map
      chart-configuration))
 )

(defn bar-axis-segment
  "Calculate segments for bars"
  [bar-labels
   width]
  (when (and bar-labels
             (vector?
               bar-labels)
             (not
               (empty?
                 bar-labels))
             width
             (number?
               width))
    (let [bar-count (count
                      bar-labels)
          segment-pixel-value (long
                                (/ width
                                   bar-count))
          segment-pixel-value-range (range
                                      (long
                                        (/ segment-pixel-value
                                           2))
                                      (inc
                                        width)
                                      segment-pixel-value)
          result (atom [])
          map-result (map
                       (fn [segment-value-p
                            segment-pixel-value-p]
                         (swap!
                           result
                           conj
                           [segment-value-p
                            segment-pixel-value-p]))
                       bar-labels
                       segment-pixel-value-range)]
      (last
        map-result))
   ))

(defn bar-min-max-iterate-coordinates
  "Iterates through bar values"
  [bar-values
   axis-min
   axis-max]
  (when (and bar-values
             (vector?
               bar-values)
             (not
               (empty?
                 bar-values)
              )
             axis-min
             (instance?
               Atom
               axis-min)
             axis-max
             (instance?
               Atom
               axis-max))
    (doseq [bar-value bar-values]
      (let [bar-value-as-number (if (instance?
                                      js/Date
                                      bar-value)
                                  (.getTime
                                    bar-value)
                                  bar-value)]
        (when (< bar-value-as-number
                 @axis-min)
          (reset!
            axis-min
            bar-value-as-number))
        (when (< @axis-max
                 bar-value-as-number)
          (reset!
            axis-max
            bar-value-as-number))
       ))
   ))

(defn find-bar-x-y-min-max
  "Finds minimum and maximum for bars and sets them for particular axis x or y"
  [bar-values
   multi-bars
   bar-values-on-x-axis
   value-type]
  (when (and bar-values
             (vector?
               bar-values)
             (not
               (empty?
                 bar-values))
         )
    (let [axis-min (atom
                     (aget
                       js/Number
                       "MAX_SAFE_INTEGER"))
          axis-max (atom
                     (aget
                       js/Number
                       "MIN_SAFE_INTEGER"))]
      (if multi-bars
        (doseq [bar-vector bar-values]
          (bar-min-max-iterate-coordinates
            bar-vector
            axis-min
            axis-max))
        (bar-min-max-iterate-coordinates
          bar-values
          axis-min
          axis-max))
      (when @axis-max
        (if bar-values-on-x-axis
          [(if (= value-type
                  "date")
             @axis-min
             0)
           @axis-max
           0
           (aget
             js/Number
             "MIN_SAFE_INTEGER")]
          [0
           (aget
             js/Number
             "MIN_SAFE_INTEGER")
           (if (= value-type
                  "date")
             @axis-min
             0)
           @axis-max]))
     ))
 )

(defn generate-bar
  "Generates row of bars and their values"
  [bar-value
   chart-bars
   bar-values-on-x-axis
   calculate-x-coordinate
   calculate-y-coordinate
   calculate-width
   calculate-height
   itr
   number-of-bars
   value-type
   selected-language]
  (when (and bar-value
             (vector?
               bar-value)
             (not
               (empty?
                 bar-value))
             chart-bars
             (instance?
               Atom
               chart-bars)
             calculate-x-coordinate
             (fn?
               calculate-x-coordinate)
             calculate-y-coordinate
             (fn?
               calculate-y-coordinate)
             calculate-width
             (fn?
               calculate-width)
             calculate-height
             (fn?
               calculate-height)
             itr
             (number?
               itr)
             number-of-bars
             (number?
               number-of-bars))
    (let [bar-number (atom 0)]
      (doseq [value-i bar-value]
        (let [value-i (if (= value-type
                             "date")
                        (.getTime
                          value-i)
                        value-i)
              rect-width (if bar-values-on-x-axis
                           (calculate-width
                             value-i
                             (count
                               bar-value))
                           (long
                             (/ (- (calculate-width
                                     @bar-number
                                     (count
                                       bar-value))
                                   5)
                                number-of-bars))
                          )
              rect-height (if bar-values-on-x-axis
                            (long
                              (/ (- (calculate-height
                                      @bar-number
                                      (count
                                        bar-value))
                                    5)
                                 number-of-bars))
                            (calculate-height
                              value-i
                              (count
                                bar-value))
                           )
              rect-x (if bar-values-on-x-axis
                       (calculate-x-coordinate
                         value-i
                         (count
                           bar-value))
                       (+ (calculate-x-coordinate
                            @bar-number
                            (count
                              bar-value))
                          (* (long
                               (/ (- (calculate-width
                                       @bar-number
                                       (count
                                         bar-value))
                                     5)
                                  number-of-bars))
                             itr)
                          3))
              rect-y (if bar-values-on-x-axis
                       (+ (calculate-y-coordinate
                            (inc
                              @bar-number)
                            (count
                              bar-value))
                          (* (long
                               (/ (- (calculate-height
                                       @bar-number
                                       (count
                                         bar-value))
                                     5)
                                  number-of-bars))
                             itr)
                          3)
                       (calculate-y-coordinate
                         value-i
                         (count
                           bar-value))
                      )]
          (swap!
            chart-bars
            conj
            (rect
              nil
              {:width (str
                        rect-width)
               :height (str
                         rect-height)
               :x (str
                    rect-x)
               :y (str
                    rect-y)
               :class (str
                        "chart-bar "
                        "chart-bar-" itr)}
              {:onpointerenter {:evt-fn append-point-details
                                :evt-p {:x-detail value-i
                                        :x-position (+ rect-x
                                                       (long
                                                         (/ rect-width
                                                            2))
                                                     )
                                        :x-value-type value-type
                                        ;:y-detail "y-detail"
                                        :y-position rect-y
                                        ;:y-value-type "string"
                                        :selected-language selected-language}}
               :onpointerleave {:evt-fn remove-point-details}}))
         )
        (swap!
          bar-number
          inc))
     ))
 )

(defn generate-bars
  "Generates svg rectangulars as bars"
  [bar-values
   multi-bars
   bar-values-on-x-axis
   calculate-x-coordinate
   calculate-y-coordinate
   calculate-width
   calculate-height
   value-type
   selected-language]
  (when (and bar-values
             (vector?
               bar-values)
             (not
               (empty?
                 bar-values))
             calculate-x-coordinate
             (fn?
               calculate-x-coordinate)
             calculate-y-coordinate
             (fn?
               calculate-y-coordinate)
             calculate-width
             (fn?
               calculate-width)
             calculate-height
             (fn?
               calculate-height))
    (let [chart-bars (atom [])]
      (if multi-bars
        (let [itr (atom 0)]
          (doseq [bar-value bar-values]
            (generate-bar
              bar-value
              chart-bars
              bar-values-on-x-axis
              calculate-x-coordinate
              calculate-y-coordinate
              calculate-width
              calculate-height
              @itr
              (count
                bar-values)
              value-type
              selected-language)
            (swap!
              itr
              inc))
         )
        (generate-bar
          bar-values
          chart-bars
          bar-values-on-x-axis
          calculate-x-coordinate
          calculate-y-coordinate
          calculate-width
          calculate-height
          0
          1
          value-type
          selected-language)
       )
     @chart-bars))
 )

(defn build-bar-chart-clj-map
  "Builds bar chart clojure map of html elements"
  [{bar-values :bar-values
    bar-labels :bar-labels
    bar-values-on-x-axis :bar-values-on-x-axis
    value-type :value-type
    multi-bars :multi-bars
    {bar-names :bar-names
     legend-position :position} :legend
    main-title :main-title
    x-axis-title :x-axis-title
    y-axis-title :y-axis-title
    horizontal-grid-lines :horizontal-grid-lines
    vertical-grid-lines :vertical-grid-lines
    svg-width :svg-width
    svg-height :svg-height
    x-minimum :x-minimum
    y-minimum :y-minimum
    x-maximum :x-maximum
    y-maximum :y-maximum
    selected-language :selected-language}]
  (when (and bar-values
             (vector?
               bar-values)
             (not
               (empty?
                 bar-values))
         )
    (let [width (or svg-width
                    500)
          height (or svg-height
                     500)
          basic-offset 50
          x-axis-title-offset (if (and y-axis-title
                                       (string?
                                         y-axis-title))
                                50
                                0)
          legend-position (or legend-position
                              "top")
          [top-legend-offset
           right-legend-offset
           bottom-legend-offset
           left-legend-offset] (if (and bar-names
                                        (vector?
                                          bar-names)
                                        (not
                                          (nil?
                                            bar-names))
                                    )
                                 [(if (= legend-position
                                         "top")
                                    50
                                    0)
                                  (if (= legend-position
                                         "right")
                                    50
                                    0)
                                  (if (= legend-position
                                         "bottom")
                                    50
                                    0)
                                  (if (= legend-position
                                         "left")
                                    65
                                    0)]
                                 [0 0 0 0])
          bar-names-vector (when (and bar-names
                                      (vector?
                                        bar-names)
                                      (not
                                        (empty?
                                          bar-names))
                                   )
                             (let [bar-names-vector-a (atom [])
                                   itr (atom 0)
                                   lines-number (count
                                                  bar-names)
                                   top-bottom-x-start (long
                                                        (/ (- width
                                                              (* lines-number
                                                                 80))
                                                           2))
                                   top-y 40
                                   bottom-y (- height
                                               30)
                                   left-right-y-start (long
                                                        (/ (- height
                                                              (* lines-number
                                                                 30))
                                                           2))
                                   left-x 30
                                   right-x (- width
                                              65)]
                               (doseq [bar-name bar-names]
                                 (let [cx (if (contains?
                                                #{"top"
                                                  "bottom"}
                                                legend-position)
                                            (+ top-bottom-x-start
                                               (* @itr
                                                  80))
                                            (if (= legend-position
                                                   "right")
                                              right-x
                                              (when (= legend-position
                                                       "left")
                                                left-x))
                                           )
                                       cy (if (= legend-position
                                                 "top")
                                            top-y
                                            (if (= legend-position
                                                   "bottom")
                                              bottom-y
                                              (when (contains?
                                                      #{"left"
                                                        "right"}
                                                      legend-position)
                                                (+ left-right-y-start
                                                   (* @itr
                                                      30))
                                               ))
                                           )]
                                   (swap!
                                     bar-names-vector-a
                                     conj
                                     (circle
                                       nil
                                       {:cx (str
                                              cx)
                                        :cy (str
                                              cy)
                                        :r "5"
                                        :id (str
                                              "legend-line-" @itr)})
                                     (text
                                       bar-name
                                       {:x (str
                                             (- cx
                                                20))
                                        :y (str
                                             (+ cy
                                                20))
                                        :fill "#08c"}))
                                  )
                                 (swap!
                                   itr
                                   inc))
                               @bar-names-vector-a))
          bar-labels (if (and bar-labels
                              (vector?
                                bar-labels))
                       bar-labels
                       (if multi-bars
                         (let [count-labels (count
                                              (first
                                                bar-values))
                               default-bar-labels (atom [])]
                           (dotimes [i count-labels]
                             (swap!
                               default-bar-labels
                               conj
                               (str
                                 i))
                            )
                           @default-bar-labels)
                         (let [count-labels (count
                                              bar-values)
                               default-bar-labels (atom [])]
                           (dotimes [i count-labels]
                             (swap!
                               default-bar-labels
                               conj
                               (str
                                 i))
                            )
                           @default-bar-labels))
                      )
          x-left-offset (+ basic-offset
                           x-axis-title-offset
                           left-legend-offset)
          x-right-offset (+ basic-offset
                            right-legend-offset)
          y-top-offset (+ basic-offset
                          top-legend-offset)
          y-bottom-offset (+ basic-offset
                             bottom-legend-offset)
          x-axis-width (- width
                          (+ x-left-offset
                             x-right-offset))
          x-axis-height 1
          y-axis-height (- height
                           (+ y-top-offset
                              y-bottom-offset))
          y-axis-width 1
          [x-min x-max
           y-min y-max] (find-bar-x-y-min-max
                          bar-values
                          multi-bars
                          bar-values-on-x-axis
                          value-type)
          x-max (+ x-max
                   (- 10
                      (mod x-max
                           10))
                 )
          y-max (+ y-max
                   (- 10
                      (mod y-max
                           10))
                 )
          x-min (if (and x-minimum
                         (number?
                           x-minimum))
                  x-minimum
                  x-min)
          x-max (if (and x-maximum
                         (number?
                           x-maximum))
                  x-maximum
                  x-max)
          y-min (if (and y-minimum
                         (number?
                           y-minimum))
                  y-minimum
                  y-min)
          y-max (if (and y-maximum
                         (number?
                           y-maximum))
                  y-maximum
                  y-max)
          x-axis-segments (if bar-values-on-x-axis
                            (axis-segment
                              x-min
                              x-max
                              x-axis-width)
                            (bar-axis-segment
                              bar-labels
                              x-axis-width))
          x-axis-value-lines (atom [])
          void (doseq [[segment-value
                        segment-pixels] x-axis-segments]
                 (swap!
                   x-axis-value-lines
                   conj
                   (line
                     nil
                     {:x1 (+ segment-pixels
                             x-left-offset)
                      :x2 (+ segment-pixels
                             x-left-offset)
                      :y1 (if vertical-grid-lines
                            y-top-offset
                            (- height
                               y-bottom-offset
                               5))
                      :y2 (+ (- height
                                y-bottom-offset)
                             5)
                      :class "x-axis-value-line"})
                   (let [x-axis-text-x (- (+ segment-pixels
                                             x-left-offset
                                             1)
                                          (long
                                            (/ (* (count
                                                    (format-segment-value
                                                      segment-value
                                                      (when bar-values-on-x-axis
                                                        value-type)
                                                      selected-language))
                                                  9)
                                               2))
                                        )
                         x-axis-text-y (+ (- height
                                             y-bottom-offset)
                                          20)
                         x-axis-text-attributes {:x x-axis-text-x
                                                 :y x-axis-text-y
                                                 :class "x-axis-value-text"}
                         x-axis-text-attributes (if (= (when bar-values-on-x-axis
                                                         value-type)
                                                       "date")
                                                  (assoc
                                                    x-axis-text-attributes
                                                    :transform
                                                      (str
                                                        "rotate(-20 "
                                                        (+ x-axis-text-x
                                                           60)
                                                        ","
                                                        (- x-axis-text-y
                                                           10)
                                                        ")"))
                                                  x-axis-text-attributes)]
                     (text
                       (format-segment-value
                         segment-value
                         (when bar-values-on-x-axis
                           value-type)
                         selected-language)
                       x-axis-text-attributes))
                  ))
          y-axis-segments (if bar-values-on-x-axis
                            (bar-axis-segment
                              bar-labels
                              y-axis-height)
                            (axis-segment
                              y-min
                              y-max
                              y-axis-height))
          y-axis-value-lines (atom [])
          void (doseq [[segment-value
                        segment-pixels] y-axis-segments]
                 (swap!
                   y-axis-value-lines
                   conj
                   (line
                     nil
                     {:x1 (- x-left-offset
                             5)
                      :x2 (if horizontal-grid-lines
                            (+ x-left-offset
                               x-axis-width)
                            (+ x-left-offset
                               5))
                      :y1 (- height
                             y-bottom-offset
                             segment-pixels)
                      :y2 (- height
                             y-bottom-offset
                             segment-pixels)
                      :class "y-axis-value-line"})
                   (let [y-axis-text-x (- x-left-offset
                                          (long
                                            (* (count
                                                 (format-segment-value
                                                   segment-value
                                                   (when-not bar-values-on-x-axis
                                                     value-type)
                                                   selected-language))
                                               9))
                                          5)
                         y-axis-text-y (+ (- height
                                             y-bottom-offset
                                             segment-pixels)
                                          6)
                         y-text-attributes {:x y-axis-text-x
                                            :y y-axis-text-y
                                            :class "y-axis-value-text"}
                         y-text-attributes (if (= (when-not bar-values-on-x-axis
                                                    value-type)
                                                  "date")
                                             (assoc
                                               y-text-attributes
                                               :transform
                                                 (str
                                                   "rotate(-70 "
                                                   (+ y-axis-text-x
                                                      55)
                                                   ","
                                                   (- y-axis-text-y
                                                      26)
                                                   ")"))
                                             y-text-attributes)]
                     (text
                       (format-segment-value
                         segment-value
                         (when-not bar-values-on-x-axis
                           value-type)
                         selected-language)
                       y-text-attributes))
                  ))
          x-axis (line
                   nil
                   {:x1 x-left-offset
                    :x2 (- width
                           x-right-offset)
                    :y1 (- height
                           y-bottom-offset)
                    :y2 (- height
                           y-bottom-offset)
                    :class "x-axis"})
          y-axis (line
                   nil
                   {:x1 x-left-offset
                    :x2 x-left-offset
                    :y1 y-top-offset
                    :y2 (- height
                           y-bottom-offset)
                    :class "y-axis"})
          [x-s-min
           x-p-min] (first
                      x-axis-segments)
          [x-s-max
           x-p-max] (last
                      x-axis-segments)
          calculate-x-coordinate (if bar-values-on-x-axis
                                   (fn [bar-x-value
                                        bar-count]
                                     x-left-offset)
                                   (fn [bar-number
                                        bar-count]
                                     (long
                                       (+ (* (/ x-axis-width
                                                bar-count)
                                             bar-number)
                                          x-left-offset))
                                    ))
          [y-s-min
           y-p-min] (first
                      y-axis-segments)
          [y-s-max
           y-p-max] (last
                      y-axis-segments)
          y-bottom-start (- height
                            y-bottom-offset)
          calculate-y-coordinate (if bar-values-on-x-axis
                                   (fn [bar-number
                                        bar-count]
                                     (long
                                       (- y-bottom-start
                                          (* (/ y-axis-height
                                                bar-count)
                                             bar-number))
                                      ))
                                   (fn [bar-y-value
                                        bar-count]
                                     (long
                                       (- y-bottom-start
                                          (/ (* y-p-max
                                                (- bar-y-value
                                                   y-s-min))
                                             (- y-s-max
                                                y-s-min))
                                        ))
                                    ))
          calculate-width (if bar-values-on-x-axis
                            (fn [bar-value
                                 bar-count]
                              (long
                                (/ (* x-p-max
                                      (- bar-value
                                         x-s-min))
                                   (- x-s-max
                                      x-s-min))
                               ))
                            (fn [bar-number
                                 bar-count]
                              (long
                                (/ x-axis-width
                                   bar-count))
                             ))
          calculate-height (if bar-values-on-x-axis
                             (fn [bar-number
                                  bar-count]
                               (long
                                 (/ y-axis-height
                                    bar-count))
                              )
                             (fn [bar-value
                                  bar-count]
                               (long
                                 (/ (* y-p-max
                                       (- bar-value
                                          y-s-min))
                                    (- y-s-max
                                       y-s-min))
                                ))
                            )
          chart-bars (generate-bars
                       bar-values
                       multi-bars
                       bar-values-on-x-axis
                       calculate-x-coordinate
                       calculate-y-coordinate
                       calculate-width
                       calculate-height
                       value-type
                       selected-language)
          chart-content (atom [])]
      (swap!
        chart-content
        conj
        x-axis
        y-axis)
      (doseq [x-line-text @x-axis-value-lines]
        (swap!
          chart-content
          conj
          x-line-text))
      (doseq [y-line-text @y-axis-value-lines]
        (swap!
          chart-content
          conj
          y-line-text))
      (doseq [chart-bar chart-bars]
        (swap!
          chart-content
          conj
          chart-bar))
      (when (and main-title
                 (string?
                   main-title))
        (swap!
          chart-content
          conj
          (text
            main-title
            {:x (str
                  (long
                    (/ width
                       2))
                 )
             :y "20"
             :text-anchor "middle"
             :class "main-title"}))
       )
      (when (and x-axis-title
                 (string?
                   x-axis-title))
        (swap!
          chart-content
          conj
          (text
            x-axis-title
            {:x (str
                  (long
                    (/ width
                       2))
                 )
             :y (str
                  (- height
                     bottom-legend-offset))
             :text-anchor "middle"
             :class "x-axis-title"}))
       )
      (when (and y-axis-title
                 (string?
                   y-axis-title))
        (swap!
          chart-content
          conj
          (text
            y-axis-title
            {:x "10"
             :y (str
                  (+ (long
                       (/ height
                          2))
                     left-legend-offset))
             :transform (str
                          "rotate(-90 30,"
                          (long
                            (/ height
                               2))
                          ")")
             :text-anchor "middle"
             :class "y-axis-title"}))
       )
      (doseq [bar-name bar-names-vector]
        (swap!
          chart-content
          conj
          bar-name))
      (svg
        @chart-content
        {:width (str
                  width)
         :height (str
                   height)})
     ))
 )

(defn render-bar-chart
  "Renders bar chart html elements from chart map"
  [chart-configuration]
  (gen
    (build-bar-chart-clj-map
      chart-configuration))
 )

(defn calculate-clip-path
  "Calculate clip path for pie chart"
  [x-start
   y-start
   x-end
   y-end
   radius]
  (when (and x-start
             (number?
               x-start)
             y-start
             (number?
               y-start)
             x-end
             (number?
               x-end)
             y-end
             (number?
               y-end)
             radius
             (number?
               radius))
    (let [start-quadrate (utils/find-quadrate
                           x-start
                           y-start)
          end-quadrate (utils/find-quadrate
                         x-end
                         y-end)
          end-quadrate (if (< end-quadrate
                              start-quadrate)
                         3
                         end-quadrate)
          quadrates (range
                      end-quadrate
                      (dec
                        start-quadrate)
                      -1)
          points-vector (atom [[x-start y-start]
                               [0 0]
                               [x-end y-end]])
          ]
      (doseq [quadrate-number quadrates]
        (let [{start :start
               angle :angle
               end :end} (utils/get-quadrate-extreme-points
                           quadrate-number
                           radius)]
          (swap!
            points-vector
            conj
            angle
            start))
       )
      @points-vector))
 )

(defn generate-pie-slices
  "Generate pie slices in form of html svg elements"
  [pie-values
   radius
   cx
   cy
   pie-id-num
   width
   height
   value-type
   selected-language]
  (when (and pie-values
             (vector?
               pie-values)
             (not
               (empty?
                 pie-values))
         )
    (let [clip-path-elements (atom [])
          use-elements (atom [])
          total-value (apply
                        +
                        pie-values)
          starting-point (atom [radius 0])
          cumulativ-value (atom 0)
          itr (atom 0)]
      (doseq [single-value pie-values]
        (let [clip-path-id (str
                             "slice-"
                             pie-id-num
                             "-"
                             @itr)
              cumulative-single-value-half (+ @cumulativ-value
                                              (long
                                                (/ single-value
                                                   2))
                                            )
              value-angle (long
                            (/ (* cumulative-single-value-half
                                  360)
                               total-value))
              [x-value
               y-value] (utils/calculate-circle-coordinates
                          (long
                            (/ radius
                               2))
                          value-angle)
              display-value (if (= value-type
                                   "percentage")
                              (long
                                (/ (* single-value
                                      100)
                                   total-value))
                              single-value)
              void (swap!
                     cumulativ-value
                     +
                     single-value)
              angle (long
                      (/ (* @cumulativ-value
                            360)
                         total-value))
              [x-start
               y-start] @starting-point
              [x-end
               y-end] (utils/calculate-circle-coordinates
                        radius
                        angle)
              clip-path-vector (calculate-clip-path
                                 x-start
                                 y-start
                                 x-end
                                 y-end
                                 radius)
              points (atom "")]
          (doseq [[x y] clip-path-vector]
            (swap!
              points
              str
              (+ x
                 cx)
              ","
              (- cy
                 y)
              " "))
          (swap!
            clip-path-elements
            conj
            (clip-path
              (polygon
                nil
                {:points @points})
              {:id clip-path-id}))
          (swap!
            use-elements
            conj
            (svg-use
              nil
              {:clip-path (str
                            "url(#"
                            clip-path-id
                            ")")
               :class (str
                        "pie pie-"
                        @itr)}
              {:onpointerenter {:evt-fn append-point-details
                                :evt-p {:x-detail display-value
                                        :x-position (+ x-value
                                                       cx)
                                        :x-value-type value-type
                                        :y-detail nil
                                        :y-position (- cy
                                                       y-value)
                                        :y-value-type nil
                                        :background-class "point-details-background-i"
                                        :selected-language selected-language}}
               :onpointerleave {:evt-fn remove-point-details}})
           )
          (reset!
            starting-point
            [x-end
             y-end]))
        (swap!
          itr
          inc))
     [@clip-path-elements
      (reverse
        @use-elements)])
   ))

(defn build-pie-chart-clj-map
  "Builds pie chart clojure map of html elements"
  [{pie-values :pie-values
    value-type :value-type
    {bar-names :bar-names
     legend-position :position} :legend
    main-title :main-title
    svg-width :svg-width
    svg-height :svg-height
    selected-language :selected-language}]
  (when (and pie-values
             (vector?
               pie-values)
             (not
               (empty?
                 pie-values))
         )
    (let [width (or svg-width
                    500)
          height (or svg-height
                     500)
          basic-offset 50
          legend-position (or legend-position
                              "top")
          [top-legend-offset
           right-legend-offset
           bottom-legend-offset
           left-legend-offset] (if (and bar-names
                                        (vector?
                                          bar-names)
                                        (not
                                          (nil?
                                            bar-names))
                                    )
                                 [(if (= legend-position
                                         "top")
                                    50
                                    0)
                                  (if (= legend-position
                                         "right")
                                    50
                                    0)
                                  (if (= legend-position
                                         "bottom")
                                    50
                                    0)
                                  (if (= legend-position
                                         "left")
                                    65
                                    0)]
                                 [0 0 0 0])
          bar-names-vector (when (and bar-names
                                      (vector?
                                        bar-names)
                                      (not
                                        (empty?
                                          bar-names))
                                   )
                             (let [bar-names-vector-a (atom [])
                                   itr (atom 0)
                                   lines-number (count
                                                  bar-names)
                                   top-bottom-x-start (long
                                                        (/ (- width
                                                              (* lines-number
                                                                 80))
                                                           2))
                                   top-y 40
                                   bottom-y (- height
                                               30)
                                   left-right-y-start (long
                                                        (/ (- height
                                                              (* lines-number
                                                                 30))
                                                           2))
                                   left-x 30
                                   right-x (- width
                                              65)]
                               (doseq [bar-name bar-names]
                                 (let [cx (if (contains?
                                                #{"top"
                                                  "bottom"}
                                                legend-position)
                                            (+ top-bottom-x-start
                                               (* @itr
                                                  80))
                                            (if (= legend-position
                                                   "right")
                                              right-x
                                              (when (= legend-position
                                                       "left")
                                                left-x))
                                           )
                                       cy (if (= legend-position
                                                 "top")
                                            top-y
                                            (if (= legend-position
                                                   "bottom")
                                              bottom-y
                                              (when (contains?
                                                      #{"left"
                                                        "right"}
                                                      legend-position)
                                                (+ left-right-y-start
                                                   (* @itr
                                                      30))
                                               ))
                                           )]
                                   (swap!
                                     bar-names-vector-a
                                     conj
                                     (circle
                                       nil
                                       {:cx (str
                                              cx)
                                        :cy (str
                                              cy)
                                        :r "5"
                                        :id (str
                                              "legend-line-" @itr)})
                                     (text
                                       bar-name
                                       {:x (str
                                             (- cx
                                                20))
                                        :y (str
                                             (+ cy
                                                20))
                                        :fill "#08c"}))
                                  )
                                 (swap!
                                   itr
                                   inc))
                               @bar-names-vector-a))
          left-offset (+ basic-offset
                         left-legend-offset)
          right-offset (+ basic-offset
                          right-legend-offset)
          x-axis-width (- width
                          left-offset
                          right-offset)
          top-offset (+ basic-offset
                        top-legend-offset)
          bottom-offset (+ basic-offset
                           bottom-legend-offset)
          y-axis-height (- height
                           top-offset
                           bottom-offset)
          radius (if (< x-axis-width
                        y-axis-height)
                   (long
                     (/ x-axis-width
                        2))
                   (long
                     (/ y-axis-height
                        2))
                  )
          cx (+ left-offset
                radius)
          cy (+ top-offset
                radius)
          pie-id-num (long
                       (* (Math/random)
                          10000))
          pie-id (str
                   "pie-"
                   pie-id-num)
          circle-element (circle
                           nil
                           {:id pie-id
                            :cx (str
                                  cx)
                            :cy (str
                                  cy)
                            :r (str
                                 radius)})
          [clip-path-elements
           use-elements] (generate-pie-slices
                           pie-values
                           radius
                           cx
                           cy
                           pie-id-num
                           width
                           height
                           value-type
                           selected-language)
          chart-content (atom [])]
      (when (and main-title
                 (string?
                   main-title))
        (swap!
          chart-content
          conj
          (text
            main-title
            {:x (str
                  (long
                    (/ width
                       2))
                 )
             :y "20"
             :text-anchor "middle"
             :class "main-title"}))
       )
      (doseq [bar-name bar-names-vector]
        (swap!
          chart-content
          conj
          bar-name))
      (swap!
        chart-content
        conj
        circle-element)
      (doseq [clip-path-element clip-path-elements]
        (swap!
          chart-content
          conj
          clip-path-element))
      (doseq [use-element use-elements]
        (let [generated-use-element (gen
                                      use-element)]
          (aset
            (aget
              generated-use-element
              "href")
            "baseVal"
            (str
              "#"
              pie-id))
          (aset
            (aget
              generated-use-element
              "href")
            "animVal"
            (str
              "#"
              pie-id))
          (swap!
            chart-content
            conj
            generated-use-element))
       )
      (svg
        @chart-content
        {:width (str
                  width)
         :height (str
                   height)})
     ))
 )

(defn render-pie-chart
  "Renders pie chart html elements from chart map"
  [chart-configuration]
  (gen
    (build-pie-chart-clj-map
      chart-configuration))
 )


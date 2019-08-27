(ns svg-lib.calendar.week-test-cljs
  (:require [clojure.test :refer-macros [deftest is testing]]
            [js-lib.core :as md]
            [htmlcss-lib.core :refer [gen div svg polygon]]
            [svg-lib.calendar.core :as cc]
            [svg-lib.calendar.week :refer [display-month-name create-week-day-cell
                                           create-item add-item get-week-start-date
                                           get-week-end-date get-db-items-by-week-fn
                                           add-items-for-selected-week
                                           get-hours-minutes-and-seconds
                                           display-current-time-line draw-week-by-date
                                           switch-to-previous-week switch-to-next-week
                                           open-week-view]]))

(deftest test-display-month-name
  (testing "Test display month name"
    
    (md/remove-element
      ".month-name h2")
    
    (is
      (nil?
        (md/element-exists
          ".month-name h2"))
     )
    
    (.setMonth
      cc/displayed-date
      0)
    
    (display-month-name)
    
    (let [month-heading (md/query-selector
                          ".month-name h2")]
      
      (is
        (= (md/get-inner-html
             month-heading)
           "104 - 93 2019")
       )
      
     )
    
    (md/remove-element
      ".month-name h2")
    
    (is
      (nil?
        (md/element-exists
          ".month-name h2"))
     )
    
    (.setMonth
      cc/displayed-date
      1)
    
    (display-month-name)
    
    (let [month-heading (md/query-selector
                          ".month-name h2")]
      
      (is
        (= (md/get-inner-html
             month-heading)
           "93 - 94 2019")
       )
      
     )
    
    (md/remove-element
      ".month-name h2")
    
   ))

(deftest test-create-week-day-cell
  (testing "Test create week day cell"
    
    (let [day-date nil
          ax nil
          ay nil
          bx nil
          by nil
          cx nil
          cy nil
          dx nil
          dy nil
          result (create-week-day-cell
                   day-date
                   ax
                   ay
                   bx
                   by
                   cx
                   cy
                   dx
                   dy)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [day-date (js/Date.
                     "2019-08-25T12:00:00.000Z")
          ax nil
          ay nil
          bx nil
          by nil
          cx nil
          cy nil
          dx nil
          dy nil
          [{el-1 :el
            {class-1 :class
             points-1 :points
             date-1 :date} :attrs}
           {el-2 :el
            {class-2 :class
             x-2 :x
             y-2 :y} :attrs 
            cont-2 :cont}] (create-week-day-cell
                             day-date
                             ax
                             ay
                             bx
                             by
                             cx
                             cy
                             dx
                             dy)]
      
      (is
        (= el-1
           "polygon")
       )
      
      (is
        (= class-1
           "day-cell")
       )
      
      (is
        (= points-1
           "1,1 -1,1 -1,-1 1,-1")
       )
      
      (is
        (= date-1
           "20190825")
       )
      
      (is
        (= el-2
           "text")
       )
      
      (is
        (= class-2
           "day-number")
       )
      
      (is
        (= x-2
           -10)
       )
      
      (is
        (= y-2
           -5)
       )
      
      (is
        (= cont-2
           25)
       )
      
     )
    
   ))

(deftest test-create-item
  (testing "Test create item"
    
    (let [item nil
          axp nil
          ayp nil
          bxp nil
          byp nil
          cxp nil
          cyp nil
          dxp nil
          dyp nil
          view-type nil
          result (create-item
                   item
                   axp
                   ayp
                   bxp
                   byp
                   cxp
                   cyp
                   dxp
                   dyp
                   view-type)]
      
      (is
        (md/html?
          result)
       )
      
      (is
        (= (.getAttribute
             result
             "class")
           "item")
       )
      
      (is
        (= (.getAttribute
             result
             "points")
           "31,1 -1,1 -1,-1 31,-1")
       )
      
     )
    
    (let [item nil
          axp 3
          ayp 3
          bxp 6
          byp 3
          cxp 3
          cyp 6
          dxp 6
          dyp 6
          view-type "week"
          result (create-item
                   item
                   axp
                   ayp
                   bxp
                   byp
                   cxp
                   cyp
                   dxp
                   dyp
                   view-type)]
      
      (is
        (md/html?
          result)
       )
      
      (is
        (= (.getAttribute
             result
             "class")
           "item")
       )
      
      (is
        (= (.getAttribute
             result
             "points")
           "34,4 5,4 5,5 34,5")
       )
      
     )
    
   ))

(deftest test-add-item
  (testing "Test add item"
    
    (let [calendar-container-el (gen
                                  (div
                                    (svg
                                      (polygon
                                        nil
                                        {:points "1,1 499,1 499,499 1,499"
                                         :date "20190825"})
                                      {:width "500"
                                       :height "500"
                                       :view-type "week"})
                                    {:class "calendar-container"}))
          body-el (md/query-selector
                    "body")]
      (md/remove-element
        ".calendar-container svg")
      (md/append-element
        body-el
        calendar-container-el))
    
    (let [item nil
          result (add-item
                   item)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [start-datetime (js/Date.
                           "2019-08-25T12:00:00.000Z")
          end-datetime (js/Date.
                         "2019-08-25T13:00:00.000Z")
          item {:start-date start-datetime
                :end-date end-datetime}
          result (add-item
                   item)]
      
      (is
        (md/html?
          result)
       )
      
      (is
        (= (.getAttribute
             result
             "class")
           "item")
       )
      
      (is
        (= (.getAttribute
             result
             "points")
           "35,292 495,292 495,310.5 35,310.5")
       )
      
     )
    
   ))

(deftest test-get-week-start-date
  (testing "Test get week start date"
    
    (let [date nil
          result (get-week-start-date
                   date)]
      
      (is
        (= result
           (js/Date.
             "1969-12-28T23:00:00.000Z"))
       )
      
     )
    
    (let [date (js/Date.
                 "2018-08-25T12:00:00.000Z")
          result (get-week-start-date
                   date)]
      
      (is
        (= result
           (js/Date.
             "2018-08-19T22:00:00.000Z"))
       )
      
     )
    
    (let [date (js/Date.
                 "2018-08-31T12:00:00.000Z")
          result (get-week-start-date
                   date)]
      
      (is
        (= result
           (js/Date.
             "2018-08-26T22:00:00.000Z"))
       )
      
     )
    
   ))

(deftest test-get-week-end-date
  (testing "Test get week end date"
    
    (let [date nil
          result (get-week-end-date
                   date)]
      
      (is
        (= result
           (js/Date.
             "1970-01-04T22:59:59.999Z"))
       )
      
     )
    
    (let [date (js/Date.
                 "2019-08-25T12:00:00.000Z")
          result (get-week-end-date
                   date)]
      
      (is
        (= result
           (js/Date.
             "2019-08-25T21:59:59.999Z"))
       )
      
     )
    
    (let [date (js/Date.
                 "2019-08-31T12:00:00.000Z")
          result (get-week-end-date
                   date)]
      
      (is
        (= result
           (js/Date.
             "2019-09-01T21:59:59.999Z"))
       )
      
     )
    
   ))

(deftest test-get-db-items-by-week-fn
  (testing "Test get db items by week fn"
    
    (let [db-date (js/Date.
                    "2019-08-25T12:00:00.000Z")
          result (get-db-items-by-week-fn
                   db-date)]
      
      (is
        (= (count
             result)
           7)
       )
      
     )
    
   ))

(deftest test-add-items-for-selected-week
  (testing "Test add items for selected week"
    
    (let [result (add-items-for-selected-week)]
      
     )
    
   ))

(deftest test-get-hours-minutes-and-seconds
  (testing "Test get hours minutes and seconds"
    
    (let [date-param nil
          result (get-hours-minutes-and-seconds
                   date-param)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-08-25T02:02:02.000Z")
          result (get-hours-minutes-and-seconds
                   date-param)]
      
      (is
        (= result
           "04:02:02")
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-08-25T11:12:13.000Z")
          result (get-hours-minutes-and-seconds
                   date-param)]
      
      (is
        (= result
           "13:12:13")
       )
      
     )
    
   ))

(deftest test-display-current-time-line
  (testing "Test display current time line"
    
    (let [current-date (js/Date.)
          calendar-container-el (gen
                                  (div
                                    (svg
                                      (polygon
                                        nil
                                        {:points "1,1 499,1 499,499 1,499"
                                         :date (cc/format-date-id
                                                 current-date)})
                                      {:width "500"
                                       :height "500"
                                       :view-type "week"})
                                    {:class "calendar-container"}))
          body-el (md/query-selector
                    "body")]
      (md/remove-element
        ".calendar-container")
      (md/append-element
        body-el
        calendar-container-el))
    
    (display-current-time-line
      "week")
    
    (is
      (md/html?
        (md/query-selector
          ".calendar-container svg line[class='current-time-line']"))
     )
    
    (is
      (md/html?
        (md/query-selector
          ".calendar-container svg polygon[class='current-time-triangle']"))
     )
    
    (is
      (md/html?
        (md/query-selector
          ".calendar-container svg text[class='current-time-text']"))
     )
    
    (let [date-param (js/Date.
                       "2019-08-25T12:00:00.000Z")
          calendar-container-el (gen
                                  (div
                                    (svg
                                      (polygon
                                        nil
                                        {:points "1,1 499,1 499,499 1,499"
                                         :date (cc/format-date-id
                                                 date-param)})
                                      {:width "500"
                                       :height "500"
                                       :view-type "week"})
                                    {:class "calendar-container"}))
          body-el (md/query-selector
                    "body")]
      (md/remove-element
        ".calendar-container")
      (md/append-element
        body-el
        calendar-container-el))
    
    (display-current-time-line
      "week")
    
    (is
      (nil?
        (md/query-selector
          ".calendar-container svg line[class='current-time-line']"))
     )
    
    (is
      (nil?
        (md/query-selector
          ".calendar-container svg polygon[class='current-time-triangle']"))
     )
    
    (is
      (nil?
        (md/query-selector
          ".calendar-container svg text[class='current-time-text']"))
     )
    
   ))

(deftest test-draw-week-by-date
  (testing "Test draw week by date"
    
    (md/remove-element
      ".calendar-container svg")
      
    (is
      (nil?
        (md/query-selector
          ".calendar-container svg"))
     )
    
    (.setTime
      cc/displayed-date
      (.getTime
        (js/Date.
          "2019-08-25T12:00:00.000Z"))
     )
    
    (let [date-param nil
          result (draw-week-by-date
                   date-param)]
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg"))
       )
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg polygon[date='20190819']"))
       )
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg polygon[date='20190820']"))
       )
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg polygon[date='20190821']"))
       )
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg polygon[date='20190822']"))
       )
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg polygon[date='20190823']"))
       )
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg polygon[date='20190824']"))
       )
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg polygon[date='20190825']"))
       )
      
     )
    
   ))

(deftest test-switch-to-previous-week
  (testing "Test switch to previous week"
    
    (.setTime
      cc/displayed-date
      (.getTime
        (js/Date.
          "2019-08-26T12:00:00.000Z"))
     )
    
    (switch-to-previous-week)
    
    (let [non-existing-day-cell-el (md/query-selector
                                     (str
                                       ".calendar-container svg polygon[date='"
                                       (cc/format-date-id
                                         (js/Date.
                                           "2019-08-26T12:00:00.000Z"))
                                       "']"))
          existing-day-cell-el (md/query-selector
                                 (str
                                   ".calendar-container svg polygon[date='"
                                   (cc/format-date-id
                                     (js/Date.
                                       "2019-08-25T12:00:00.000Z"))
                                   "']"))]
      
      (is
        (nil?
          non-existing-day-cell-el)
       )
      
      (is
        (md/html?
          existing-day-cell-el)
       )
      
      (is
        (= (.getAttribute
             existing-day-cell-el
             "points")
           "796.4285714285716,51 927.0000000000001,51 927.0000000000001,-1 796.4285714285716,-1")
       )
      
     )
    
   ))

(deftest test-switch-to-next-week
  (testing "Test switch to next week"
    
    (.setTime
      cc/displayed-date
      (.getTime
        (js/Date.
          "2019-08-26T12:00:00.000Z"))
     )
    
    (switch-to-next-week)
    
    (let [non-existing-day-cell-el (md/query-selector
                                     (str
                                       ".calendar-container svg polygon[date='"
                                       (cc/format-date-id
                                         (js/Date.
                                           "2019-08-26T12:00:00.000Z"))
                                       "']"))
          existing-day-cell-el (md/query-selector
                                 (str
                                   ".calendar-container svg polygon[date='"
                                   (cc/format-date-id
                                     (js/Date.
                                       "2019-09-02T12:00:00.000Z"))
                                   "']"))]
      
      (is
        (nil?
          non-existing-day-cell-el)
       )
      
      (is
        (md/html?
          existing-day-cell-el)
       )
      
      (is
        (= (.getAttribute
             existing-day-cell-el
             "points")
           "1,51 131.57142857142858,51 131.57142857142858,-1 1,-1")
       )
      
     )
    
   ))

(deftest test-open-week-view
  (testing "Test open week view"
    
    (md/remove-element
      ".calendar-container svg")
      
    (is
      (nil?
        (md/query-selector
          ".calendar-container svg"))
     )
    
    (let [last-week-day (js/Date.
                          "2019-08-04T12:00:00.000Z")
          month-week-number 4
          week-number 35
          evt-p {:last-week-day last-week-day
                 :month-week-number month-week-number
                 :week-number week-number}
          result (open-week-view
                   evt-p)]
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg"))
       )
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg polygon[date='20190826']"))
       )
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg polygon[date='20190827']"))
       )
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg polygon[date='20190828']"))
       )
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg polygon[date='20190829']"))
       )
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg polygon[date='20190830']"))
       )
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg polygon[date='20190831']"))
       )
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg polygon[date='20190901']"))
       )
      
     )
    
    
    
   ))


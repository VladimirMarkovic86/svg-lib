(ns svg-lib.calendar.day-test-cljs
  (:require [clojure.test :refer-macros [deftest is testing]]
            [js-lib.core :as md]
            [htmlcss-lib.core :refer [gen div svg polygon]]
            [svg-lib.calendar.core :as cc]
            [svg-lib.calendar.week :as cw]
            [svg-lib.calendar.day :refer [get-db-items-by-day-fn
                                          add-items-for-selected-day
                                          refresh-onresize-event-month
                                          refresh-onresize-event-week
                                          refresh-onresize-event-day refresh-onresize-event
                                          draw-day-by-date switch-to-previous-day
                                          switch-to-next-day open-day-view]]))

(deftest test-get-db-items-by-day-fn
  (testing "Test get db items by day fn"
    
    (let [db-date (js/Date.
                    "2019-08-26T12:00:00.000Z")
          result (get-db-items-by-day-fn
                   db-date)]
      
      (is
        (= (count
             result)
           1)
       )
      
     )
    
   ))

(deftest test-add-items-for-selected-day
  (testing "Test add items for selected day"
    
    (let [result (add-items-for-selected-day)]
      
      
      
     )
    
   ))

(deftest test-refresh-onresize-event-month
  (testing "Test refresh onresize event month"
    
    (let [result (refresh-onresize-event-month)]
      
     )
    
   ))

(deftest test-refresh-onresize-event-week
  (testing "Test refresh onresize event week"
    
    (let [result (refresh-onresize-event-week)]
      
     )
    
   ))

(deftest test-refresh-onresize-event-day
  (testing "Test refresh onresize event day"
    
    (let [result (refresh-onresize-event-day)]
      
     )
    
   ))

(deftest test-refresh-onresize-event
  (testing "Test refresh onresize event"
    
    (let [result (refresh-onresize-event)]
      
     )
    
   ))

(deftest test-draw-day-by-date
  (testing "Test draw day by date"
    
    (.setTime
      cc/displayed-date
      (.getTime
        (js/Date.
          "2019-08-26T12:00:00.000Z"))
     )
    
    (let [date-param nil
          result (draw-day-by-date
                   date-param)]
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg[view-type='day'] polygon[date='20190826']"))
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-08-25T12:00:00.000Z")
          result (draw-day-by-date
                   date-param)]
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg[view-type='day'] polygon[date='20190825']"))
       )
      
     )
    
   ))

(deftest test-switch-to-previous-day
  (testing "Test switch to previous day"
    
    (.setTime
      cc/displayed-date
      (.getTime
        (js/Date.
          "2019-08-26T12:00:00.000Z"))
     )
    
    (switch-to-previous-day)
    
    (is
      (md/html?
        (md/query-selector
          ".calendar-container svg[view-type='day'] polygon[date='20190825']"))
     )
    
   ))

(deftest test-switch-to-next-day
  (testing "Test switch to next day"
    
    (.setTime
      cc/displayed-date
      (.getTime
        (js/Date.
          "2019-08-26T12:00:00.000Z"))
     )
    
    (switch-to-next-day)
    
    (is
      (md/html?
        (md/query-selector
          ".calendar-container svg[view-type='day'] polygon[date='20190827']"))
     )
    
   ))

(deftest test-open-day-view
  (testing "Test open day view"
    
    (.setTime
      cc/displayed-date
      (.getTime
        (js/Date.
          "2019-08-26T12:00:00.000Z"))
     )
    
    (let [evt-p nil
          element nil
          event nil
          void (open-day-view
                 evt-p
                 element
                 event)]
      
      (is
        (md/html?
          (md/query-selector
            ".calendar-container svg[view-type='day'] polygon[date='20190826']"))
       )
      
     )
    
   ))


(ns svg-lib.calendar.core-test-cljs
  (:require [clojure.test :refer-macros [deftest is testing]]
            [js-lib.core :as md]
            [htmlcss-lib.core :refer [gen div svg polygon]]
            [svg-lib.calendar.core :refer [displayed-date format-date-id format-datetime-id
                                           get-hours-and-minutes is-day-in-first-week-fn
                                           append-item-details remove-item-details
                                           create-item timezone-offset refresh-view
                                           save-item delete-item get-item-by-id
                                           create-item-form insert-edit-item-fn
                                           display-add-button hide-add-button
                                           create-day-cell get-month-name
                                           display-month-name calculate-day-x
                                           day-name-by-number display-day-name
                                           display-day-names calculate-number-of-weeks
                                           add-item draw-items-list
                                           set-time-at-day-beginning set-time-at-day-ending
                                           get-start-date get-end-date
                                           get-db-items-by-month-fn
                                           add-items-for-selected-month
                                           get-month-week-numbers display-week-numbers
                                           draw-by-date switch-to-previous-month
                                           switch-to-next-month]]))

(deftest test-format-date-id
  (testing "Test format date id"
    
    (let [format-date nil
          result (format-date-id
                   format-date)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [format-date (js/Date.
                        "2019-08-13")
          result (format-date-id
                   format-date)]
      
      (is
        (= result
           "20190813")
       )
      
     )
    
   ))

(deftest test-format-datetime-id
  (testing "Test format datetime id"
    
    (let [format-date nil
          result (format-datetime-id
                   format-date)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [format-date (js/Date.
                        "2019-08-13T01:01:01.001Z")
          result (format-datetime-id
                   format-date)]
      
      (is
        (= result
           "20190813030101001")
       )
      
     )
    
   ))

(deftest test-get-hours-and-minutes
  (testing "Test get hours and minutes"
    
    (let [date-param nil
          result (get-hours-and-minutes
                   date-param)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-08-14T13:00:00.000Z")
          result (get-hours-and-minutes
                   date-param)]
      
      (is
        (= result
           "15:00")
       )
      
      (is
        (not= result
              "13:00")
       )
      
     )
    
   ))

(deftest test-is-day-in-first-week-fn
  (testing "Test is day in first week fn"
    
    (let [date-param nil
          result (is-day-in-first-week-fn
                   date-param)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-08-15")
          result (is-day-in-first-week-fn
                   date-param)]
      
      (is
        (false?
          result)
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-08-04")
          result (is-day-in-first-week-fn
                   date-param)]
      
      (is
        (true?
          result)
       )
      
     )
    
   ))

(deftest test-append-item-details-and-remove-item-details
  (testing "Test append item details and remove item details"
    
    (let [calendar-container-el (gen
                                  (div
                                    (svg
                                      nil
                                      {:width "500"
                                       :height "500"
                                       :view-type "month"})
                                    {:class "calendar-container"}))
          body-el (md/query-selector
                    "body")]
      (md/append-element
        body-el
        calendar-container-el))
    
    (let [evt-p nil
          element nil
          event nil]
      
      (append-item-details
        evt-p
        element
        event)
      
      (is
        (not
          (nil?
            (md/element-exists
              ".calendar-container svg .item-details-name"))
         )
       )
      
      (is
        (not
          (nil?
            (md/element-exists
              ".calendar-container svg .item-details-type"))
         )
       )
      
      (is
        (not
          (nil?
            (md/element-exists
              ".calendar-container svg .item-details-time"))
         )
       )
      
      (remove-item-details
        evt-p
        element
        event)
      
      (is
        (nil?
          (md/element-exists
            ".calendar-container svg .item-details-name"))
       )
      
      (is
        (nil?
          (md/element-exists
            ".calendar-container svg .item-details-type"))
       )
      
      (is
        (nil?
          (md/element-exists
            ".calendar-container svg .item-details-time"))
       )
      
     )
    
    (let [item-start-date (js/Date.
                            "2019-08-16T12:12:12.121Z")
          item-end-date (js/Date.
                          "2019-08-16T13:12:12.121Z")
          item-name "Item name"
          item-type "Item type"
          ax 1
          ay 1
          bx 1
          by 1
          cx 1
          cy 1
          dx 1
          dy 1
          evt-p {:item {:start-date item-start-date
                        :end-date item-end-date
                        :name item-name
                        :type item-type}
                 :coordinates [ax
                               ay
                               bx
                               by
                               cx
                               cy
                               dx
                               dy]}
          element nil
          event nil]
      
      (append-item-details
        evt-p
        element
        event)
      
      (is
        (not
          (nil?
            (md/element-exists
              ".calendar-container svg .item-details-name"))
         )
       )
      
      (is
        (not
          (nil?
            (md/element-exists
              ".calendar-container svg .item-details-type"))
         )
       )
      
      (is
        (not
          (nil?
            (md/element-exists
              ".calendar-container svg .item-details-time"))
         )
       )
      
      (remove-item-details
        evt-p
        element
        event)
      
      (is
        (nil?
          (md/element-exists
            ".calendar-container svg .item-details-name"))
       )
      
      (is
        (nil?
          (md/element-exists
            ".calendar-container svg .item-details-type"))
       )
      
      (is
        (nil?
          (md/element-exists
            ".calendar-container svg .item-details-time"))
       )
      
     )
    
   ))

(deftest test-create-item
  (testing "Test create item"
    
    (let [item nil
          ax nil
          ay nil
          bx nil
          by nil
          cx nil
          cy nil
          dx nil
          dy nil
          position-x nil
          position-y nil
          result (create-item
                   item
                   ax
                   ay
                   bx
                   by
                   cx
                   cy
                   dx
                   dy
                   position-x
                   position-y)]
      
      (is
        (md/html?
          result)
       )
      
      (is
        (= (.getAttribute
             result
             "points")
           "1,1 -1,1 -1,-1 1,-1")
       )
      
      (is
        (empty?
          (.getAttribute
            result
            "position-x"))
       )
      
      (is
        (empty?
          (.getAttribute
            result
            "position-y"))
       )
      
      (is
        (nil?
          (aget
            result
            "start-date"))
       )
      
      (is
        (nil?
          (aget
            result
            "end-date"))
       )
      
     )
    
    (let [item {:start-date (js/Date.
                              "2019-08-13T12:00:00.000Z")
                :end-date (js/Date.
                            "2019-08-13T13:00:00.000Z")}
          ax [1]
          ay [1]
          bx [1]
          by [1]
          cx [1]
          cy [1]
          dx [1]
          dy [1]
          position-x 1
          position-y 1
          result (create-item
                   item
                   ax
                   ay
                   bx
                   by
                   cx
                   cy
                   dx
                   dy
                   position-x
                   position-y)]
      
      (is
        (md/html?
          result)
       )
      
      (is
        (= (.getAttribute
             result
             "points")
           "2,2 0,2 0,0 2,0")
       )
      
      (is
        (= (.getAttribute
             result
             "position-x")
           "1")
       )
      
      (is
        (= (.getAttribute
             result
             "position-y")
           "1")
       )
      
      (is
        (= (aget
             result
             "start-date")
           (js/Date.
             "2019-08-13T12:00:00.000Z"))
       )
      
      (is
        (= (aget
             result
             "end-date")
           (js/Date.
             "2019-08-13T13:00:00.000Z"))
       )
      
     )
    
   ))

(deftest test-timezone-offset
  (testing "Test timezone offset"
    
    (let [date-param (js/Date.
                       "2019-08-15")
          result (timezone-offset
                   date-param)]
      ; +2h Belgrade Serbia
      (is
        (= result
           -2)
       )
      
     )
    
   ))

(deftest test-refresh-view
  (testing "Test refresh view"
    
    (let [result (refresh-view)]
      
     )
    
   ))

(deftest test-save-item
  (testing "Test save item"
    
    (let []
      ; test sjax functionality
     )
    
   ))

(deftest test-delete-item
  (testing "Test delete item"
    
    (let [result (delete-item)]
      
     )
    
   ))

(deftest test-get-item-by-id
  (testing "Test get item by id"
    
    (let [;_id nil
          ;result (get-item-by-id
          ;         _id)
                   ]
      ; test sjax functionality
     )
    
   ))

(deftest test-create-item-form
  (testing "Test create item form"
    
    (let [evt-p nil
          result (create-item-form
                   evt-p)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [evt-p {:item {:start-date (js/Date.
                                      "2019-08-16T12:12:12.121")
                        :end-date (js/Date.
                                    "2019-08-16T13:12:12.121")}}
          result (create-item-form
                   evt-p)
          [{el-fieldset :el}
           {el-div :el}] result]
      
      (is
        (= el-fieldset
           "fieldset")
       )
      
      (is
        (= el-div
           "div")
       )
      
     )
    
   ))

(deftest test-insert-edit-item-fn
  (testing "Test insert edit item fn"
    
    (let [evt-p nil
          result (insert-edit-item-fn
                   evt-p)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [evt-p {:item {:start-date (js/Date.
                                      "2019-08-16T12:12:12.121")
                        :end-date (js/Date.
                                    "2019-08-16T13:12:12.121")}}]
      
      (md/remove-element
        ".popup-modal")
      
      (is
        (nil?
          (md/element-exists
            ".popup-modal .entity"))
       )
      
      (insert-edit-item-fn
        evt-p)
      
      (is
        (not
          (nil?
            (md/element-exists
              ".popup-modal .entity"))
         )
       )
      
      (md/remove-element
        ".popup-modal")
      
      (is
        (nil?
          (md/element-exists
            ".popup-modal .entity"))
       )
      
     )
    
   ))

(deftest test-display-add-button
  (testing "Test display add button"
    
    (let [calendar-container-el (gen
                                  (div
                                    (svg
                                      nil
                                      {:width "500"
                                       :height "500"
                                       :view-type "month"})
                                    {:class "calendar-container"}))
          body-el (md/query-selector
                    "body")]
      (md/append-element
        body-el
        calendar-container-el))
    
    (let [date-id nil
          dx nil
          dy nil
          day-date nil]
      
      (display-add-button
        {:date-id date-id
         :dx dx
         :dy dy
         :date day-date})
      
      (is
        (not
          (nil?
            (md/element-exists
              ".calendar-container svg .add-button"))
         )
       )
      
      (is
        (not
          (nil?
            (md/element-exists
              ".calendar-container svg text[id='_add_btn']"))
         )
       )
      
     )
    
    (hide-add-button
      {:date-id nil}
      nil
      (clj->js
        {:explicitOriginalTarget {:data "+"}}))
   
   ))

(deftest test-hide-add-button
  (testing "Test hide add button"
    
    (let [date-id nil
          element nil
          event nil
          result (hide-add-button
                   {:date-id date-id}
                   element
                   event)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (display-add-button
      {:date-id nil
       :dx nil
       :dy nil
       :date nil})
    
    (let [date-id nil
          element nil
          event (clj->js
                  {:explicitOriginalTarget {:data "not plus"}})]
      
      (is
        (not
          (empty?
            (md/element-exists
              ".calendar-container svg text[id='_add_btn']"))
         )
       )
      
      (hide-add-button
        {:date-id date-id}
        element
        event)
      
      (is
        (nil?
          (md/element-exists
            ".calendar-container svg text[id='_add_btn']"))
       )
      
     )
    
   ))

(deftest test-create-day-cell
  (testing "Test create day cell"
    
    (let [day-date nil
          ax nil
          ay nil
          bx nil
          by nil
          cx nil
          cy nil
          dx nil
          dy nil
          [day-cell-el-r
           day-cell-num-r] (create-day-cell
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
          day-cell-el-r)
       )
      
      (is
        (nil?
          day-cell-num-r)
       )
      
     )
    
    (let [day-date (js/Date.
                     "2019-08-13")
          ax nil
          ay nil
          bx nil
          by nil
          cx nil
          cy nil
          dx nil
          dy nil
          [day-cell-el-r
           day-cell-num-r] (create-day-cell
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
        (= (:el day-cell-el-r)
           "polygon")
       )
      
      (is
        (not
          (nil?
            (:events day-cell-el-r))
         )
       )
      
      (is
        (= (:attrs day-cell-el-r)
           {:class "day-cell",
            :points "1,1 -1,1 -1,-1 1,-1",
            :date "20190813"})
       )
      
      (is
        (= (:el day-cell-num-r)
           "text")
       )
      
      (is
        (= (:attrs day-cell-num-r)
           {:class "day-number", :x 6, :y -6})
       )
      
      (is
        (= (:cont day-cell-num-r)
           13)
       )
      
     )
    
   ))

(deftest test-get-month-name
  (testing "Test get month name"
    
    (let [month-num nil
          result (get-month-name
                   month-num)]
      
      (is
        (= result
           "Error")
       )
      
     )
    
    (let [month-num 0
          result (get-month-name
                   month-num)]
      
      (is
        (= result
           93)
       )
      
     )
    
    (let [month-num 1
          result (get-month-name
                   month-num)]
      
      (is
        (= result
           94)
       )
      
     )
    
   ))

(deftest test-display-month-name
  (testing "Test display month name"
    
    (let [month-div (gen
                      (div
                        nil
                        {:class "month-name"}))
          body-el (md/query-selector
                    "body")]
      (md/append-element
        body-el
        month-div))
    
    (.setTime
      displayed-date
      (.getTime
        (js/Date.
          "2019-08-13"))
     )
    
    (let [result (display-month-name)]
      
      (is
        (= result
           "<h2>100 2019</h2>")
       )
            
     )
    
   ))

(deftest test-calculate-day-x
  (testing "Test calculate day x"
    
    (let [day-cell-width nil
          day-number nil
          day-name-width nil
          result (calculate-day-x
                   day-cell-width
                   day-number
                   day-name-width)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [day-cell-width 22
          day-number 0
          day-name-width 13
          result (calculate-day-x
                   day-cell-width
                   day-number
                   day-name-width)]
      
      (is
        (= result
           4.5)
       )
      
     )
    
    (let [day-cell-width 22
          day-number 3
          day-name-width 15
          result (calculate-day-x
                   day-cell-width
                   day-number
                   day-name-width)]
      
      (is
        (= result
           69.5)
       )
      
     )
    
   ))

(deftest test-day-name-by-number
  (testing "Test day-name-by-number"
    
    (let [day-num nil
          result (day-name-by-number
                   day-num)]
      
      (is
        (= result
           "Unknown")
       )
      
     )
    
    (let [day-num 0
          result (day-name-by-number
                   day-num)]
      
      (is
        (= result
           111)
       )
      
     )
    
    (let [day-num 1
          result (day-name-by-number
                   day-num)]
      
      (is
        (= result
           105)
       )
      
     )
    
    (let [day-num 2
          result (day-name-by-number
                   day-num)]
      
      (is
        (= result
           106)
       )
      
     )
    
    (let [day-num 3
          result (day-name-by-number
                   day-num)]
      
      (is
        (= result
           107)
       )
      
     )
    
    (let [day-num 4
          result (day-name-by-number
                   day-num)]
      
      (is
        (= result
           108)
       )
      
     )
    
    (let [day-num 5
          result (day-name-by-number
                   day-num)]
      
      (is
        (= result
           109)
       )
      
     )
    
    (let [day-num 6
          result (day-name-by-number
                   day-num)]
      
      (is
        (= result
           110)
       )
      
     )
    
    (let [day-num 7
          result (day-name-by-number
                   day-num)]
      
      (is
        (= result
           111)
       )
      
     )
    
   ))

(deftest test-display-day-name
  (testing "Test display day name"
    
    (let [day-cell-width nil
          width nil
          day-date nil
          result (display-day-name
                   day-cell-width
                   width
                   day-date)]
      
     )
    
   ))

(deftest test-display-day-names
  (testing "Test display day names"
    
    (md/remove-element-content
      ".calendar-container svg")
    
    (let [day-cell-width nil
          width nil
          week-numbers-column-offset nil]
      
      (is
        (= (count
             (md/element-exists
               ".calendar-container svg .day-name"))
           0)
       )
      
      (display-day-names
        day-cell-width
        width
        week-numbers-column-offset)
      
      (is
        (md/element-exists
          ".calendar-container svg .day-name-background")
       )
      
      (let [day-name-background-el (md/query-selector
                                     ".calendar-container svg .day-name-background")]
        (is
          (= (.getAttribute
               day-name-background-el
               "points")
             ",0 0,0 0,25 ,25")
         )
       )
      
      (is
        (= (count
             (md/element-exists
               ".calendar-container svg .day-name"))
           7)
       )
      
      (let [monday-el (md/query-selector
                        ".calendar-container svg text[day-number='0']")]
        
        (is
          (= (.getAttribute
               monday-el
               "day-number")
             "0")
         )
        
        (is
          (= (.getAttribute
               monday-el
               "x")
             "-15")
         )
        
        (is
          (= (.getAttribute
               monday-el
               "y")
             "18")
         )
        
       )
      
     )
    
    (md/remove-element
      ".calendar-container svg .day-name")
    
    (md/remove-element
      ".calendar-container svg .day-name-background")
    
    (let [day-cell-width 50
          width 300
          week-numbers-column-offset 25]
      
      (is
        (= (count
             (md/element-exists
               ".calendar-container svg .day-name"))
           0)
       )
      
      (display-day-names
        day-cell-width
        (- width
           week-numbers-column-offset)
        week-numbers-column-offset)
      
      (is
        (md/element-exists
          ".calendar-container svg .day-name-background")
       )
      
      (let [day-name-background-el (md/query-selector
                                     ".calendar-container svg .day-name-background")]
        (is
          (= (.getAttribute
               day-name-background-el
               "points")
             "25,0 300,0 300,25 25,25")
         )
       )
      
      (is
        (= (count
             (md/element-exists
               ".calendar-container svg .day-name"))
           7)
       )
      
      (let [monday-el (md/query-selector
                        ".calendar-container svg text[day-number='0']")]
        
        (is
          (= (.getAttribute
               monday-el
               "day-number")
             "0")
         )
        
        (is
          (= (.getAttribute
               monday-el
               "x")
             "35")
         )
        
        (is
          (= (.getAttribute
               monday-el
               "y")
             "18")
         )
        
       )
      
     )
    
   ))

(deftest test-calculate-number-of-weeks
  (testing "Test calculate number of weeks"
    
    (let [number-of-weeks-date nil
          result (calculate-number-of-weeks
                   number-of-weeks-date)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [number-of-weeks-date (js/Date.
                                 "2019-08-13")
          result (calculate-number-of-weeks
                   number-of-weeks-date)]
      
      (is
        (= result
           5)
       )
      
     )
    
    (let [number-of-weeks-date (js/Date.
                                 "2021-02-13")
          result (calculate-number-of-weeks
                   number-of-weeks-date)]
      
      (is
        (= result
           4)
       )
      
     )
    
    (let [number-of-weeks-date (js/Date.
                                 "2019-12-13")
          result (calculate-number-of-weeks
                   number-of-weeks-date)]
      
      (is
        (= result
           6)
       )
      
     )
    
   ))

(deftest test-add-item
  (testing "Test add item"
    
    (let [item nil
          rows nil
          columns nil
          position-x nil
          position-y nil
          result (add-item
                   item
                   rows
                   columns
                   position-x
                   position-y)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [date-polygon (gen
                         (polygon
                           nil
                           {:points "1,1 -1,1 -1,-1 1,-1"
                            :date (format-date-id
                                    (js/Date.
                                      "2019-08-13T12:00:00.000Z"))}
                          ))
          svg-el (md/query-selector
                   ".calendar-container svg")]
      (md/append-element
        svg-el
        date-polygon))
    
    (let [item {:start-date (js/Date.
                              "2019-08-13T12:00:00.000Z")
                :end-date (js/Date.
                            "2019-08-13T13:00:00.000Z")}
          rows 1
          columns 1
          position-x 0
          position-y 0]
      
      (add-item
        item
        rows
        columns
        position-x
        position-y)
      
      (let [item-el (md/query-selector
                      (str
                        ".calendar-container svg polygon[start-datetime-id='"
                        (format-datetime-id
                          (js/Date.
                            "2019-08-13T12:00:00.000Z"))
                        "']"))]
        
        (is
          (= (.getAttribute
               item-el
               "points")
             "5,5 -5,5 -5,-30 5,-30")
         )
        
        (is
          (= (.getAttribute
               item-el
               "position-x")
             "0")
         )
        
        (is
          (= (.getAttribute
               item-el
               "position-y")
             "0")
         )
        
        (is
          (= (.getAttribute
               item-el
               "end-datetime-id")
             "20190813150000000")
         )
        
       )
      
     )
    
   ))

(deftest test-draw-items-list
  (testing "Test draw items list"
    
    (let [items nil
          rows nil
          columns nil]
      
      (draw-items-list
        items
        rows
        columns)
      
     )
    
    (let [date-polygon (gen
                         (polygon
                           nil
                           {:points "1,1 -1,1 -1,-1 1,-1"
                            :date (format-date-id
                                    (js/Date.
                                      "2019-08-14T12:00:00.000Z"))}
                          ))
          svg-el (md/query-selector
                   ".calendar-container svg")]
      (md/append-element
        svg-el
        date-polygon))
    
    (let [items [{:start-date (js/Date.
                                "2019-08-14T12:00:00.000Z")
                  :end-date (js/Date.
                              "2019-08-14T14:00:00.000Z")}]
          rows 1
          columns 1]
      
      (draw-items-list
        items
        rows
        columns)
      
      (let [added-item-el (md/query-selector
                            (str
                              ".calendar-container svg polygon[start-datetime-id='"
                              (format-datetime-id
                                (js/Date.
                                  "2019-08-14T12:00:00.000Z"))
                              "']"))]
        
        (is
          (not
            (nil?
              added-item-el))
         )
        
        (is
          (= (.getAttribute
               added-item-el
               "points")
             "5,5 -5,5 -5,-30 5,-30")
         )
        
        (is
          (= (.getAttribute
               added-item-el
               "position-x")
             "0")
         )
        
        (is
          (= (.getAttribute
               added-item-el
               "position-y")
             "0")
         )
        
        (is
          (= (.getAttribute
               added-item-el
               "end-datetime-id")
             "20190814160000000")
         )
        
       )
      
     )
    
   ))

(deftest test-set-time-at-day-beginning
  (testing "Test set time at day beginning"
    
    (let [date-param (js/Date.
                       "2019-08-15T12:12:12.121Z")]
      
      (set-time-at-day-beginning
        date-param)
      
      (is
        (= (.getHours
             date-param)
           0)
       )
      
      (is
        (= (.getMinutes
             date-param)
           0)
       )
      
      (is
        (= (.getSeconds
             date-param)
           0)
       )
      
      (is
        (= (.getMilliseconds
             date-param)
           0)
       )
      
     )
    
   ))

(deftest test-set-time-at-day-ending
  (testing "Test set time at day ending"
    
    (let [date-param (js/Date.
                       "2019-08-15T12:12:12.121Z")]
      
      (set-time-at-day-ending
        date-param)
      
      (is
        (= (.getHours
             date-param)
           23)
       )
      
      (is
        (= (.getMinutes
             date-param)
           59)
       )
      
      (is
        (= (.getSeconds
             date-param)
           59)
       )
      
      (is
        (= (.getMilliseconds
             date-param)
           999)
       )
      
     )
    
   ))

(deftest test-get-start-date
  (testing "Test get start date"
    
    (let [date-param (js/Date.
                       "2019-08-15T12:12:12.121Z")
          result-date (get-start-date
                        date-param)]
      
      (is
        (= (.getFullYear
             result-date)
           2019)
       )
      
      (is
        (= (.getMonth
             result-date)
           7)
       )
      
      (is
        (= (.getDate
             result-date)
           1)
       )
      
      (is
        (= (.getHours
             result-date)
           0)
       )
      
      (is
        (= (.getMinutes
             result-date)
           0)
       )
      
      (is
        (= (.getSeconds
             result-date)
           0)
       )
      
      (is
        (= (.getMilliseconds
             result-date)
           0)
       )
      
     )
    
   ))

(deftest test-get-end-date
  (testing "Test get end date"
    
    (let [date-param (js/Date.
                       "2019-08-15T12:12:12.121Z")
          result-date (get-end-date
                        date-param)]
      
      (is
        (= (.getFullYear
             result-date)
           2019)
       )
      
      (is
        (= (.getMonth
             result-date)
           7)
       )
      
      (is
        (= (.getDate
             result-date)
           31)
       )
      
      (is
        (= (.getHours
             result-date)
           23)
       )
      
      (is
        (= (.getMinutes
             result-date)
           59)
       )
      
      (is
        (= (.getSeconds
             result-date)
           59)
       )
      
      (is
        (= (.getMilliseconds
             result-date)
           999)
       )
      
     )
    
   ))

(deftest test-get-db-items-by-month-fn
  (testing "Test get db items by month fn"
    
    (let [result (get-db-items-by-month-fn
                   (js/Date.
                     "2019-08-14T12:00:00.000Z"))]
      
      (is
        (= (count
             result)
           31)
       )
      
     )
    
   ))

(deftest test-add-items-for-selected-month
  (testing "Test add items for selected month"
    
    (let [date-polygon (gen
                         (polygon
                           nil
                           {:points "1,1 -1,1 -1,-1 1,-1"
                            :date (format-date-id
                                    (js/Date.
                                      "2019-08-15T12:00:00.000Z"))}
                          ))
          svg-el (md/query-selector
                   ".calendar-container svg")]
      (md/append-element
        svg-el
        date-polygon))
    
    (add-items-for-selected-month)
    
    (let [item-el (md/query-selector
                    (str
                      ".calendar-container svg polygon[start-datetime-id='"
                      (format-datetime-id
                        (js/Date.
                          "2019-08-15T12:00:00.000Z"))
                      "']"))]
      
      ;(is
      ;  (= (.getAttribute
      ;       item-el
      ;       "points")
      ;     "5,5 -5,5 -5,-30 5,-30")
      ; )
      
      ;(is
      ;  (= (.getAttribute
      ;       item-el
      ;       "position-x")
      ;     "0")
      ; )
      
      ;(is
      ;  (= (.getAttribute
      ;       item-el
      ;       "position-y")
      ;     "0")
      ; )
      
      ;(is
      ;  (= (.getAttribute
      ;       item-el
      ;       "end-datetime-id")
      ;     "20190815160000000")
      ; )
      
     )
    
   ))

(deftest test-get-month-week-numbers
  (testing "Test get month week numbers"
    
    (let [date-param nil
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-01-01")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [1
            (js/Date.
              "2019-01-06T00:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-01-06")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [1
            (js/Date.
              "2019-01-06T00:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-01-15")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [1
            (js/Date.
              "2019-01-06T00:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-01-25")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [1
            (js/Date.
              "2019-01-06T00:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-02-01")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [5
            (js/Date.
              "2019-02-03T00:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-02-07")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [5
            (js/Date.
              "2019-02-03T00:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-02-15")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [5
            (js/Date.
              "2019-02-03T00:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-02-25")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [5
            (js/Date.
              "2019-02-03T00:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-03-25")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [9
            (js/Date.
              "2019-03-03T00:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-04-25")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [14
            (js/Date.
              "2019-04-07T01:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-05-25")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [18
            (js/Date.
              "2019-05-05T01:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-06-25")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [22
            (js/Date.
              "2019-06-02T01:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-07-25")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [27
            (js/Date.
              "2019-07-07T01:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-08-25")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [31
            (js/Date.
              "2019-08-04T01:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-09-25")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [35
            (js/Date.
              "2019-09-01T01:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-10-25")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [40
            (js/Date.
              "2019-10-06T01:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-11-25")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [44
            (js/Date.
              "2019-11-03T00:00:00.000Z")])
       )
      
     )
    
    (let [date-param (js/Date.
                       "2019-12-25")
          result (get-month-week-numbers
                   date-param)]
      
      (is
        (= result
           [48
            (js/Date.
              "2019-12-01T00:00:00.000Z")])
       )
      
     )
    
   ))

(deftest test-display-week-numbers
  (testing "Test display week numbers"
    
    (let [day-names-row-offset 25
          height 443
          number-of-weeks 5
          day-cell-height (/ (- 443
                                25)
                             number-of-weeks)]
      
      (.setTime
        displayed-date
        (.getTime
          (js/Date.
            "2019-01-01"))
       )
      
      (md/remove-element
        ".calendar-container svg .week-number-background")
      
      (md/remove-element
        ".calendar-container svg .week-number-text")
      
      (display-week-numbers
        day-names-row-offset
        height
        day-cell-height
        number-of-weeks)
      
      (is
        (not
          (nil?
            (md/element-exists
               ".calendar-container svg .week-number-background"))
         )
       )
      
      (is
        (= (count
             (md/element-exists
               ".calendar-container svg .week-number-text"))
           number-of-weeks)
       )
      
      (md/remove-element
        ".calendar-container svg .week-number-background")
      
      (md/remove-element
        ".calendar-container svg .week-number-text")
      
     )
    
   ))

(deftest test-draw-by-date
  (testing "Test draw by date"
    
    (draw-by-date
      (js/Date.
        "2019-09-15T12:00:00.000Z"))
    
    (let [item-el (md/query-selector
                    (str
                      ".calendar-container svg polygon[start-datetime-id='"
                      (format-datetime-id
                        (js/Date.
                          "2019-09-15T12:00:00.000Z"))
                      "']"))]
      
      ;(is
      ;  (= (.getAttribute
      ;       item-el
      ;       "points")
      ;     "800.4285714285716,21.666666666666668 923.0000000000001,21.666666666666668 923.0000000000001,-17.500000000000004 800.4285714285716,-17.500000000000004")
      ; )
      
      ;(is
      ;  (= (.getAttribute
      ;       item-el
      ;       "position-x")
      ;     "0")
      ; )
      
      ;(is
      ;  (= (.getAttribute
      ;       item-el
      ;       "position-y")
      ;     "0")
      ; )
      
      ;(is
      ;  (= (.getAttribute
      ;       item-el
      ;       "end-datetime-id")
      ;     "20190915160000000")
      ; )
      
     )
    
   ))

(deftest test-switch-to-previous-month
  (testing "Test switch to previous month"
    
    (.setTime
      displayed-date
      (.getTime
        (js/Date.
          "2019-07-01T12:00:00.000Z"))
     )
    
    (switch-to-previous-month)
    
    (let [non-existing-day-cell-el (md/query-selector
                                     (str
                                       ".calendar-container svg polygon[date='"
                                       (format-date-id
                                         (js/Date.
                                           "2019-07-15T12:00:00.000Z"))
                                       "']"))
          existing-day-cell-el (md/query-selector
                                 (str
                                   ".calendar-container svg polygon[date='"
                                   (format-date-id
                                     (js/Date.
                                       "2019-06-15T12:00:00.000Z"))
                                   "']"))]
      
      (is
        (nil?
          non-existing-day-cell-el)
       )
      
      (is
        (= (.getAttribute
             existing-day-cell-el
             "points")
           "662.4285714285714,16 787.7142857142858,16 787.7142857142858,9 662.4285714285714,9")
       )
      
     )
    
   ))

(deftest test-switch-to-next-month
  (testing "Test switch to next month"
    
    (.setTime
      displayed-date
      (.getTime
        (js/Date.
          "2019-07-01T12:00:00.000Z"))
     )
    
    (switch-to-next-month)
    
    (let [non-existing-day-cell-el (md/query-selector
                                     (str
                                       ".calendar-container svg polygon[date='"
                                       (format-date-id
                                         (js/Date.
                                           "2019-07-15T12:00:00.000Z"))
                                       "']"))
          existing-day-cell-el (md/query-selector
                                 (str
                                   ".calendar-container svg polygon[date='"
                                   (format-date-id
                                     (js/Date.
                                       "2019-08-15T12:00:00.000Z"))
                                   "']"))]
      
      (is
        (nil?
          non-existing-day-cell-el)
       )
      
      (is
        (= (.getAttribute
             existing-day-cell-el
             "points")
           "407.8571428571429,16 533.1428571428571,16 533.1428571428571,9 407.8571428571429,9")
       )
      
     )
    
   ))


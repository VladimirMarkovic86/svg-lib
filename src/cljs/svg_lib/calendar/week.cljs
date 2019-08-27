(ns svg-lib.calendar.week
  (:require [ajax-lib.core :refer [sjax get-response]]
            [common-middle.request-urls :as rurls]
            [common-middle.collection-names :refer [item-cname]]
            [common-middle.session :as cms]
            [js-lib.core :as md]
            [htmlcss-lib.core :refer [gen svg polygon text line]]
            [cljs.reader :as reader]
            [svg-lib.calendar.core :as cc]))

(defn display-month-name
  "Displays month name"
  []
  (let [month-date (js/Date.
                     cc/displayed-date)
        void (.setHours
               month-date
               12)
        void (case (.getDay
                     month-date)
               1 nil
               2 (.setDate
                   month-date
                   (- (.getDate
                        month-date)
                      1))
               3 (.setDate
                   month-date
                   (- (.getDate
                        month-date)
                      2))
               4 (.setDate
                   month-date
                   (- (.getDate
                        month-date)
                      3))
               5 (.setDate
                   month-date
                   (- (.getDate
                        month-date)
                      4))
               6 (.setDate
                   month-date
                   (- (.getDate
                        month-date)
                      5))
               0 (.setDate
                   month-date
                   (- (.getDate
                        month-date)
                      6))
               nil)
        month-num (.getMonth
                    month-date)
        month-name (cc/get-month-name
                     month-num)
        month-name-a (atom
                       month-name)
        is-month-appended-a (atom false)
        void (dotimes [i 6]
               (.setDate
                 month-date
                 (+ (.getDate
                      month-date)
                    1))
               (when (and (not
                            @is-month-appended-a)
                          (not= month-num
                                (.getMonth
                                  month-date))
                      )
                 (swap!
                   month-name-a
                   str
                   " - "
                   (cc/get-month-name
                     (.getMonth
                       month-date))
                  )
                 (reset!
                   is-month-appended-a
                   true))
              )
        month-name-div (md/query-selector
                         ".month-name")]
    (md/set-inner-html
      month-name-div
      (str
        "<h2>"
        @month-name-a
        " "
        (.getFullYear
          month-date)
        "</h2>"))
   ))

(defn create-week-day-cell
  "Creates week day cell"
  [day-date
   ax
   ay
   bx
   by
   cx
   cy
   dx
   dy
   & [view-type]]
  (when (and day-date
             (instance?
               js/Date
               day-date))
    (let [date-id (cc/format-date-id
                    day-date)
          date (.getDate
                 day-date)
          day-date-clone (js/Date.
                           day-date)
          ax (inc
               ax)
          ay (inc
               ay)
          bx (dec
               bx)
          by (inc
               by)
          cx (inc
               cx)
          cy (dec
               cy)
          dx (dec
               dx)
          dy (dec
               dy)
          points (str
                   ax "," ay " "
                   bx "," by " "
                   dx "," dy " "
                   cx "," cy)
          day-cell-el (polygon
                        nil
                        {:class "day-cell"
                         :points points
                         :date date-id}
                        {:ondblclick {:evt-fn cc/insert-edit-item-fn
                                      :evt-p {:item {:start-date (js/Date.
                                                                   day-date)
                                                     :end-date (js/Date.
                                                                 day-date)}}
                                      }})
          day-cell-num-class (if (= date-id
                                    (cc/format-date-id
                                      (js/Date.))
                                  )
                               (str
                                 "day-number current-date")
                               "day-number")
          axd (+ (/ (- bx
                       ax)
                    2)
                 ax)
          axd (if (< date
                     10)
                (- axd
                   5)
                (- axd
                   10))
          element-events (when (not= view-type
                                     "day")
                           {:onclick {:evt-fn @cc/open-day-view-a-fn
                                      :evt-p {:day-date day-date-clone}}
                            })
          day-cell-num (text
                         date
                         {:class day-cell-num-class
                          :x axd
                          :y (- ay
                                6)}
                         element-events)
          day-cell-height (- cy
                             ay)
          day-cell-width (- bx
                            ax)
          date-and-day-offset 50
          two-hours-height (/ day-cell-height
                              12)
          day-cell-hours-els-a (atom [])]
      (dotimes [i 12]
        (let [hour-line-el (line
                             nil
                             {:class "hour-line"
                              :x1 ax
                              :x2 (+ ax
                                     day-cell-width)
                              :y1 (+ (* two-hours-height
                                        i)
                                     date-and-day-offset)
                              :y2 (+ (* two-hours-height
                                        i)
                                     date-and-day-offset)})
              hour-value (* i
                            2)
              hour-value (if (< hour-value
                                10)
                           (str
                             "0"
                             hour-value
                             ":00")
                           (str
                             hour-value
                             ":00"))
              hour-text-el (text
                             hour-value
                             {:class "hour-text"
                              :x ax
                              :y (+ (* two-hours-height
                                        i)
                                    date-and-day-offset
                                    12)})]
          (swap!
            day-cell-hours-els-a
            conj
            {:line hour-line-el
             :text hour-text-el}))
       )
      [day-cell-el
       day-cell-num
       @day-cell-hours-els-a]))
 )

(defn create-item
  "Creates item"
  [item
   axp
   ayp
   bxp
   byp
   cxp
   cyp
   dxp
   dyp
   view-type]
  (let [margin-offset 1
        time-numbers-offset 30
        axi (+ axp
               margin-offset
               time-numbers-offset)
        ayi (+ ayp
               margin-offset)
        bxi (- bxp
               margin-offset)
        byi (+ byp
               margin-offset)
        cxi (+ cxp
               margin-offset
               time-numbers-offset)
        cyi (- cyp
               margin-offset)
        dxi (- dxp
               margin-offset)
        dyi (- dyp
               margin-offset)
        points (str
                 axi "," ayi " "
                 bxi "," byi " "
                 dxi "," dyi " "
                 cxi "," cyi)]
    (gen
      (polygon
        nil
        {:class "item"
         :points points}
        {:onpointerenter {:evt-fn cc/append-item-details
                          :evt-p {:item item
                                  :coordinates [axp
                                                ayp
                                                bxp
                                                byp
                                                cxp
                                                cyp
                                                dxp
                                                dyp]
                                  :view-type view-type}}
         :onpointerleave {:evt-fn cc/remove-item-details}
         :onclick {:evt-fn cc/insert-edit-item-fn
                   :evt-p {:item item}}
         }))
   ))

(defn add-item
  "Adds item in week"
  [item
   view-type]
  (let [{start-datetime :start-date
         end-datetime :end-date} item
        start-date-id (cc/format-date-id
                        start-datetime)]
    (when-let [date-polygon (md/query-selector
                              (str
                                "polygon[date='"
                                start-date-id
                                "']"))]
      (let [points (.split
                     (.getAttribute
                       date-polygon
                       "points")
                     " ")
            [ax
             ay] (.split
                   (aget
                     points
                     0)
                   ",")
            [bx
             by] (.split
                   (aget
                     points
                     1)
                   ",")
            [cx
             cy] (.split
                   (aget
                     points
                     3)
                   ",")
            [dx
             dy] (.split
                   (aget
                     points
                     2)
                   ",")
            ax (reader/read-string
                 ax)
            ay (reader/read-string
                 ay)
            bx (reader/read-string
                 bx)
            by (reader/read-string
                 by)
            cx (reader/read-string
                 cx)
            cy (reader/read-string
                 cy)
            dx (reader/read-string
                 dx)
            dy (reader/read-string
                 dy)
            ax (+ ax
                  3)
            ay (+ ay
                  3)
            bx (- bx
                  3)
            by (+ by
                  3)
            cx (+ cx
                  3)
            cy (- cy
                  3)
            dx (- dx
                  3)
            dy (- dy
                  3)
            item-width (- bx
                          ax)
            entire-day-height (- cy
                                 ay)
            entire-day-hours (* 1000
                                60
                                60
                                24)
            duration-height-ratio (/ entire-day-height
                                     entire-day-hours)
            item-duration (- (.getTime
                               end-datetime)
                             (.getTime
                               start-datetime))
            item-height (* duration-height-ratio
                           item-duration)
            day-start-time (js/Date.
                             start-datetime)
            void (.setMilliseconds
                   day-start-time
                   0)
            void (.setSeconds
                   day-start-time
                   0)
            void (.setMinutes
                   day-start-time
                   0)
            void (.setHours
                   day-start-time
                   0)
            item-start-time-offset (- (.getTime
                                        start-datetime)
                                      (.getTime
                                        day-start-time))
            item-start-height-offset (* item-start-time-offset
                                        duration-height-ratio)
            axi ax
            ayi (+ ay
                   item-start-height-offset)
            bxi (+ ax
                   item-width)
            byi (+ ay
                   item-start-height-offset)
            cxi ax
            cyi (+ ay
                   item-height
                   item-start-height-offset)
            dxi (+ ax
                   item-width)
            dyi (+ ay
                   item-height
                   item-start-height-offset)
            item-el (create-item
                      item
                      axi
                      ayi
                      bxi
                      byi
                      cxi
                      cyi
                      dxi
                      dyi
                      view-type)
            svg-el (md/query-selector
                     ".calendar-container svg")]
        (md/append-element
          svg-el
          item-el))
     ))
 )

(defn get-week-start-date
  "Returns date at the beginning of week"
  [date]
  (let [week-start-date (js/Date.
                          date)]
    (case (.getDay
            week-start-date)
      1 nil
      2 (.setDate
          week-start-date
          (- (.getDate
               week-start-date)
             1))
      3 (.setDate
          week-start-date
          (- (.getDate
               week-start-date)
             2))
      4 (.setDate
          week-start-date
          (- (.getDate
               week-start-date)
             3))
      5 (.setDate
          week-start-date
          (- (.getDate
               week-start-date)
             4))
      6 (.setDate
          week-start-date
          (- (.getDate
               week-start-date)
             5))
      0 (.setDate
          week-start-date
          (- (.getDate
               week-start-date)
             6))
      nil)
    (cc/set-time-at-day-beginning
      week-start-date)
    week-start-date))

(defn get-week-end-date
  "Returns date at the ending of week"
  [date]
  (let [week-end-date (js/Date.
                        date)]
    (case (.getDay
            week-end-date)
      1 (.setDate
          week-end-date
          (+ (.getDate
               week-end-date)
             6))
      2 (.setDate
          week-end-date
          (+ (.getDate
               week-end-date)
             5))
      3 (.setDate
          week-end-date
          (+ (.getDate
               week-end-date)
             4))
      4 (.setDate
          week-end-date
          (+ (.getDate
               week-end-date)
             3))
      5 (.setDate
          week-end-date
          (+ (.getDate
               week-end-date)
             2))
      6 (.setDate
          week-end-date
          (+ (.getDate
               week-end-date)
             1))
      0 nil
      nil)
    (cc/set-time-at-day-ending
      week-end-date)
    week-end-date))

(defn get-db-items-by-week-fn
  "Gets items from database for particular month"
  [db-date]
  (let [week-start-date (get-week-start-date
                          db-date)
        week-end-date (get-week-end-date
                        db-date)
        xhr (sjax
              {:url rurls/get-items-url
               :entity {:entity-type item-cname
                        :entity-filter {:$and
                                         [{:$or
                                            [{:$and
                                               [{:start-date {:$gte week-start-date}}
                                                {:start-date {:$lte week-end-date}}]
                                              }
                                             {:$and [{:end-date {:$gte week-start-date}}
                                                     {:end-date {:$lte week-end-date}}]
                                              }
                                             {:$and [{:start-date {:$lte week-start-date}}
                                                     {:end-date {:$gte week-end-date}}]
                                              }]}
                                          {:username (:username @cms/logged-in-user)}]}
                        :projection [:_id
                                     :name
                                     :type
                                     :description
                                     :start-date
                                     :end-date]
                        :projection-include true
                        :qsort {:start-date 1}
                        :pagination false
                        :username (:username @cms/logged-in-user)}})
        response (get-response
                   xhr)
        items (:data response)
        itr-date (js/Date.
                   week-start-date)
        items-by-dates-a (atom [])]
    (while (< itr-date
              week-end-date)
      (let [items-by-date-a (atom [])]
        (doseq [item items]
          (let [start-date (:start-date item)
                predicate-start-date (js/Date.
                                       start-date)
                end-date (:end-date item)
                predicate-end-date (js/Date.
                                     end-date)]
            (cc/set-time-at-day-beginning
              predicate-start-date)
            (cc/set-time-at-day-beginning
              predicate-end-date)
            (when (and (<= predicate-start-date
                           itr-date)
                       (<= itr-date
                           predicate-end-date))
              (swap!
                items-by-date-a
                conj
                (assoc
                  item
                  :start-date (if (= (cc/format-date-id
                                       start-date)
                                     (cc/format-date-id
                                       itr-date))
                                (js/Date.
                                  start-date)
                                (let [itr-date-s (js/Date.
                                                   itr-date)]
                                  (cc/set-time-at-day-beginning
                                    itr-date-s)
                                  itr-date-s))
                  :end-date (if (= (cc/format-date-id
                                     end-date)
                                   (cc/format-date-id
                                     itr-date))
                              (js/Date.
                                end-date)
                              (let [itr-date-e (js/Date.
                                                 itr-date)]
                                (cc/set-time-at-day-ending
                                  itr-date-e)
                                itr-date-e))
                 ))
             ))
         )
        (swap!
          items-by-dates-a
          conj
          @items-by-date-a))
      (.setDate
        itr-date
        (inc
          (.getDate
            itr-date))
       ))
    @items-by-dates-a))

(defn add-items-for-selected-week
  "Adds items in week"
  []
  (let [items-by-dates (get-db-items-by-week-fn
                         cc/displayed-date)]
    (doseq [items-by-date items-by-dates]
      (doseq [item-by-date items-by-date]
        (add-item
          item-by-date
          "week"))
     ))
 )

(defn get-hours-minutes-and-seconds
  "Returns string representing hours, minutes and seconds"
  [date-param]
  (when (and date-param
             (instance?
               js/Date
               date-param))
    (let [time-s (cc/get-hours-and-minutes
                   date-param)
          time-s (str
                   time-s
                   (if (< (.getSeconds
                            date-param)
                          10)
                     ":0"
                     ":")
                   (.getSeconds
                     date-param))]
      time-s))
 )

(defn display-current-time-line
  "Displays horizontal line that represents current time line"
  [view-type]
  (when-let [current-day-cell (md/query-selector
                                (str
                                  ".calendar-container svg[view-type='"
                                  view-type
                                  "']"
                                  " polygon[date='"
                                  (cc/format-date-id
                                    (js/Date.))
                                  "']"))]
    (let [current-datetime (js/Date.)
          points (.split
                   (.getAttribute
                     current-day-cell
                     "points")
                   " ")
          [ax
           ay] (.split
                 (aget
                   points
                   0)
                 ",")
          [bx
           by] (.split
                 (aget
                   points
                   1)
                 ",")
          [cx
           cy] (.split
                 (aget
                   points
                   3)
                 ",")
          [dx
           dy] (.split
                 (aget
                   points
                   2)
                 ",")
          ax (reader/read-string
               ax)
          ay (reader/read-string
               ay)
          bx (reader/read-string
               bx)
          by (reader/read-string
               by)
          cx (reader/read-string
               cx)
          cy (reader/read-string
               cy)
          dx (reader/read-string
               dx)
          dy (reader/read-string
               dy)
          day-cell-height (- cy
                             ay)
          day-duration (* 1000
                          60
                          60
                          24)
          day-cell-height-duration-ratio (/ day-cell-height
                                            day-duration)
          day-start (js/Date.
                      current-datetime)
          void (.setMilliseconds
                 day-start
                 0)
          void (.setSeconds
                 day-start
                 0)
          void (.setMinutes
                 day-start
                 0)
          void (.setHours
                 day-start
                 0)
          past-time-of-current-day-duration (- (.getTime
                                                 current-datetime)
                                               (.getTime
                                                 day-start))
          past-time-of-current-day-height (+ (* day-cell-height-duration-ratio
                                                past-time-of-current-day-duration)
                                             ay)
          current-time-line-el (gen
                                 (line
                                   nil
                                   {:class "current-time-line"
                                    :x1 ax
                                    :x2 bx
                                    :y1 past-time-of-current-day-height
                                    :y2 past-time-of-current-day-height}))
          triangle-points (str
                            (+ ax
                               5) "," past-time-of-current-day-height " "
                            ax "," (- past-time-of-current-day-height
                                      5) " "
                            ax "," (+ past-time-of-current-day-height
                                      5))
          current-time-triangle-el (gen
                                     (polygon
                                       nil
                                       {:class "current-time-triangle"
                                        :points triangle-points}))
          current-time-text-el (gen
                                 (text
                                   (get-hours-minutes-and-seconds
                                     current-datetime)
                                   {:class "current-time-text"
                                    :x (- bx
                                          40)
                                    :y (- past-time-of-current-day-height
                                          5)})
                                )
          svg-el (md/query-selector
                   ".calendar-container svg")]
      (md/remove-element
        ".calendar-container svg .current-time-line")
      (md/remove-element
        ".calendar-container svg .current-time-triangle")
      (md/remove-element
        ".calendar-container svg .current-time-text")
      (md/append-element
        svg-el
        current-time-line-el)
      (md/append-element
        svg-el
        current-time-triangle-el)
      (md/append-element
        svg-el
        current-time-text-el)
      (md/timeout
        #(display-current-time-line
           view-type)
        1000))
   ))

(defn draw-week-by-date
  "Draws week by date"
  [& [date-param]]
  (@cc/refresh-onresize-event-a-fn
    "week")
  (if-let[calendar-container-el (md/query-selector
                                  ".calendar-container")]
    (let [date-param (if (and date-param
                              (instance?
                                js/Date
                                date-param))
                       (do
                         (.setTime
                           cc/displayed-date
                           (.getTime
                             date-param))
                         (js/Date.
                           date-param))
                       (js/Date.
                         cc/displayed-date))
          void (display-month-name)
          void (md/set-inner-html
                 calendar-container-el
                 "")
          width (aget
                  calendar-container-el
                  "clientWidth")
          height (aget
                   calendar-container-el
                   "clientHeight")
          day-names-row-offset 25
          date-nums-row-offset 25
          date-and-day-offset (+ day-names-row-offset
                                 date-nums-row-offset)
          day-cell-width (/ width
                            7)
          day-cell-height (- height
                             date-and-day-offset)
          week-date (js/Date.
                      date-param)
          void (case (.getDay
                       week-date)
                 1 (.setDate
                     week-date
                     (- (.getDate
                          week-date)
                        1))
                 2 (.setDate
                     week-date
                     (- (.getDate
                          week-date)
                        2))
                 3 (.setDate
                     week-date
                     (- (.getDate
                          week-date)
                        3))
                 4 (.setDate
                     week-date
                     (- (.getDate
                          week-date)
                        4))
                 5 (.setDate
                     week-date
                     (- (.getDate
                          week-date)
                        5))
                 6 (.setDate
                     week-date
                     (- (.getDate
                          week-date)
                        6))
                 0 (.setDate
                     week-date
                     (- (.getDate
                          week-date)
                        7))
                 nil)
          svg-content-a (atom [])]
      (dotimes [i 7]
        (.setDate
          week-date
          (+ (.getDate
               week-date)
             1))
        (let [ax (* day-cell-width
                    i)
              ay date-and-day-offset
              bx (+ (* day-cell-width
                       i)
                    day-cell-width)
              by date-and-day-offset
              cx (* day-cell-width
                    i)
              cy (+ date-and-day-offset
                    day-cell-height)
              dx (+ (* day-cell-width
                       i)
                    day-cell-width)
              dy (+ date-and-day-offset
                    day-cell-height)
              [day-cell-el
               day-cell-num
               day-cell-hours-els] (create-week-day-cell
                                     week-date
                                     ax
                                     ay
                                     bx
                                     by
                                     cx
                                     cy
                                     dx
                                     dy)]
          (swap!
            svg-content-a
            conj
            day-cell-el
            day-cell-num)
          (doseq [{hour-line-el :line
                   hour-text-el :text} day-cell-hours-els]
            (swap!
              svg-content-a
              conj
              hour-line-el
              hour-text-el))
         ))
      (md/append-element
        calendar-container-el
        (gen
          (svg
            @svg-content-a
            {:view-type "week"
             :width (str
                      width
                      "px")
             :height (str
                       height
                       "px")}))
       )
      (cc/display-day-names
        day-cell-width
        width
        0)
      (add-items-for-selected-week)
      (display-current-time-line
        "week"))
    (let [body-el (md/query-selector
                    "body")]
      (md/remove-event
        body-el
        "onresize"
        draw-week-by-date))
   ))

(defn switch-to-previous-week
  "Switches display to previous week"
  []
  (.setDate
    cc/displayed-date
    (- (.getDate
         cc/displayed-date)
       7))
  (display-month-name)
  (draw-week-by-date
    cc/displayed-date))

(defn switch-to-next-week
  "Switches display to next week"
  []
  (.setDate
    cc/displayed-date
    (+ (.getDate
         cc/displayed-date)
       7))
  (display-month-name)
  (draw-week-by-date
    cc/displayed-date))

(defn open-week-view
  "Opens week view"
  [evt-p
   element
   event]
  (let [{last-week-day :last-week-day
         month-week-number :month-week-number
         week-number :week-number} evt-p]
    (.setDate
      last-week-day
      (+ (.getDate
           last-week-day)
         (* month-week-number
            7))
     )
    (draw-week-by-date
      last-week-day))
 )

(reset!
  cc/draw-week-by-date-a-fn
  draw-week-by-date)

(reset!
  cc/open-week-view-a-fn
  open-week-view)

(reset!
  cc/switch-to-previous-week-a-fn
  switch-to-previous-week)

(reset!
  cc/switch-to-next-week-a-fn
  switch-to-next-week)


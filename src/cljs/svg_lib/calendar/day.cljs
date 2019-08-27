(ns svg-lib.calendar.day
  (:require [ajax-lib.core :refer [sjax get-response]]
            [common-middle.request-urls :as rurls]
            [common-middle.collection-names :refer [item-cname]]
            [common-middle.session :as cms]
            [js-lib.core :as md]
            [htmlcss-lib.core :refer [gen svg text line]]
            [svg-lib.calendar.core :as cc]
            [svg-lib.calendar.week :as cw]))

(def draw-day-by-date-a-fn
     (atom (fn [] ))
 )

(defn get-db-items-by-day-fn
  "Gets items from database for particular month"
  [db-date]
  (let [day-start-time (js/Date.
                         db-date)
        void (cc/set-time-at-day-beginning
               day-start-time)
        day-end-time (js/Date.
                       db-date)
        void (cc/set-time-at-day-ending
               day-end-time)
        xhr (sjax
              {:url rurls/get-items-url
               :entity {:entity-type item-cname
                        :entity-filter {:$and
                                         [{:$or
                                            [{:$and
                                               [{:start-date {:$gte day-start-time}}
                                                {:start-date {:$lte day-end-time}}]
                                              }
                                             {:$and [{:end-date {:$gte day-start-time}}
                                                     {:end-date {:$lte day-end-time}}]
                                              }
                                             {:$and [{:start-date {:$lte day-start-time}}
                                                     {:end-date {:$gte day-end-time}}]
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
                   day-start-time)]
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
      [@items-by-date-a]))
 )

(defn add-items-for-selected-day
  "Adds items in week"
  []
  (let [items-by-dates (get-db-items-by-day-fn
                         cc/displayed-date)]
    (doseq [items-by-date items-by-dates]
      (doseq [item-by-date items-by-date]
        (cw/add-item
          item-by-date
          "day"))
     ))
 )

(defn refresh-onresize-event-month
  "Refreshes onresize event for month view type"
  []
  (let [body-el (md/query-selector
                  "body")]
    (when (md/contains-event-fn?
            body-el
            "onresize"
            cw/draw-week-by-date)
      (md/remove-event
        body-el
        "onresize"
        cw/draw-week-by-date))
    (when (md/contains-event-fn?
            body-el
            "onresize"
            @draw-day-by-date-a-fn)
      (md/remove-event
        body-el
        "onresize"
        @draw-day-by-date-a-fn))
    (when-not (md/contains-event-fn?
                body-el
                "onresize"
                cc/draw-by-date)
      (md/event
        body-el
        "onresize"
        cc/draw-by-date))
   ))

(defn refresh-onresize-event-week
  "Refreshes onresize event for week view type"
  []
  (let [body-el (md/query-selector
                  "body")]
    (when (md/contains-event-fn?
            body-el
            "onresize"
            cc/draw-by-date)
      (md/remove-event
        body-el
        "onresize"
        cc/draw-by-date))
    (when (md/contains-event-fn?
            body-el
            "onresize"
            @draw-day-by-date-a-fn)
      (md/remove-event
        body-el
        "onresize"
        @draw-day-by-date-a-fn))
    (when-not (md/contains-event-fn?
                body-el
                "onresize"
                cw/draw-week-by-date)
      (md/event
        body-el
        "onresize"
        cw/draw-week-by-date))
   ))

(defn refresh-onresize-event-day
  "Refreshes onresize event for day view type"
  []
  (let [body-el (md/query-selector
                  "body")]
    (when (md/contains-event-fn?
            body-el
            "onresize"
            cc/draw-by-date)
      (md/remove-event
        body-el
        "onresize"
        cc/draw-by-date))
    (when (md/contains-event-fn?
            body-el
            "onresize"
            cw/draw-week-by-date)
      (md/remove-event
        body-el
        "onresize"
        cw/draw-week-by-date))
    (when-not (md/contains-event-fn?
                body-el
                "onresize"
                @draw-day-by-date-a-fn)
      (md/event
        body-el
        "onresize"
        @draw-day-by-date-a-fn))
   ))

(defn refresh-onresize-event
  "Refreshes onresize event"
  [view-type]
  (case view-type
    "month" (refresh-onresize-event-month)
    "week" (refresh-onresize-event-week)
    "day" (refresh-onresize-event-day)
    nil))

(defn draw-day-by-date
  "Draws week by date"
  [& [date-param]]
  (refresh-onresize-event
    "day")
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
          void (cc/display-month-name)
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
                            1)
          day-cell-height (- height
                             date-and-day-offset)
          day-date (js/Date.
                     date-param)
          svg-content-a (atom [])]
      (let [ax (* day-cell-width
                  0)
            ay date-and-day-offset
            bx (+ (* day-cell-width
                     0)
                  day-cell-width)
            by date-and-day-offset
            cx (* day-cell-width
                  0)
            cy (+ date-and-day-offset
                  day-cell-height)
            dx (+ (* day-cell-width
                     0)
                  day-cell-width)
            dy (+ date-and-day-offset
                  day-cell-height)
            [day-cell-el
             day-cell-num
             day-cell-hours-els] (cw/create-week-day-cell
                                   day-date
                                   ax
                                   ay
                                   bx
                                   by
                                   cx
                                   cy
                                   dx
                                   dy
                                   "day")]
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
       )
      (md/append-element
        calendar-container-el
        (gen
          (svg
            @svg-content-a
            {:view-type "day"
             :width (str
                      width
                      "px")
             :height (str
                       height
                       "px")}))
       )
      (cc/display-day-name
        day-cell-width
        width
        day-date)
      (add-items-for-selected-day)
      (cw/display-current-time-line
        "day"))
    (let [body-el (md/query-selector
                    "body")]
      (md/remove-event
        body-el
        "onresize"
        @draw-day-by-date-a-fn))
   ))

(defn switch-to-previous-day
  "Switches display to previous day"
  []
  (.setDate
    cc/displayed-date
    (- (.getDate
         cc/displayed-date)
       1))
  (cc/display-month-name)
  (draw-day-by-date
    cc/displayed-date))

(defn switch-to-next-day
  "Switches display to next day"
  []
  (.setDate
    cc/displayed-date
    (+ (.getDate
         cc/displayed-date)
       1))
  (cc/display-month-name)
  (draw-day-by-date
    cc/displayed-date))

(defn open-day-view
  "Opens day view"
  [evt-p
   element
   event]
  (let [{day-date :day-date} evt-p]
    (draw-day-by-date
      day-date))
 )

(reset!
  cc/draw-day-by-date-a-fn
  draw-day-by-date)

(reset!
  cc/open-day-view-a-fn
  open-day-view)

(reset!
  cc/switch-to-previous-day-a-fn
  switch-to-previous-day)

(reset!
  cc/switch-to-next-day-a-fn
  switch-to-next-day)

(reset!
  draw-day-by-date-a-fn
  draw-day-by-date)

(reset!
  cc/refresh-onresize-event-a-fn
  refresh-onresize-event)


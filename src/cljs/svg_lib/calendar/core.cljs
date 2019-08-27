(ns svg-lib.calendar.core
  (:require [ajax-lib.core :refer [sjax get-response]]
            [common-middle.request-urls :as rurls]
            [common-middle.collection-names :refer [item-cname]]
            [common-middle.session :as cms]
            [js-lib.core :as md]
            [validator-lib.core :refer [validate-field validate-input]]
            [htmlcss-lib.core :refer [gen form div fieldset label input span textarea
                                      text polygon svg]]
            [cljs.reader :as reader]
            [language-lib.core :refer [get-label]]
            [framework-lib.core :as frm]))

(def displayed-date
     (js/Date.))

(def draw-by-date-a-fn
     (atom (fn [] ))
 )

(def draw-week-by-date-a-fn
     (atom (fn [] ))
 )

(def open-week-view-a-fn
     (atom (fn [] ))
 )

(def draw-day-by-date-a-fn
     (atom (fn [] ))
 )

(def open-day-view-a-fn
     (atom (fn [] ))
 )

(def switch-to-previous-week-a-fn
     (atom (fn [] ))
 )

(def switch-to-previous-day-a-fn
     (atom (fn [] ))
 )

(def switch-to-next-week-a-fn
     (atom (fn [] ))
 )

(def switch-to-next-day-a-fn
     (atom (fn [] ))
 )

(def refresh-onresize-event-a-fn
     (atom (fn [] ))
 )

(defn format-date-id
  "Formats date in particular format 'yyyymmdd'"
  [format-date]
  (when (and format-date
             (instance?
               js/Date
               format-date))
    (let [date-id (str
                    (.getFullYear
                      format-date))
          date-id (if (< (.getMonth
                           format-date)
                         9)
                    (str
                      date-id
                      "0")
                    date-id)
          date-id (str
                    date-id
                    (inc
                      (.getMonth
                        format-date))
                   )
          date-id (if (< (.getDate
                           format-date)
                         10)
                    (str
                      date-id
                      "0")
                    date-id)
          date-id (str
                    date-id
                    (.getDate
                      format-date))]
      date-id))
 )

(defn format-datetime-id
  "Formats datetime in particular format 'yyyymmddHHMMssSSS'"
  [format-date]
  (when (and format-date
             (instance?
               js/Date
               format-date))
    (let [datetime-id (format-date-id
                        format-date)
          datetime-id (if (< (.getHours
                               format-date)
                             10)
                        (str
                          datetime-id
                          "0")
                        datetime-id)
          datetime-id (str
                        datetime-id
                        (.getHours
                          format-date))
          datetime-id (if (< (.getMinutes
                               format-date)
                             10)
                        (str
                          datetime-id
                          "0")
                        datetime-id)
          datetime-id (str
                        datetime-id
                        (.getMinutes
                          format-date))
          datetime-id (if (< (.getSeconds
                               format-date)
                             10)
                        (str
                          datetime-id
                          "0")
                        datetime-id)
          datetime-id (str
                        datetime-id
                        (.getSeconds
                          format-date))
          datetime-id (if (< (.getMilliseconds
                               format-date)
                             100)
                        (str
                          datetime-id
                          "0")
                        datetime-id)
          datetime-id (if (< (.getMilliseconds
                               format-date)
                             10)
                        (str
                          datetime-id
                          "0")
                        datetime-id)
          datetime-id (str
                        datetime-id
                        (.getMilliseconds
                          format-date))]
      datetime-id))
 )

(defn get-hours-and-minutes
  "Returns string representing hours and minutes"
  [date-param]
  (when (and date-param
             (instance?
               js/Date
               date-param))
    (let [time-s (if (< (.getHours
                          date-param)
                        10)
                   "0"
                   "")
          time-s (str
                   time-s
                   (.getHours
                     date-param)
                   ":")
          time-s (if (< (.getMinutes
                          date-param)
                        10)
                   (str
                     time-s
                     "0")
                   time-s)
          time-s (str
                   time-s
                   (.getMinutes
                     date-param))]
      time-s))
 )

(defn is-day-in-first-week-fn
  "Checks if day is in first week of month"
  [date-param]
  (when (and date-param
             (instance?
               js/Date
               date-param))
    (let [cloned-date-param (js/Date.
                              date-param)
          void (.setDate
                 cloned-date-param
                 1)
          day-of-first-date (.getDay
                              cloned-date-param)]
      (case day-of-first-date
        1 (< (.getDate
               date-param)
             8)
        2 (< (.getDate
               date-param)
             7)
        3 (< (.getDate
               date-param)
             6)
        4 (< (.getDate
               date-param)
             5)
        5 (< (.getDate
               date-param)
             4)
        6 (< (.getDate
               date-param)
             3)
        0 (< (.getDate
               date-param)
             2)
        false))
   ))

(defn append-item-details
  "Appends item details in svg"
  [evt-p
   element
   event]
  (let [{{item-start-date :start-date
          item-end-date :end-date
          item-name :name
          item-type :type} :item
         [ax
          ay
          bx
          by
          cx
          cy
          dx
          dy] :coordinates
         view-type :view-type} evt-p
        is-day-in-first-week (is-day-in-first-week-fn
                               item-start-date)
        time-numbers-offset 30
        [ax
         ay
         bx
         by
         cx
         cy
         dx
         dy
         is-day-in-first-week] (case view-type
                                 "week" [ax
                                         (+ ay
                                            5)
                                         bx
                                         by
                                         cx
                                         (+ cy
                                            5)
                                         dx
                                         dy
                                         false]
                                 "day" [(+ ax
                                           time-numbers-offset)
                                        (+ ay
                                           5)
                                        (+ bx
                                           time-numbers-offset)
                                        by
                                        (+ cx
                                           time-numbers-offset)
                                        (+ cy
                                           5)
                                        (+ dx
                                           time-numbers-offset)
                                        dy
                                        false]
                                 [ax
                                  ay
                                  bx
                                  by
                                  cx
                                  cy
                                  dx
                                  dy
                                  is-day-in-first-week])
        text-name-el (gen
                       (text
                         (if (< (count
                                  item-name)
                                11)
                           item-name
                           (str
                             (.substring
                               item-name
                               0
                               10)
                             "..."))
                         {:class "item-details-name"
                          :x (+ ax
                                5)
                          :y (if is-day-in-first-week
                               (+ cy
                                  22)
                               (- ay
                                  42))}
                        ))
        text-type-el (gen
                       (text
                         (if (< (count
                                  item-type)
                                15)
                           item-type
                           (str
                             (.substring
                               item-name
                               0
                               14)
                             "..."))
                         {:class "item-details-type"
                          :x (+ ax
                                5)
                          :y (if is-day-in-first-week
                               (+ cy
                                  34)
                               (- ay
                                  30))}
                        ))
        text-time-el (gen
                       (text
                         (str
                           (get-hours-and-minutes
                             item-start-date)
                           " - "
                           (get-hours-and-minutes
                             item-end-date))
                         {:class "item-details-time"
                          :x (+ ax
                                12)
                          :y (if is-day-in-first-week
                               (+ cy
                                  54)
                               (- ay
                                  10))}
                        ))
        text-background-el (gen
                             (polygon
                               nil
                               {:class "item-details-background"
                                :points (str
                                          ax "," (if is-day-in-first-week
                                                   (+ cy
                                                      60)
                                                   (- ay
                                                      60)) " "
                                          (+ ax
                                             100) "," (if is-day-in-first-week
                                                        (+ cy
                                                           60)
                                                        (- ay
                                                           60)) " "
                                          (+ ax
                                             100) "," (if is-day-in-first-week
                                                        (+ cy
                                                           5)
                                                        (- ay
                                                           5)) " "
                                          ax "," (if is-day-in-first-week
                                                   (+ cy
                                                      5)
                                                   (- ay
                                                      5))
                                         )})
                             )
        svg-el (md/query-selector
                 ".calendar-container svg")]
    (md/append-element
      svg-el
      text-background-el)
    (md/append-element
      svg-el
      text-name-el)
    (md/append-element
      svg-el
      text-type-el)
    (md/append-element
      svg-el
      text-time-el))
 )

(defn remove-item-details
  "Removes item details from svg"
  [evt-p
   element
   event]
  (md/remove-element
    ".calendar-container svg .item-details-name")
  (md/remove-element
    ".calendar-container svg .item-details-type")
  (md/remove-element
    ".calendar-container svg .item-details-time")
  (md/remove-element
    ".calendar-container svg .item-details-background"))

(defn timezone-offset
  "Returns timezone offset in hours"
  [date-param]
  (/ (.getTimezoneOffset
       date-param)
     60))

(defn refresh-view
  "Refreshes current view type"
  []
  (let [is-month-view (md/element-exists
                        ".calendar-container svg[view-type='month']")
        is-week-view (md/element-exists
                       ".calendar-container svg[view-type='week']")
        is-day-view (md/element-exists
                      ".calendar-container svg[view-type='day']")]
    (when is-month-view
      (@draw-by-date-a-fn))
    (when is-week-view
      (@draw-week-by-date-a-fn))
    (when is-day-view
      (@draw-day-by-date-a-fn))
   ))

(defn save-item
  "Save item"
  []
  (let [item-id-el (md/query-selector
                       ".popup-content #item-id")
        item-name-el (md/query-selector
                       ".popup-content #name")
        item-type-el (md/query-selector
                       ".popup-content #type")
        start-date-el (md/query-selector
                        ".popup-content #start-date")
        start-time-el (md/query-selector
                        ".popup-content #start-time")
        end-date-el (md/query-selector
                      ".popup-content #end-date")
        end-time-el (md/query-selector
                      ".popup-content #end-time")
        item-description-el (md/query-selector
                              ".popup-content #description")
        is-valid-a (atom true)]
    (validate-field
      item-name-el
      is-valid-a)
    (validate-field
      item-type-el
      is-valid-a)
    (validate-field
      start-date-el
      is-valid-a)
    (validate-field
      start-time-el
      is-valid-a)
    (validate-field
      end-date-el
      is-valid-a)
    (validate-field
      end-time-el
      is-valid-a)
    (validate-field
      item-description-el
      is-valid-a)
    (let [item-name-v (md/get-value
                        item-name-el)
          item-type-v (md/get-value
                        item-type-el)
          start-date-vd (md/get-value-as-date
                          start-date-el)
          start-time-vd (md/get-value-as-date
                          start-time-el)
          end-date-vd (md/get-value-as-date
                        end-date-el)
          end-time-vd (md/get-value-as-date
                        end-time-el)
          item-description-v (md/get-value
                               item-description-el)]
      (when @is-valid-a
        (.setHours
          start-date-vd
          (+ (.getHours
               start-time-vd)
             (timezone-offset
               start-time-vd))
         )
        (.setMinutes
          start-date-vd
          (.getMinutes
            start-time-vd))
        (.setHours
          end-date-vd
          (+ (.getHours
               end-time-vd)
             (timezone-offset
               end-time-vd))
         )
        (.setMinutes
          end-date-vd
          (.getMinutes
            end-time-vd))
        (let [start-after-end-validator-predicate (and start-date-vd
                                                       end-date-vd
                                                       (< end-date-vd
                                                          start-date-vd))
              custom-validator-message (get-label
                                         119)]
          (validate-field
            start-date-el
            is-valid-a
            custom-validator-message
            start-after-end-validator-predicate)
          (validate-field
            start-time-el
            is-valid-a
            custom-validator-message
            start-after-end-validator-predicate)
          (validate-field
            end-date-el
            is-valid-a
            custom-validator-message
            start-after-end-validator-predicate)
          (validate-field
            end-time-el
            is-valid-a
            custom-validator-message
            start-after-end-validator-predicate))
       )
      (when @is-valid-a
        (let [request-body {:start-datetime start-date-vd
                            :end-datetime end-date-vd
                            :username (:username @cms/logged-in-user)}
              request-body (if item-id-el
                             (assoc
                               request-body
                               :_id (md/get-value
                                      item-id-el))
                             request-body)
              xhr (sjax
                    {:url rurls/is-item-datetime-taken-url
                     :entity request-body})
              response (get-response
                         xhr)
              is-already-taken-validator-predicate (:is-already-taken response)
              custom-validator-message (get-label
                                         121)]
          (validate-field
            start-date-el
            is-valid-a
            custom-validator-message
            is-already-taken-validator-predicate)
          (validate-field
            start-time-el
            is-valid-a
            custom-validator-message
            is-already-taken-validator-predicate)
          (validate-field
            end-date-el
            is-valid-a
            custom-validator-message
            is-already-taken-validator-predicate)
          (validate-field
            end-time-el
            is-valid-a
            custom-validator-message
            is-already-taken-validator-predicate))
       )
      (when @is-valid-a
        (let [new-item {:name item-name-v
                        :type item-type-v
                        :start-date start-date-vd
                        :end-date end-date-vd
                        :description item-description-v
                        :username (:username @cms/logged-in-user)}
              request-body {:entity-type item-cname
                            :entity new-item
                            :username (:username @cms/logged-in-user)}
              request-body (if item-id-el
                             (assoc
                               request-body
                               :_id (md/get-value
                                      item-id-el))
                             request-body)
              xhr (sjax
                    {:url (if item-id-el
                            rurls/update-item-url
                            rurls/insert-item-url)
                     :entity request-body})]
          (refresh-view)
          (frm/close-popup))
       ))
   ))

(defn delete-item
  "Deletes item from database"
  [evt-p]
  (let [xhr (sjax
              {:url rurls/delete-item-url
               :entity {:entity-type item-cname
                        :entity-filter {:_id evt-p}
                        :username (:username @cms/logged-in-user)}})]
    (refresh-view)
    (frm/close-popup))
 )

(defn get-item-by-id
  "Returns item retrieved from database by _id"
  [_id]
  (let [xhr (sjax
              {:url rurls/get-item-url
               :entity {:entity-type item-cname
                        :entity-filter {:_id _id}
                        :entity-projection [:_id
                                            :name
                                            :type
                                            :description
                                            :start-date
                                            :end-date]
                        :projection-include true
                        :username (:username @cms/logged-in-user)}})
        response (get-response
                   xhr)
        item (:data response)]
    [(:start-date item)
     (:end-date item)])
 )

(defn create-item-form
  "Generates create item form"
  [evt-p]
  (when evt-p
    (let [{start-date :start-date
           end-date :end-date
           item-name :name
           item-type :type
           item-description :description
           _id :_id} (:item evt-p)
          [start-date
           end-date] (if _id
                       (get-item-by-id
                         _id)
                       [start-date
                        end-date])
          start-date-obj (js/Date.
                           start-date)
          end-date-obj (js/Date.
                         end-date)]
      (.setHours
        start-date-obj
        (- (.getHours
             start-date-obj)
           (timezone-offset
             start-date-obj))
       )
      (.setHours
        end-date-obj
        (- (.getHours
             end-date-obj)
           (timezone-offset
             end-date-obj))
       )
      (when-not _id
        (.setSeconds
          start-date-obj
          0)
        (.setMilliseconds
          start-date-obj
          0)
        (.setSeconds
          end-date-obj
          0)
        (.setMilliseconds
          end-date-obj
          0)
        (.setHours
          end-date-obj
          (inc
            (.getHours
              end-date-obj))
         ))
      [(fieldset
         [(when _id
            (div
              (input
                nil
                {:id "item-id"
                 :type "hidden"
                 :value _id}))
           )
          (when _id
            (div))
          (div
            (label
              [(get-label
                 122)
               (input
                 nil
                 {:name "name"
                  :type "text"
                  :placeholder (get-label
                                 122)
                  :required true
                  :title (get-label
                           122)
                  :id "name"
                  :value (str
                           item-name)}
                 {:onchange {:evt-fn validate-input}})
               (span)])
           )
          (div
            (label
              [(get-label
                 116)
               (input
                 nil
                 {:name "type"
                  :type "text"
                  :placeholder (get-label
                                 116)
                  :required true
                  :title (get-label
                           116)
                  :id "type"
                  :value (str
                           item-type)}
                 {:onchange {:evt-fn validate-input}})
               (span)])
           )
          (div
            (label
              [(get-label
                 113)
               (input
                 nil
                 {:name "start-date"
                  :type "date"
                  :placeholder (get-label
                                 113)
                  :required true
                  :title (get-label
                           113)
                  :id "start-date"}
                 {:onchange {:evt-fn validate-input}}
                 {:valueAsDate start-date-obj})
               (span)])
           )
          (div
            (label
              [(get-label
                 117)
               (input
                 nil
                 {:name "start-time"
                  :type "time"
                  :placeholder (get-label
                                 117)
                  :required true
                  :title (get-label
                           117)
                  :id "start-time"}
                 {:onchange {:evt-fn validate-input}}
                 {:valueAsDate start-date-obj})
               (span)])
           )
          (div
            (label
              [(get-label
                 114)
               (input
                 nil
                 {:name "end-date"
                  :type "date"
                  :placeholder (get-label
                                 114)
                  :required true
                  :title (get-label
                           114)
                  :id "end-date"}
                 {:onchange {:evt-fn validate-input}}
                 {:valueAsDate end-date-obj})
               (span)])
           )
          (div
            (label
              [(get-label
                 118)
               (input
                 nil
                 {:name "end-time"
                  :type "time"
                  :placeholder (get-label
                                 118)
                  :required true
                  :title (get-label
                           118)
                  :id "end-time"}
                 {:onchange {:evt-fn validate-input}}
                 {:valueAsDate end-date-obj})
               (span)])
           )
          (div
            (label
              [(get-label
                 120)
               (textarea
                 (str
                   item-description)
                 {:name "description"
                  :placeholder (get-label
                                 120)
                  :required true
                  :title (get-label
                           120)
                  :id "description"}
                 {:onchange {:evt-fn validate-input}})
               (span)])
           )
          ])
       (div
         [(when _id
            (input
              nil
              {:id "btnDelete"
               :class "btn"
               :type "submit"
               :value (get-label
                        8)}
              {:onclick {:evt-fn delete-item
                         :evt-p _id}}))
          (input
            nil
            {:id "btnCreate"
             :class "btn btn-default"
             :style (when-not _id
                      {:float "right"
                       :margin-right "10px"})
             :type "submit"
             :value (if _id
                      (get-label
                        11)
                      (get-label
                        10))}
            {:onclick {:evt-fn save-item}})
          ])
       ]))
 )

(defn insert-edit-item-fn
  "Insert or edit item in database"
  [evt-p
   element
   event]
  (let [heading (get-label
                  112)
        content (form
                  (create-item-form
                    evt-p)
                  {:class "entity"
                   :onsubmit "return false"
                   :autocomplete "off"
                   :novalidate true})]
    (frm/popup-fn
      {:heading heading
       :content content}))
 )

(defn display-add-button
  "Display add button for day cell"
  [{date-id :date-id
    dx :dx
    dy :dy
    day-date :date}]
  (md/remove-element
    ".calendar-container svg text[class='add-button']")
  (let [add-button-text-el (gen
                             (text
                               "+"
                               {:class "add-button"
                                :id (str
                                      date-id
                                      "_add_btn")
                                :x (- dx
                                      20)
                                :y (- dy
                                      10)}
                               {:onclick {:evt-fn insert-edit-item-fn
                                          :evt-p {:item {:start-date day-date
                                                         :end-date day-date}}
                                          }}))
        svg-el (md/query-selector
                 ".calendar-container svg")]
    (md/append-element
      svg-el
      add-button-text-el))
 )

(defn hide-add-button
  "Hide add button for day cell"
  [{date-id :date-id}
   element
   event]
  (when (and event
             (not= (aget
                     (aget
                       event
                       "explicitOriginalTarget")
                     "data")
                   "+"))
    (md/remove-element
      (str
        ".calendar-container svg text[id='"
        date-id
        "_add_btn']"))
   ))

(defn create-day-cell
  "Creates day cell"
  [day-date
   ax
   ay
   bx
   by
   cx
   cy
   dx
   dy]
  (when (and day-date
             (instance?
               js/Date
               day-date))
    (let [date-id (format-date-id
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
                        {:onmouseenter {:evt-fn display-add-button
                                        :evt-p {:date-id date-id
                                                :dx dx
                                                :dy dy
                                                :date day-date}}
                         :onmouseleave {:evt-fn hide-add-button
                                        :evt-p {:date-id date-id}}
                         })
          day-cell-num-class (if (= date-id
                                    (format-date-id
                                      (js/Date.))
                                  )
                               (str
                                 "day-number current-date")
                               "day-number")
          day-cell-num (text
                         date
                         {:class day-cell-num-class
                          :x (+ cx
                                5)
                          :y (- cy
                                5)}
                         {:onclick {:evt-fn @open-day-view-a-fn
                                    :evt-p {:day-date day-date-clone}}
                          })]
      [day-cell-el
       day-cell-num]))
 )

(defn get-month-name
  "Returns month name"
  [month-num]
  (case month-num
    0 (get-label
        93)
    1 (get-label
        94)
    2 (get-label
        95)
    3 (get-label
        96)
    4 (get-label
        97)
    5 (get-label
        98)
    6 (get-label
        99)
    7 (get-label
        100)
    8 (get-label
        101)
    9 (get-label
        102)
    10 (get-label
         103)
    11 (get-label
         104)
    "Error"))

(defn display-month-name
  "Displays month name"
  []
  (let [month-num (.getMonth
                    displayed-date)
        month-name (get-month-name
                     month-num)
        month-name-div (md/query-selector
                         ".month-name")]
    (md/set-inner-html
      month-name-div
      (str
        "<h2>"
        month-name
        " "
        (.getFullYear
          displayed-date)
        "</h2>"))
   ))

(defn calculate-day-x
  "Calculate day x coordinate"
  [day-cell-width
   day-number
   day-name-width]
  (+ (* day-cell-width
        day-number)
     (/ (- day-cell-width
           day-name-width)
        2)
   ))

(defn day-name-by-number
  "Returns day name by number"
  [day-num]
  (case day-num
    1 (get-label
        105)
    2 (get-label
        106)
    3 (get-label
        107)
    4 (get-label
        108)
    5 (get-label
        109)
    6 (get-label
        110)
    7 (get-label
        111)
    0 (get-label
        111)
    "Unknown"))

(defn display-day-name
  "Displays day name"
  [day-cell-width
   width
   day-date]
  (when (and day-date
             (instance?
               js/Date
               day-date))
    (let [svg-el (md/query-selector
                   ".calendar-container svg")
          background-polygon (polygon
                               nil
                               {:class "day-name-background"
                                :points (str
                                          0
                                          ",0 "
                                          width
                                          ",0 "
                                          width
                                          ",25 "
                                          0
                                          ",25")})
          day-name (text
                     (day-name-by-number
                       (.getDay
                         day-date))
                     {:class "day-name"
                      :y 18
                      :day-number 0})]
      (md/append-element
        svg-el
        (gen
          background-polygon))
      (md/append-element
        svg-el
        (gen
          day-name))
      (let [day-name-el (md/query-selector
                          ".calendar-container svg text[day-number]")
            day-name-width (aget
                             (.getBBox
                               day-name-el)
                             "width")]
        (.setAttribute
          day-name-el
          "x"
          (calculate-day-x
            day-cell-width
            0
            day-name-width))
       ))
   ))

(defn display-day-names
  "Displays day names"
  [day-cell-width
   width
   week-numbers-column-offset]
  (let [svg-el (md/query-selector
                 ".calendar-container svg")
        background-polygon (polygon
                             nil
                             {:class "day-name-background"
                              :points (str
                                        week-numbers-column-offset
                                        ",0 "
                                        (+ width
                                           week-numbers-column-offset)
                                        ",0 "
                                        (+ width
                                           week-numbers-column-offset)
                                        ",25 "
                                        week-numbers-column-offset
                                        ",25")})
        day-names [(text
                     (day-name-by-number
                       1)
                     {:class "day-name"
                      :y 18
                      :day-number 0})
                   (text
                     (day-name-by-number
                       2)
                     {:class "day-name"
                      :y 18
                      :day-number 1})
                   (text
                     (day-name-by-number
                       3)
                     {:class "day-name"
                      :y 18
                      :day-number 2})
                   (text
                     (day-name-by-number
                       4)
                     {:class "day-name"
                      :y 18
                      :day-number 3})
                   (text
                     (day-name-by-number
                       5)
                     {:class "day-name"
                      :y 18
                      :day-number 4})
                   (text
                     (day-name-by-number
                       6)
                     {:class "day-name"
                      :y 18
                      :day-number 5})
                   (text
                     (day-name-by-number
                       7)
                     {:class "day-name"
                      :y 18
                      :day-number 6})]]
    (md/append-element
      svg-el
      (gen
        background-polygon))
    (doseq [day-name day-names]
      (md/append-element
        svg-el
        (gen
          day-name))
     )
    (let [day-names-el (md/query-selector-all
                         ".calendar-container svg text[day-number]")
          itr (atom 0)]
      (doseq [day-name-el day-names-el]
        (let [day-name-width (aget
                               (.getBBox
                                 day-name-el)
                               "width")]
          (.setAttribute
            day-name-el
            "x"
            (+ (calculate-day-x
                 day-cell-width
                 @itr
                 day-name-width)
               week-numbers-column-offset))
          (swap!
            itr
            inc))
       ))
   ))

(defn calculate-number-of-weeks
  "Calculates number of weeks in month"
  [number-of-weeks-date]
  (when (and number-of-weeks-date
             (instance?
               js/Date
               number-of-weeks-date))
    (.setDate
      number-of-weeks-date
      1)
    (let [number-of-weeks 5
          number-of-weeks (if (or (and (= (.getDay
                                            number-of-weeks-date)
                                          0)
                                       (not= (.getMonth
                                               number-of-weeks-date)
                                             1))
                                  (and (= (.getDay
                                            number-of-weeks-date)
                                          6)
                                       (not= (.getMonth
                                               number-of-weeks-date)
                                             1)
                                       (not= (.indexOf
                                               "0,2,4,6,7,9,11"
                                               (str
                                                 (.getMonth
                                                   number-of-weeks-date))
                                              )
                                             -1))
                               )
                            6
                            number-of-weeks)
          number-of-weeks (if (and (= (.getDay
                                        number-of-weeks-date)
                                      1)
                                   (= (.getMonth
                                        number-of-weeks-date)
                                      1)
                                   (not= (mod (.getFullYear
                                                number-of-weeks-date)
                                              4)
                                         0))
                            4
                            number-of-weeks)]
      number-of-weeks))
 )

(defn create-item
  "Creates calendar item"
  [item
   [ax axi]
   [ay ayi]
   [bx bxi]
   [by byi]
   [cx cxi]
   [cy cyi]
   [dx dxi]
   [dy dyi]
   position-x
   position-y]
  (let [{item-start-date :start-date
         item-end-date :end-date
         _id :_id} item
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
                 cx "," cy)]
    (gen
      (polygon
        nil
        {:points points
         :class "item"
         :position-x position-x
         :position-y position-y
         :start-datetime-id (format-datetime-id
                              item-start-date)
         :end-datetime-id (format-datetime-id
                            item-end-date)
         :item-id _id}
        {:onpointerenter {:evt-fn append-item-details
                          :evt-p {:item item
                                  :coordinates [axi
                                                ayi
                                                bxi
                                                byi
                                                cxi
                                                cyi
                                                dxi
                                                dyi]}}
         :onpointerleave {:evt-fn remove-item-details}
         :onclick {:evt-fn insert-edit-item-fn
                   :evt-p {:item item}}
         }
        {:start-date item-start-date
         :end-date item-end-date}))
   ))

(defn add-item
  "Adds item in calendar"
  [item
   rows
   columns
   position-x
   position-y]
  (let [{start-datetime :start-date
         end-datetime :end-date
         _id :_id} item
        start-datetime-id (format-date-id
                            start-datetime)
        end-datetime-id (format-date-id
                          end-datetime)
        date-polygon (md/query-selector
                       (str
                         "polygon[date='"
                         start-datetime-id
                         "']"))]
    (when (and date-polygon
               (md/html?
                 date-polygon))
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
            item-width (/ (- bx
                             ax)
                          columns)
            item-height (/ (- cy
                              ay
                              25)
                           rows)
            axi (+ ax
                   (* position-x
                      item-width))
            ayi (+ ay
                   (* position-y
                      item-height))
            bxi (+ ax
                   (* position-x
                      item-width)
                   item-width)
            byi (+ ay
                   (* position-y
                      item-height))
            cxi (+ ax
                   (* position-x
                      item-width))
            cyi (+ ay
                   (* position-y
                      item-height)
                   item-height)
            dxi (+ ax
                   (* position-x
                      item-width)
                   item-width)
            dyi (+ ay
                   (* position-y
                      item-height)
                   item-height)
            item-el (create-item
                      item
                      [axi ax]
                      [ayi ay]
                      [bxi bx]
                      [byi by]
                      [cxi cx]
                      [cyi cy]
                      [dxi dx]
                      [dyi dy]
                      position-x
                      position-y)
            svg-el (md/query-selector
                     ".calendar-container svg")]
        (md/append-element
          svg-el
          item-el))
     ))
 )

(defn draw-items-list
  "Drwas items list in rows x columns matrix"
  [items
   rows
   columns]
  (dotimes [i rows]
    (dotimes [j columns]
      (let [item-index (+ (* i
                             columns)
                          j)]
        (when (< item-index
                 (count
                   items))
          (add-item
            (get
              items
              item-index)
            rows
            columns
            j
            i))
       ))
   ))

(defn set-time-at-day-beginning
  "Sets time of date object at the beginning of day"
  [date-obj]
  (.setHours
    date-obj
    0)
  (.setMinutes
    date-obj
    0)
  (.setSeconds
    date-obj
    0)
  (.setMilliseconds
    date-obj
    0))

(defn set-time-at-day-ending
  "Sets time of date object at the ending of day"
  [date-obj]
  (.setHours
    date-obj
    23)
  (.setMinutes
    date-obj
    59)
  (.setSeconds
    date-obj
    59)
  (.setMilliseconds
    date-obj
    999))

(defn get-start-date
  "Returns date at beginning of month"
  [date]
  (let [month-start-date (js/Date.
                           date)
        void (.setDate
               month-start-date
               1)]
    (set-time-at-day-beginning
      month-start-date)
    month-start-date))

(defn get-end-date
  "Returns date at end of the month"
  [date]
  (let [month-end-date (js/Date.
                         date)]
    (.setMonth
      month-end-date
      (inc
        (.getMonth
          month-end-date))
     )
    (.setDate
      month-end-date
      1)
    (.setDate
      month-end-date
      (dec
        (.getDate
          month-end-date))
     )
    (set-time-at-day-ending
      month-end-date)
    month-end-date))

(defn get-db-items-by-month-fn
  "Gets items from database for particular month"
  [db-date]
  (let [month-start-date (get-start-date
                           db-date)
        month-end-date (get-end-date
                         db-date)
        xhr (sjax
              {:url rurls/get-items-url
               :entity {:entity-type item-cname
                        :entity-filter {:$and
                                         [{:$or
                                            [{:$and
                                               [{:start-date {:$gte month-start-date}}
                                                {:start-date {:$lte month-end-date}}]
                                              }
                                             {:$and [{:end-date {:$gte month-start-date}}
                                                     {:end-date {:$lte month-end-date}}]
                                              }
                                             {:$and [{:start-date {:$lte month-start-date}}
                                                     {:end-date {:$gte month-end-date}}]
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
                   db-date)
        itr-date-month (.getMonth
                         itr-date)
        items-by-dates-a (atom [])]
    (.setDate
      itr-date
      1)
    (set-time-at-day-beginning
      itr-date)
    (while (= itr-date-month
              (.getMonth
                itr-date))
      (let [items-by-date-a (atom [])]
        (doseq [item items]
          (let [start-date (:start-date item)
                predicate-start-date (js/Date.
                                       start-date)
                end-date (:end-date item)
                predicate-end-date (js/Date.
                                     end-date)]
            (set-time-at-day-beginning
              predicate-start-date)
            (set-time-at-day-beginning
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
                  :start-date (if (= (format-date-id
                                       start-date)
                                     (format-date-id
                                       itr-date))
                                (js/Date.
                                  start-date)
                                (let [itr-date-s (js/Date.
                                                   itr-date)]
                                  (set-time-at-day-beginning
                                    itr-date-s)
                                  itr-date-s))
                  :end-date (if (= (format-date-id
                                     end-date)
                                   (format-date-id
                                     itr-date))
                              (js/Date.
                                end-date)
                              (let [itr-date-e (js/Date.
                                                 itr-date)]
                                (set-time-at-day-ending
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

(defn add-items-for-selected-month
  "Adds items for selected month
   
   items-by-dates structure:
   
   [[{:start-date (js/Date.)
      :end-date (js/Date.)}]
    [{:start-date (js/Date.)
      :end-date (js/Date.)}
     {:start-date (js/Date.)
      :end-date (js/Date.)}]
    [{:start-date (js/Date.)
      :end-date (js/Date.)}
     {:start-date (js/Date.)
      :end-date (js/Date.)}]
    ]"
  []
  (let [items-by-dates (get-db-items-by-month-fn
                         displayed-date)]
    (doseq [items-by-date items-by-dates]
      (let [number-of-items (count
                              items-by-date)]
        (cond
          (= number-of-items
             1)
            (draw-items-list
              items-by-date
              1
              1)
          (= number-of-items
             2)
            (draw-items-list
              items-by-date
              2
              1)
          (or (= number-of-items
                 3)
              (= number-of-items
                 4))
            (draw-items-list
              items-by-date
              2
              2)
          (or (= number-of-items
                 5)
              (= number-of-items
                 6))
            (draw-items-list
              items-by-date
              3
              2)
          (or (= number-of-items
                 7)
              (= number-of-items
                 8)
              (= number-of-items
                 9))
            (draw-items-list
              items-by-date
              3
              3)
          (or (= number-of-items
                 10)
              (= number-of-items
                 11)
              (= number-of-items
                 12))
            (draw-items-list
              items-by-date
              3
              4)
          (or (= number-of-items
                 13)
              (= number-of-items
                 14)
              (= number-of-items
                 15))
            (draw-items-list
              items-by-date
              3
              5)
          (or (= number-of-items
                 16)
              (= number-of-items
                 17)
              (= number-of-items
                 18)
              (< 18
                 number-of-items))
            (draw-items-list
              items-by-date
              3
              6))
       ))
   ))

(defn get-month-week-numbers
  "Returns first week number for particular month in a year"
  [date-param]
  (when (and date-param
             (instance?
               js/Date
               date-param))
    (let [start-date-param (js/Date.
                             date-param)
          itr-date-param (js/Date.
                           date-param)
          week-number (atom 1)]
      (.setDate
        itr-date-param
        1)
      (.setMonth
        itr-date-param
        0)
      (.setDate
        start-date-param
        1)
      (.setDate
        itr-date-param
        (+ (.getDate
             itr-date-param)
           (case (.getDay
                   itr-date-param)
             1 6
             2 5
             3 4
             4 3
             5 2
             6 1
             0 0
             0))
       )
      (while (< itr-date-param
                start-date-param)
        (.setDate
          itr-date-param
          (+ (.getDate
               itr-date-param)
             7))
        (swap!
          week-number
          inc))
      [@week-number
       (js/Date.
         itr-date-param)])
   ))

(defn display-week-numbers
  "Displays week numbers"
  [day-names-row-offset
   height
   day-cell-height
   number-of-weeks]
  (let [week-numbers-background-el (gen
                                     (polygon
                                       nil
                                       {:class "week-number-background"
                                        :points (str
                                                  "0,0 "
                                                  day-names-row-offset
                                                  ",0 "
                                                  day-names-row-offset
                                                  ","
                                                  height
                                                  " 0,"
                                                  height)})
                                    )
        x 2.5
        y (atom
            (+ day-names-row-offset
               (/ day-cell-height
                  2)
               5))
        [first-week-number
         last-week-day] (get-month-week-numbers
                          displayed-date)
        numbers-of-month-weeks (range
                                 first-week-number
                                 (+ first-week-number
                                    number-of-weeks))
        i (atom 0)
        svg-el (md/query-selector
                 ".calendar-container svg")]
    (md/append-element
      svg-el
      week-numbers-background-el)
    (doseq [week-number numbers-of-month-weeks]
      (let [week-text-el (gen
                           (text
                             (str
                               week-number)
                             {:class "week-number-text"
                              :x (if (< week-number
                                        10)
                                   (+ x
                                      5)
                                   x)
                              :y @y}
                             {:onclick {:evt-fn @open-week-view-a-fn
                                        :evt-p {:last-week-day last-week-day
                                                :month-week-number @i
                                                :week-number week-number}}
                              }))]
        (md/append-element
          svg-el
          week-text-el)
        (swap!
          y
          +
          day-cell-height)
        (swap!
          i
          inc))
     ))
 )

(defn draw-by-date
  "Draws calendar by date"
  [& [date-param]]
  (@refresh-onresize-event-a-fn
    "month")
  (if-let[calendar-container-el (md/query-selector
                                  ".calendar-container")]
    (let [date-param (if (and date-param
                              (instance?
                                js/Date
                                date-param))
                       (do
                         (.setTime
                           displayed-date
                           (.getTime
                             date-param))
                         (js/Date.
                           date-param))
                       (js/Date.
                         displayed-date))
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
          number-of-weeks (calculate-number-of-weeks
                            (js/Date.
                              (.getTime
                                date-param))
                           )
          day-names-row-offset 25
          week-numbers-column-offset 25
          width-with-week-offset (- width
                                    week-numbers-column-offset)
          day-cell-width (/ width-with-week-offset
                            7)
          day-cell-height (/ (- height
                                day-names-row-offset)
                             number-of-weeks)
          date (.getDate
                 date-param)
          month (.getMonth
                  date-param)
          week-a (atom 1)
          previous-day-a (atom (dec
                                 (.getDay
                                   date-param))
                          )
          svg-content-a (atom [])]
      (when (= @previous-day-a
               -1)
        (reset!
          previous-day-a
          0))
      (dotimes [i 31]
        (.setDate
          date-param
          (inc
            i))
        (when (= month
                 (.getMonth
                   date-param))
          (when (= @previous-day-a
                   7)
            (swap!
              week-a
              inc))
          (let [day (.getDay
                      date-param)
                day (if (= day
                           0)
                      7
                      day)
                ax (- (* day
                         day-cell-width)
                      day-cell-width
                      (- week-numbers-column-offset))
                ay (- (* @week-a
                         day-cell-height)
                      day-cell-height
                      (- day-names-row-offset))
                bx (+ (* day
                         day-cell-width)
                      week-numbers-column-offset)
                by (- (* @week-a
                         day-cell-height)
                      day-cell-height
                      (- day-names-row-offset))
                cx (- (* day
                         day-cell-width)
                      day-cell-width
                      (- week-numbers-column-offset))
                cy (+ (* @week-a
                         day-cell-height)
                      day-names-row-offset)
                dx (+ (* day
                         day-cell-width)
                      week-numbers-column-offset)
                dy (+ (* @week-a
                         day-cell-height)
                      day-names-row-offset)
                [day-cell-el
                 day-cell-num] (create-day-cell
                                 (js/Date.
                                   date-param)
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
            (reset!
              previous-day-a
              day))
         ))
      (.setMonth
        date-param
        (dec
          (.getMonth
            date-param))
       )
      (md/append-element
        calendar-container-el
        (gen
          (svg
            @svg-content-a
            {:view-type "month"
             :width (str
                      width
                      "px")
             :height (str
                       height
                       "px")}))
       )
      (display-day-names
        day-cell-width
        width-with-week-offset
        week-numbers-column-offset)
      (display-week-numbers
        day-names-row-offset
        height
        day-cell-height
        number-of-weeks)
      (add-items-for-selected-month))
    (let [body-el (md/query-selector
                    "body")]
      (md/remove-event
        body-el
        "onresize"
        draw-by-date))
   ))

(reset!
  draw-by-date-a-fn
  draw-by-date)

(defn switch-to-previous-month
  "Switches display to previous month"
  []
  (.setMonth
    displayed-date
    (dec
      (.getMonth
        displayed-date))
   )
  (draw-by-date
    displayed-date))

(defn switch-to-next-month
  "Switches display to next month"
  []
  (.setMonth
    displayed-date
    (inc
      (.getMonth
        displayed-date))
   )
  (draw-by-date
    displayed-date))

(defn switch-to-previous
  "Switches display to previous month, week or day"
  []
  (let [is-month-view (md/element-exists
                        ".calendar-container svg[view-type='month']")
        is-week-view (md/element-exists
                       ".calendar-container svg[view-type='week']")
        is-day-view (md/element-exists
                      ".calendar-container svg[view-type='day']")]
    (when is-month-view
      (switch-to-previous-month))
    (when is-week-view
      (@switch-to-previous-week-a-fn))
    (when is-day-view
      (@switch-to-previous-day-a-fn))
   ))

(defn switch-to-next
  "Switches display to next month, week or day"
  []
  (let [is-month-view (md/element-exists
                        ".calendar-container svg[view-type='month']")
        is-week-view (md/element-exists
                       ".calendar-container svg[view-type='week']")
        is-day-view (md/element-exists
                      ".calendar-container svg[view-type='day']")]
    (when is-month-view
      (switch-to-next-month))
    (when is-week-view
      (@switch-to-next-week-a-fn))
    (when is-day-view
      (@switch-to-next-day-a-fn))
   ))


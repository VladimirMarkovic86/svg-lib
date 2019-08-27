(ns svg-lib.test-runner
  (:require [svg-lib.chart.core-test-cljs]
            [svg-lib.calendar.core-test-cljs]
            [svg-lib.calendar.week-test-cljs]
            [svg-lib.calendar.day-test-cljs]
            [doo.runner :refer-macros [doo-tests doo-all-tests]]))

(enable-console-print!)

(doo-tests
  'svg-lib.chart.core-test-cljs
  'svg-lib.calendar.core-test-cljs
  'svg-lib.calendar.week-test-cljs
  'svg-lib.calendar.day-test-cljs)


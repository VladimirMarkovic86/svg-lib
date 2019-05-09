(ns svg-lib.test-runner
  (:require [svg-lib.core-test-cljs]
            [doo.runner :refer-macros [doo-tests doo-all-tests]]))

(enable-console-print!)

(doo-tests
  'svg-lib.core-test-cljs)


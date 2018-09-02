(ns me.lomin.sayang.test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [pjstadig.humane-test-output]
            [me.lomin.sayang.core-test]
            [me.lomin.sayang.api-test]))

(doo-tests 'me.lomin.sayang.core-test
           'me.lomin.sayang.api-test)

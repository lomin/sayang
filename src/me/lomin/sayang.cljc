(ns me.lomin.sayang
  (:require [clojure.spec.alpha :as s]
            [me.lomin.sayang.core :as sayang]
            [clojure.string :as string])
  #?(:cljs (:require-macros me.lomin.sayang)))

#?(:cljs (goog-define activate false))

(defonce
  ^{:dynamic true
    :doc     "If true, register a function spec for every function defined with sdefn."}
  *activate*
  (boolean
    (if-let [prop #?(:cljs activate
                     :clj  (System/getProperty "me.lomin.sayang.activate"))]
      (and (not (string/blank? prop))
           (not= "false" prop)))))

(defmacro of [f]
  (s/get-spec (ns-resolve *ns* &env f)))

(defmacro sdefn [& defn-args]
  (if *activate*
    (do
      (s/assert :me.lomin.sayang.specs/defn-args defn-args)
      (list 'do
            (sayang/make-defn-form defn-args)
            (sayang/make-fdef-form defn-args)))
    (sayang/make-defn-form defn-args)))
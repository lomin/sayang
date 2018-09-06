(ns me.lomin.sayang
  (:require [clojure.spec.alpha :as s]
            [me.lomin.sayang.core :as sayang]
            [clojure.string :as string])
  #?(:cljs (:require-macros me.lomin.sayang)))

#?(:clj (defonce
          ^{:dynamic true
            :doc     "If true, register a function spec for every function defined with sdefn."}
          *activate*
          (System/getProperty "me.lomin.sayang.*activate*")))

(defn toggle [b]
  #?(:clj  (alter-var-root #'*activate* (constantly b))
     :cljs (if b (goog-define *activate* true) (goog-define *activate* false))))

(defn activate! []
  (toggle true))

(defmacro deactivate! []
  (binding [sayang/*cljs?* (-> &env :ns some?)]
    (list 'do
          (list 'me.lomin.sayang/toggle false)
          (if sayang/*cljs?*
            (list 'cljs.spec.test.alpha/unstrument)
            (list 'clojure.spec.test.alpha/unstrument)))))

(defn activated? []
  (and *activate*
       (or (boolean? *activate*)
           (and (not (string/blank? *activate*))
                (not= "false" *activate*)))))

(defmacro sdefn [& defn-args]
  (binding [sayang/*cljs?* (-> &env :ns some?)]
    (if (activated?)
      (do
        (s/assert :me.lomin.sayang.specs/defn-args defn-args)
        (list 'do
              (sayang/make-defn-form defn-args)
              (list 'when (list 'me.lomin.sayang/activated?)
                    (sayang/make-fdef-form defn-args))))
      (sayang/make-defn-form defn-args))))


(defmacro of [f]
  (binding [sayang/*cljs?* (-> &env :ns some?)]
    (list (sayang/spec-fn ::sayang/get-spec) (list 'var f))))
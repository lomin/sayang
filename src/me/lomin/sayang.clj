(ns me.lomin.sayang
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as specs]
            [me.lomin.sayang.core :as sayang]))

(defmacro sdefn [& [sym :as defn-args]]
  (let [defn-args-spec (s/conform ::specs/defn-args defn-args)
        specs (update (select-keys (:meta defn-args-spec) [:args :ret :fn])
                      :args
                      (fn [x] (if x x (sayang/arity->spec (:bs defn-args-spec)))))
        ;defn-args* (s/unform ::specs/defn-args (remove-type-defs defn-args-spec))
        defn-args* (sayang/workaround-for-CLJ-2021 defn-args)
        register-fdef (cons 's/fdef (cons sym (mapcat seq specs)))]
    (list 'do
          (cons 'defn defn-args*)
          register-fdef)))
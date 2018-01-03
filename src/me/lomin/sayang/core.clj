(ns me.lomin.sayang.core
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as specs]
            [com.rpl.specter :as specter]))

(s/def ::type (s/or ::spec (s/fspec :args (s/cat :arg any?)
                                    :ret any?)
                    ::fn-call list?
                    ::vector-of-1 #(and (vector? %)
                                        (= 1 (count %)))
                    ::vector vector?))

(s/def ::type-def-form (s/cat :sym any?
                              ::sep #(= % :-)
                              ::type ::type))

(s/def ::specs/binding-form
  (s/or :sym ::specs/local-name
        :seq ::specs/seq-binding-form
        :map ::specs/map-binding-form
        ::type-def ::type-def-form))

(defn vector->tuple-spec [args]
  (cons 'clojure.spec.alpha/tuple args))

(defn get-type [type-def]
  (let [[k v] (::type type-def)]
    (condp = k
      ::vector-of-1 (list 'clojure.spec.alpha/every (first v))
      ::vector (vector->tuple-spec v)
      v)))

(defn args->spec [args]
  (cons 'clojure.spec.alpha/cat
        (let [args* (get-in args [:args :args])]
          (reduce (fn [key-pred-forms [idx [k sym-or-typ-def]]]
                    (into key-pred-forms
                          (if (= k ::type-def)
                            (list (get-type sym-or-typ-def) (or (keyword (:sym sym-or-typ-def)) (keyword (str idx))))
                            (list 'any? (or (keyword sym-or-typ-def) (keyword (str idx)))))))
                  '()
                  (reverse (map-indexed vector args*))))))

(defn count-args [args]
  (count (get-in args [:args :args] '())))

(defn arity->spec [[k bodies-or-args]]
  (if (= k :arity-n)
    (if-let [args (:bodies bodies-or-args)]
      (arity->spec [k args])
      (cons 'clojure.spec.alpha/or
            (mapcat (fn [x] [(keyword (str (count-args x)))
                             (args->spec x)])
                    bodies-or-args)))
    (if (zero? (count-args bodies-or-args))
      'empty?
      (args->spec bodies-or-args))))

(def make-any-walker
  (specter/recursive-path [path]
                          p
                          (specter/cond-path sequential?
                                             (specter/if-path path
                                                              (specter/continue-then-stay specter/ALL p)
                                                              [specter/ALL p])
                                             map?
                                             (specter/if-path path
                                                              (specter/continue-then-stay specter/MAP-VALS p)
                                                              [specter/MAP-VALS p]))))

(comment
  (defn remove-type-defs [m]
    (specter/transform m
                       (make-any-walker [specter/FIRST (specter/pred= ::type-def)])
                       (fn [[_ {sym :sym}]] sym))))

(defn remove-type-defs-with-workaround-for-CLJ-2021 [defn-args]
  (specter/transform (make-any-walker #(and
                                        (vector? %)
                                        (= (count %) 3)
                                        (let [[_ sep _] %]
                                          (= :- sep))))
                     (fn [[k]] k)
                     defn-args))

(defn merge-specs [defn-args-conformed]
  (update (select-keys (:meta defn-args-conformed) [:args :ret :fn])
          :args
          (fn [x] (if x x (arity->spec (:bs defn-args-conformed))))))

(defn make-fdef-form [[sym :as defn-args]]
  (let [defn-args-conformed (s/conform :clojure.core.specs.alpha/defn-args defn-args)
        specs (merge-specs defn-args-conformed)]
    (cons 'clojure.spec.alpha/fdef (cons sym (mapcat seq specs)))))

(defn make-defn-form [defn-args]
  (cons 'defn
        ; (s/unform ::specs/defn-args (remove-type-defs defn-args-spec))
        (remove-type-defs-with-workaround-for-CLJ-2021 defn-args)))

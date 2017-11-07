(ns me.lomin.sayang.core
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as specs]
            [com.rpl.specter :as specter]))

(s/def ::type (s/fspec :args (s/cat :arg any?)
                       :ret any?))

(s/def ::specs/type-def-form (s/cat :sym any?
                                    ::sep any?
                                    ::type ::type))

(s/def ::specs/binding-form
  (s/or :sym ::specs/local-name
        :seq ::specs/seq-binding-form
        :map ::specs/map-binding-form
        ::type-def ::specs/type-def-form))

(def gen-key (comp keyword gensym))

(defn args->spec
  ([args] (args->spec args gen-key))
  ([args gen-key-fn]
   (cons 'clojure.spec.alpha/cat
         (let [args* (get-in args [:args :args])]
           (reduce (fn [key-pred-forms [k sym]]
                     (into key-pred-forms
                           (if (= k ::type-def)
                             (list (::type sym) (or (keyword (:sym sym)) (gen-key-fn)))
                             (list 'any? (or (keyword sym) (gen-key-fn))))))
                   '()
                   (reverse args*))))))

(defn count-args [args]
  (count (get-in args [:args :args] '())))

(defn arity->spec
  ([arity] (arity->spec arity gen-key))
  ([[k bodies-or-args] gen-key-fn]
   (if (= k :arity-n)
     (if-let [args (:bodies bodies-or-args)]
       (arity->spec [k args] gen-key-fn)
       (cons 'clojure.spec.alpha/or
             (mapcat (fn [x] [(keyword (str (count-args x)))
                              (args->spec x gen-key-fn)])
                     bodies-or-args)))
     (if (zero? (count-args bodies-or-args))
       'empty?
       (args->spec bodies-or-args gen-key-fn)))))

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

(defn workaround-for-CLJ-2021 [defn-args]
  (specter/transform (make-any-walker #(and
                                         (vector? %)
                                         (= (count %) 3)
                                         (let [[k sep t] %]
                                           (and (simple-symbol? k)
                                                (= :- sep)
                                                (s/valid? ::type t)))))
                     (fn [[k]] k)
                     defn-args))
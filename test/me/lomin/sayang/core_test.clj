(ns me.lomin.sayang.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as specs]
            [me.lomin.sayang.core :refer :all :as sayang-core]
            [me.lomin.sayang :refer [sdefn] :as sayang]
            [orchestra.spec.test :as spec-test-all])
  (:import (clojure.lang ExceptionInfo)))

(s/check-asserts true)

(deftest complex-fn-test
  (is (= '{:bs   [:arity-1
                  {:args {:args    [[::sayang-core/type-def
                                     {:sym                       x,
                                      ::sayang-core/sep  :-,
                                      ::sayang-core/type [::sayang-core/spec number?]}]
                                    [::sayang-core/type-def
                                     {:sym                       y,
                                      ::sayang-core/sep  :-,
                                      ::sayang-core/type [::sayang-core/spec number?]}]
                                    [::sayang-core/type-def
                                     {:sym                       {:keys [a b c]},
                                      ::sayang-core/sep  :-,
                                      ::sayang-core/type [::sayang-core/spec map?]}]],
                          :varargs {:amp  &,
                                    :form [:seq
                                           {:as    {:as :as, :sym h},
                                            :elems [[::sayang-core/type-def
                                                     {:sym                       d,
                                                      ::sayang-core/sep  :-,
                                                      ::sayang-core/type [::sayang-core/spec number?]}]],
                                            :rest  {:amp  &,
                                                    :form [::sayang-core/type-def
                                                           {:sym                       [e _ & f :as g],
                                                            ::sayang-core/sep  :-,
                                                            ::sayang-core/type [::sayang-core/spec sequential?]}]}}]}},
                   :body [:body [(str x y)]]}],
           :name my-name}
         (s/conform ::specs/defn-args
                    '(my-name
                      [[x :- number?]
                       [y :- number?]
                       [{:keys [a b c]} :- map?]
                       & [[d :- number?] & [[e _ & f :as g] :- sequential?] :as h]]
                      (str x y)))))

  (is (= '(clojure.spec.alpha/cat :x number? :y number? :2 map?)
         (arity->spec '[:arity-1
                        {:args {:args    [[::sayang-core/type-def
                                           {:sym               x
                                            ::sayang-core/sep  :-
                                            ::sayang-core/type [::sayang-core/spec number?]}]
                                          [::sayang-core/type-def
                                           {:sym               y
                                            ::sayang-core/sep  :-
                                            ::sayang-core/type [::sayang-core/spec number?]}]
                                          [::sayang-core/type-def
                                           {:sym               {:keys [a b c]}
                                            ::sayang-core/sep  :-
                                            ::sayang-core/type [::sayang-core/spec map?]}]]
                                :varargs {:amp  &
                                          :form [:seq
                                                 {:as    {:as :as, :sym h}
                                                  :elems [[::sayang-core/type-def
                                                           {:sym               d
                                                            ::sayang-core/sep  :-
                                                            ::sayang-core/type [::sayang-core/spec number?]}]]
                                                  :rest  {:amp  &
                                                          :form [::sayang-core/type-def
                                                                 {:sym               [e _ & f :as g]
                                                                  ::sayang-core/sep  :-
                                                                  ::sayang-core/type [::sayang-core/spec sequential?]}]}}]}}
                         :body [:body [(str x y)]]}]))))

(deftest arity-0-test
  (is (= '{:bs [:arity-1 {:args {}, :body [:body [1]]}], :name my-name}
         (s/conform ::specs/defn-args
                    '(my-name [] 1)))))

(deftest arity-1-test
  (is (= '{:bs [:arity-1 {:args {:args [[:sym x]]}, :body [:body [(+ x 1)]]}], :name fn}
         (s/conform ::specs/defn-args
                    '(fn [x]
                       (+ x 1)))))
  (is (= '{:bs   [:arity-1
                  {:args {:args [[::sayang-core/type-def
                                  {:sym               x
                                   ::sayang-core/sep  :-
                                   ::sayang-core/type [::sayang-core/spec int?]}]
                                 [:sym y]]}
                   :body [:body [(+ x y) (+ y x)]]}]
           :name fn}
         (s/conform ::specs/defn-args
                    '(fn [[x :- int?] y]
                       (+ x y)
                       (+ y x)))))
  (is (= '{:bs   [:arity-n
                  {:bodies [{:args {:args [[::sayang-core/type-def
                                            {:sym               x
                                             ::sayang-core/sep  :-
                                             ::sayang-core/type [::sayang-core/spec int?]}]]}
                             :body [:body [(str x %)]]}]}]
           :name fn}
         (s/conform ::specs/defn-args
                    '(fn
                       ([[x :- int?]]
                        (str x %))))))

  (testing "defn-form"
    (let [form (s/conform ::specs/defn-args '(test [x] (inc x)))]
      (is (= '(clojure.spec.alpha/cat :x any?) (arity->spec (:bs form)))))))

(deftest arity-1->spec-test
  (is (= '(clojure.spec.alpha/cat :x any?)
         (args->spec '{:args {:args [[:sym x]]}
                       :body [:body [(+ x 1)]]})))
  (is (= '(clojure.spec.alpha/cat :y any?)
         (args->spec '{:args {:args [[:sym y]]}
                       :body [:body [(+ y 1)]]})))

  (is (= '(clojure.spec.alpha/cat :x number? :y any?)
         (args->spec '{:args {:args [[::sayang-core/type-def
                                      {:sym               x
                                       ::sayang-core/type [::sayang-core/spec number?]}]
                                     [:sym y]]}
                       :body [:body [(+ x y) (+ y x)]]}))))

(deftest arity-n>spec-test
  (is (= '(clojure.spec.alpha/or :1 (clojure.spec.alpha/cat :x ::sayang-core/number?))
         (arity->spec '[:arity-n
                        [{:args {:args [[::sayang-core/type-def
                                         {:sym               x
                                          ::sayang-core/type [::sayang-core/spec ::sayang-core/number?]}]]}
                          :body [:body [(str x %)]]}]])))

  (is (= 'empty?
         (arity->spec '[:arity-1 {:args {}, :body [:body [1]]}])))

  (is (= '(clojure.spec.alpha/cat :x ::sayang-core/number? :y any?)
         (arity->spec '[:arity-1
                        {:args {:args [[::sayang-core/type-def
                                        {:sym               x
                                         ::sayang-core/type [::sayang-core/spec ::sayang-core/number?]}]
                                       [:sym y]]}
                         :body [:body [(+ x y) (+ y x)]]}])))
  (testing "defn-form"
    (is (= '(clojure.spec.alpha/or :2 (clojure.spec.alpha/cat :0 any? :y any?) :1 (clojure.spec.alpha/cat :x any?))
           (arity->spec '[:arity-n
                          {:bodies
                           [{:args {:args [[:seq {:elems [[:sym x]]}] [:sym y]]}
                             :body [:body [(+ x 1)]]}
                            {:args {:args [[:sym x]]}, :body [:body [(+ x 1)]]}]}])))))

(deftest workaround-for-CLJ-2021-test
  (is (= '(f-name {} ([x y] (str x y)) ([x] (str x)))
         (remove-type-defs-with-workaround-for-CLJ-2021 '(f-name
                                                          {}
                                                          ([[x :- number?]
                                                            [y :- ::test-type]]
                                                           (str x y))
                                                          ([[x :- number?]]
                                                           (str x)))))))

(sdefn getting-started {:ret string?}
       [f
        [x :- int?]]
       (f x))

(deftest getting-started-test
  (is (= "5" (getting-started str 5)))
  (is (= :exception (try (getting-started str "5")
                         (catch ExceptionInfo _ :exception))))
  (is (= :exception (try (getting-started identity 5)
                         (catch ExceptionInfo _ :exception)))))

(sdefn make-magic-string {:ret string?}
       ([[x :- int?]]
        (str x "?"))
       ([[x :- string?] [y :- string?]]
        (str x "?" y)))

(deftest multi-arity-test
  (is (= "2?" (make-magic-string 2)))
  (is (= "2?!" (make-magic-string "2" "!")))
  (is (= :exception (try (make-magic-string 2 "!")
                         (catch ExceptionInfo _ :exception)))))

(s/def ::number? number?)
(sdefn speced-return [[x :- ::number?]]
       x)

(deftest reference-to-speced-keywords-test
  (is (= 2 (speced-return 2)))
  (is (= :exception (try (speced-return "2")
                         (catch ExceptionInfo _ :exception)))))

(sdefn call-with-7 [[f :- (sayang/of make-magic-string)]]
       (f 7))

(deftest of-test
  (is (= "7?" (call-with-7 make-magic-string)))
  (is (= :exception (try (call-with-7 identity)
                         (catch ExceptionInfo _ :exception)))))

(defn result-larger-than-min-arg-value? [spec]
  (< (apply min (vals (:0 (:args spec))))
     (:ret spec)))

(sdefn add-map-values {:ret int?
                       :fn  result-larger-than-min-arg-value?}
       [[{:keys [a b c]} :- map?]]
       (+ a b c))

(deftest add-map-values-test
  (is (= -1 (add-map-values {:a 1 :b 0 :c -2})))
  (is (= :exception (try (add-map-values {:a -1 :b -2 :c -3})
                         (catch ExceptionInfo _ :exception)))))

(sdefn speced-add {:ret int?}
       [[xs :- [int?]]]
       (apply + xs))

(deftest every-spec-data-dsl-test
  (is (= 105 (speced-add (range 15))))
  (is (= :exception (try (speced-add (cons 1.0 (range 15)))
                         (catch ExceptionInfo _ :exception)))))

(sdefn sum-of-4-tuple {:ret float?}
       [[tuple :- [int? float? int? float?]]]
       (apply + tuple))

(deftest tuple-spec-data-dsl-test
  (is (= 10.0 (sum-of-4-tuple [1 2.0 3 4.0])))
  (is (= :exception (try (sum-of-4-tuple [1 2 3 4])
                         (catch ExceptionInfo _ :exception)))))

(sdefn speced-return-2 {:args (s/cat :x int?)}
       [x]
       x)

(deftest only-map-specs-test
  (is (= 100 (speced-return-2 100)))
  (is (= :exception (try (speced-return-2 "100")
                         (catch ExceptionInfo _ :exception)))))

(spec-test-all/instrument)
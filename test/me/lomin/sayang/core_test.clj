(ns me.lomin.sayang.core-test
  (:require [clojure.test :refer :all]
            [me.lomin.sayang.core :refer :all :as sayang]
            [me.lomin.sayang :refer [sdefn]]
            [clojure.spec.alpha :as spec]
            [clojure.spec.test.alpha :as spec-test]
            [clojure.core.specs.alpha :as specs])
  (:import (clojure.lang ExceptionInfo)))

(deftest complex-fn-test
  (is (= '{:bs   [:arity-1
                  {:args {:args    [[::sayang/type-def
                                     {:sym          x
                                      ::sayang/sep  :-
                                      ::sayang/type number?}]
                                    [::sayang/type-def
                                     {:sym          y
                                      ::sayang/sep  :-
                                      ::sayang/type number?}]
                                    [::sayang/type-def
                                     {:sym          {:keys [a b c]}
                                      ::sayang/sep  :-
                                      ::sayang/type map?}]]
                          :varargs {:amp  &
                                    :form [:seq
                                           {:as    {:as :as, :sym h}
                                            :elems [[::sayang/type-def
                                                     {:sym          d
                                                      ::sayang/sep  :-
                                                      ::sayang/type number?}]]
                                            :rest  {:amp  &
                                                    :form [::sayang/type-def
                                                           {:sym          [e _ & f :as g]
                                                            ::sayang/sep  :-
                                                            ::sayang/type sequential?}]}}]}}
                   :body [:body [(str x y)]]}]
           :name fn}
         (spec/conform ::specs/defn-args
                       '(fn
                       [[x :- number?]
                        [y :- number?]
                        [{:keys [a b c]} :- map?]
                        & [[d :- number?] & [[e _ & f :as g] :- sequential?] :as h]]
                       (str x y)))))

  (is (= '(clojure.spec.alpha/cat :x number? :y number? :test-key map?)
         (arity->spec '[:arity-1
                        {:args {:args    [[::sayang/type-def
                                           {:sym          x
                                            ::sayang/sep  :-
                                            ::sayang/type number?}]
                                          [::sayang/type-def
                                           {:sym          y
                                            ::sayang/sep  :-
                                            ::sayang/type number?}]
                                          [::sayang/type-def
                                           {:sym          {:keys [a b c]}
                                            ::sayang/sep  :-
                                            ::sayang/type map?}]]
                                :varargs {:amp  &
                                          :form [:seq
                                                 {:as    {:as :as, :sym h}
                                                  :elems [[::sayang/type-def
                                                           {:sym          d
                                                            ::sayang/sep  :-
                                                            ::sayang/type number?}]]
                                                  :rest  {:amp  &
                                                          :form [::sayang/type-def
                                                                 {:sym          [e _ & f :as g]
                                                                  ::sayang/sep  :-
                                                                  ::sayang/type sequential?}]}}]}}
                         :body [:body [(str x y)]]}]
                      (constantly :test-key)))))

(deftest arity-0-test
  (is (= '{:bs [:arity-1 {:args {}, :body [:body [1]]}], :name my-name}
         (spec/conform ::specs/defn-args
                       '(my-name [] 1)))))

(deftest arity-1-test
  (is (= '{:bs [:arity-1 {:args {:args [[:sym x]]}, :body [:body [(+ x 1)]]}], :name fn}
         (spec/conform ::specs/defn-args
                       '(fn [x]
                       (+ x 1)))))
  (is (= '{:bs   [:arity-1
                  {:args {:args [[::sayang/type-def
                                  {:sym x, ::sayang/sep :-, ::sayang/type int?}]
                                 [:sym y]]}
                   :body [:body [(+ x y) (+ y x)]]}]
           :name fn}
         (spec/conform ::specs/defn-args
                       '(fn [[x :- int?] y]
                       (+ x y)
                       (+ y x)))))
  (is (= '{:bs   [:arity-n
                  {:bodies [{:args {:args [[::sayang/type-def
                                            {:sym          x
                                             ::sayang/sep  :-
                                             ::sayang/type int?}]]}
                             :body [:body [(str x %)]]}]}]
           :name fn}
         (spec/conform ::specs/defn-args
                       '(fn
                       ([[x :- int?]]
                        (str x %))))))

  (testing "defn-form"
    (let [form (spec/conform ::specs/defn-args '(test [x] (inc x)))]
      (is (= '(clojure.spec.alpha/cat :x any?) (arity->spec (:bs form)))))))

(deftest arity-1->spec-test
  (is (= '(clojure.spec.alpha/cat :x any?)
         (args->spec '{:args {:args [[:sym x]]}
                       :body [:body [(+ x 1)]]})))
  (is (= '(clojure.spec.alpha/cat :y any?)
         (args->spec '{:args {:args [[:sym y]]}
                       :body [:body [(+ y 1)]]})))

  (is (= '(clojure.spec.alpha/cat :x number? :y any?)
         (args->spec '{:args {:args [[::sayang/type-def
                                      {:sym x, ::sayang/type number?}]
                                     [:sym y]]}
                       :body [:body [(+ x y) (+ y x)]]}))))

(deftest arity-n>spec-test
  (is (= '(clojure.spec.alpha/or :1 (clojure.spec.alpha/cat :x ::sayang/number?))
         (arity->spec '[:arity-n
                        [{:args {:args [[::sayang/type-def
                                         {:sym x, ::sayang/type ::sayang/number?}]]}
                          :body [:body [(str x %)]]}]])))

  (is (= 'empty?
         (arity->spec '[:arity-1 {:args {}, :body [:body [1]]}])))

  (is (= '(clojure.spec.alpha/cat :x ::sayang/number? :y any?)
         (arity->spec '[:arity-1
                        {:args {:args [[::sayang/type-def
                                        {:sym x, ::sayang/type ::sayang/number?}]
                                       [:sym y]]}
                         :body [:body [(+ x y) (+ y x)]]}])))
  (testing "defn-form"
    (is (= '(clojure.spec.alpha/or :2 (clojure.spec.alpha/cat :test-key any? :y any?) :1 (clojure.spec.alpha/cat :x any?))
           (arity->spec '[:arity-n
                          {:bodies
                           [{:args {:args [[:seq {:elems [[:sym x]]}] [:sym y]]}
                             :body [:body [(+ x 1)]]}
                            {:args {:args [[:sym x]]}, :body [:body [(+ x 1)]]}]}]
                        (constantly :test-key))))))

(deftest workaround-for-CLJ-2021-test
  (is (= '(f-name {} ([x y] (str x y)) ([x] (str x)))
         (workaround-for-CLJ-2021 '(f-name
                                    {}
                                    ([[x :- number?]
                                      [y :- ::test-type]]
                                     (str x y))
                                    ([[x :- number?]]
                                     (str x)))))))

(sdefn test-fn {:ret string?}
       ([[x :- int?]]
        (str x "?"))
       ([[x :- string?] [y :- string?]]
        (str x "?" y)))

(deftest test-fn-test
  (is (= "2?" (test-fn 2)))
  (is (= "2?!" (test-fn "2" "!")))
  (is (= :exception (try (test-fn 2 "!")
                         (catch ExceptionInfo _
                           :exception)))))

(spec-test/instrument)
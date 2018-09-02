(ns me.lomin.sayang.core-test
  (:require #?(:cljs [cljs.test :refer-macros [deftest is testing]]
               :clj [clojure.test :refer [deftest is testing]])
            [clojure.spec.alpha :as s]
            [clojure.test.check]
            [clojure.test.check.properties]
            [me.lomin.sayang.specs :as sayang-specs]
            [me.lomin.sayang.core :as sayang-core]))


(deftest complex-fn-test
  (is (= '{:bs   [:arity-1
                  {:args {:args    [[::sayang-specs/type-def
                                     {:sym                       x,
                                      ::sayang-specs/sep  :-,
                                      ::sayang-specs/type [::sayang-specs/spec number?]}]
                                    [::sayang-specs/type-def
                                     {:sym                       y,
                                      ::sayang-specs/sep  :-,
                                      ::sayang-specs/type [::sayang-specs/spec number?]}]
                                    [::sayang-specs/type-def
                                     {:sym                       {:keys [a b c]},
                                      ::sayang-specs/sep  :-,
                                      ::sayang-specs/type [::sayang-specs/spec map?]}]],
                          :varargs {:amp  &,
                                    :form [:seq
                                           {:as    {:as :as, :sym h},
                                            :elems [[::sayang-specs/type-def
                                                     {:sym                       d,
                                                      ::sayang-specs/sep  :-,
                                                      ::sayang-specs/type [::sayang-specs/spec number?]}]],
                                            :rest  {:amp  &,
                                                    :form [::sayang-specs/type-def
                                                           {:sym                       [e _ & f :as g],
                                                            ::sayang-specs/sep  :-,
                                                            ::sayang-specs/type [::sayang-specs/spec sequential?]}]}}]}},
                   :body [:body [(str x y)]]}],
           :name my-name}
         (s/conform ::sayang-specs/defn-args
                    '(my-name
                       [[x :- number?]
                        [y :- number?]
                        [{:keys [a b c]} :- map?]
                        & [[d :- number?] & [[e _ & f :as g] :- sequential?] :as h]]
                       (str x y)))))

  (is (= '(clojure.spec.alpha/cat :x number? :y number? :2 map?)
         (sayang-core/arity->spec '[:arity-1
                        {:args {:args    [[::sayang-specs/type-def
                                           {:sym               x
                                            ::sayang-specs/sep  :-
                                            ::sayang-specs/type [::sayang-specs/spec number?]}]
                                          [::sayang-specs/type-def
                                           {:sym               y
                                            ::sayang-specs/sep  :-
                                            ::sayang-specs/type [::sayang-specs/spec number?]}]
                                          [::sayang-specs/type-def
                                           {:sym               {:keys [a b c]}
                                            ::sayang-specs/sep  :-
                                            ::sayang-specs/type [::sayang-specs/spec map?]}]]
                                :varargs {:amp  &
                                          :form [:seq
                                                 {:as    {:as :as, :sym h}
                                                  :elems [[::sayang-specs/type-def
                                                           {:sym               d
                                                            ::sayang-specs/sep  :-
                                                            ::sayang-specs/type [::sayang-specs/spec number?]}]]
                                                  :rest  {:amp  &
                                                          :form [::sayang-specs/type-def
                                                                 {:sym               [e _ & f :as g]
                                                                  ::sayang-specs/sep  :-
                                                                  ::sayang-specs/type [::sayang-specs/spec sequential?]}]}}]}}
                         :body [:body [(str x y)]]}]))))

(deftest arity-0-test
  (is (= '{:bs [:arity-1 {:args {}, :body [:body [1]]}], :name my-name}
         (s/conform ::sayang-specs/defn-args
                    '(my-name [] 1)))))

(deftest arity-1-test
  (is (= '{:bs [:arity-1 {:args {:args [[:sym x]]}, :body [:body [(+ x 1)]]}], :name fn}
         (s/conform ::sayang-specs/defn-args
                    '(fn [x]
                       (+ x 1)))))
  (is (= '{:bs   [:arity-1
                  {:args {:args [[::sayang-specs/type-def
                                  {:sym               x
                                   ::sayang-specs/sep  :-
                                   ::sayang-specs/type [::sayang-specs/spec int?]}]
                                 [:sym y]]}
                   :body [:body [(+ x y) (+ y x)]]}]
           :name fn}
         (s/conform ::sayang-specs/defn-args
                    '(fn [[x :- int?] y]
                       (+ x y)
                       (+ y x)))))
  (is (= '{:bs   [:arity-n
                  {:bodies [{:args {:args [[::sayang-specs/type-def
                                            {:sym               x
                                             ::sayang-specs/sep  :-
                                             ::sayang-specs/type [::sayang-specs/spec int?]}]]}
                             :body [:body [(str x %)]]}]}]
           :name fn}
         (s/conform ::sayang-specs/defn-args
                    '(fn
                       ([[x :- int?]]
                        (str x %))))))

  (testing "defn-form"
    (let [form (s/conform ::sayang-specs/defn-args '(test [x] (inc x)))]
      (is (= '(clojure.spec.alpha/cat :x any?) (sayang-core/arity->spec (:bs form)))))))

(deftest arity-1->spec-test
  (is (= '(clojure.spec.alpha/cat :x any?)
         (sayang-core/args->spec '{:args {:args [[:sym x]]}
                       :body             [:body [(+ x 1)]]})))
  (is (= '(clojure.spec.alpha/cat :y any?)
         (sayang-core/args->spec '{:args {:args [[:sym y]]}
                       :body             [:body [(+ y 1)]]})))

  (is (= '(clojure.spec.alpha/cat :x number? :y any?)
         (sayang-core/args->spec '{:args {:args [[::sayang-specs/type-def
                                      {:sym               x
                                       ::sayang-specs/type [::sayang-specs/spec number?]}]
                                     [:sym y]]}
                       :body             [:body [(+ x y) (+ y x)]]}))))

(deftest arity-n>spec-test
  (is (= '(clojure.spec.alpha/or :1 (clojure.spec.alpha/cat :x ::sayang-core/number?))
         (sayang-core/arity->spec '[:arity-n
                        [{:args {:args [[::sayang-specs/type-def
                                         {:sym               x
                                          ::sayang-specs/type [::sayang-specs/spec ::sayang-core/number?]}]]}
                          :body [:body [(str x %)]]}]])))

  (is (= 'empty?
         (sayang-core/arity->spec '[:arity-1 {:args {}, :body [:body [1]]}])))

  (is (= '(clojure.spec.alpha/cat :x ::sayang-core/number? :y any?)
         (sayang-core/arity->spec '[:arity-1
                        {:args {:args [[::sayang-specs/type-def
                                        {:sym               x
                                         ::sayang-specs/type [::sayang-specs/spec ::sayang-core/number?]}]
                                       [:sym y]]}
                         :body [:body [(+ x y) (+ y x)]]}])))
  (testing "defn-form"
    (is (= '(clojure.spec.alpha/or :2 (clojure.spec.alpha/cat :0 any? :y any?) :1 (clojure.spec.alpha/cat :x any?))
           (sayang-core/arity->spec '[:arity-n
                          {:bodies
                           [{:args {:args [[:seq {:elems [[:sym x]]}] [:sym y]]}
                             :body [:body [(+ x 1)]]}
                            {:args {:args [[:sym x]]}, :body [:body [(+ x 1)]]}]}])))))

(deftest workaround-for-CLJ-2021-test
  (is (= '(f-name {} ([x y] (str x y)) ([x] (str x)))
         (sayang-core/remove-type-defs-with-workaround-for-CLJ-2021 '(f-name
                                                           {}
                                                           ([[x :- number?]
                                                             [y :- ::test-type]]
                                                             (str x y))
                                                           ([[x :- number?]]
                                                             (str x)))))))
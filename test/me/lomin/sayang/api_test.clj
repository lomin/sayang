;Specification at definition
(ns me.lomin.sayang.api-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [me.lomin.sayang :as sg]
            [orchestra.spec.test :as orchestra])
  (:import (clojure.lang ExceptionInfo)))

(declare thrown?)
(s/check-asserts true)
(alter-var-root #'sg/*activate* (constantly true))

(sg/sdefn basic-usage {:ret    string?
                       :fn #(<= (:x (:args %))
                                (count (:ret %)))}
          [[x :- int?]]
          (str x))

(deftest basic-usage-test
  (is (= "1" (basic-usage 1)))

  (testing "Fails :fn spec"
    (is (thrown? ExceptionInfo
                 (basic-usage 2))))
  (testing "Fails :args spec"
    (is (thrown? ExceptionInfo
                 (basic-usage "5")))))

(orchestra/instrument `basic-usage)

(sg/sdefn int-identity {:args (s/cat :x int?)}
          [x]
          x)

(deftest specs-from-meta-map-test
  (is (= 100 (int-identity 100)))

  (testing "Fails :args spec"
    (is (thrown? ExceptionInfo
                 (int-identity "100")))))

(orchestra/instrument `int-identity)

; Partial specification

(sg/sdefn partial-specs {:ret string?}
          [f
           [x :- int?]]
          (f x))

(deftest partial-specs-test
  (is (= "5" (partial-specs str 5)))

  (testing "Fails :args spec"
    (is (thrown? ExceptionInfo
                 (partial-specs str "5"))))
  (testing "Fails :ret spec"
    (is (thrown? ExceptionInfo
                 (partial-specs identity 5)))))

(orchestra/instrument `partial-specs)

; Support for destructuring

(sg/sdefn sum-first-two-elements
          [[[a b] :- (s/tuple int? int? int?)]]
          (+ a b))

(deftest support-for-destructuring-test
  (is (= 5 (sum-first-two-elements [2 3 4])))

  (testing "Fails :args spec"
    (is (thrown? ExceptionInfo
                 (sum-first-two-elements [2 3])))))

(orchestra/instrument `sum-first-two-elements)

; Specification for multiple arities

(sg/sdefn make-magic-string {:ret string?}
          ([[x :- int?]]
           (str x "?"))
          ([[x :- string?] [y :- string?]]
           (str x "?" y)))

(deftest multi-arity-test
  (is (= "2?" (make-magic-string 2)))
  (is (= "2?!" (make-magic-string "2" "!")))

  (testing "Fails :args spec for arity-2"
    (is (thrown? ExceptionInfo
                 (make-magic-string 2 "!")))))

(orchestra/instrument `make-magic-string)

(defn result-larger-than-min-arg-value? [spec]
  (< (apply min (vals (:0 (:args spec))))
     (:ret spec)))

(sg/sdefn add-map-values {:ret    int?
                          :fn result-larger-than-min-arg-value?}
          [[{:keys [a b c]} :- map?]]
          (+ a b c))

(deftest fn-spec-for-multi-arity-test
  (is (= -1 (add-map-values {:a 1 :b 0 :c -2})))

  (testing "Fails :fn spec"
    (is (thrown? ExceptionInfo
                 (add-map-values {:a -1 :b -2 :c -3})))))

(orchestra/instrument `add-map-values)

; Reference other specs

(s/def ::number? number?)
(sg/sdefn number-identity [[x :- ::number?]]
          x)

(deftest reference-to-speced-keywords-test
  (is (= 2 (number-identity 2)))

  (testing "Fails :args spec"
    (is (thrown? ExceptionInfo
                 (number-identity "2")))))

(orchestra/instrument `number-identity)

(sg/sdefn call-with-7 [[f :- (sg/of make-magic-string)]]
          (f 7))

(deftest of-test
  (is (= "7?" (call-with-7 make-magic-string)))

  (testing "'identity' does not fulfill fdef of 'make-magic-string'"
    (is (thrown? ExceptionInfo
                 (call-with-7 identity)))))

(orchestra/instrument `call-with-7)

; Data DSL for homogeneous collections

(sg/sdefn speced-add {:ret int?}
          [[xs :- [int?]]]
          (apply + xs))

(deftest every-spec-data-dsl-test
  (is (= 105 (speced-add (range 15))))

  (testing "1.0 is no integer, therefore xs is no homogeneous collection any more"
    (is (thrown? ExceptionInfo
                 (speced-add (cons 1.0 (range 15)))))))

(orchestra/instrument `speced-add)

;Data DSL for tuples

(sg/sdefn sum-of-4-tuple {:ret float?}
          [[tuple :- [int? float? int? float?]]]
          (apply + tuple))

(deftest tuple-spec-data-dsl-test
  (is (= 10.0 (sum-of-4-tuple [1 2.0 3 4.0])))

  (testing "Fails tuple spec of :args"
    (is (thrown? ExceptionInfo
                 (sum-of-4-tuple [1 2 3 4])))))

(orchestra/instrument `sum-of-4-tuple)
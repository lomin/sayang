# sayang

*sayang* complects the definition of a function with its specification.

## Rationale

A useful summary of any function is the combination of its name, its expectations about the input and guarantees of its output.
[clojure.spec](https://clojure.org/about/spec) provides this kind of function summary with the [s/fdef](https://clojure.org/guides/spec#_spec_ing_functions) macro.
With *clojure.spec*, definition and specification of a function are separated concerns. With *sayang*, you can optionally complect these concerns.
When you try to read or change a function, it is helpful to have the specification right next to the implementation.
Additionally, if you need to change the number or the order of the function parameters, you only have to change it once.

While *sayang* values a resemblance to the syntax of [prismatic/schema](https://github.com/plumatic/schema), it values not interfering with functionality of [Cursive](https://cursive-ide.com/) more.
Use the *resolve macro as*-Feature of *Cursive* to get the same tooling as function definitions with *clojure.core*. 

## Leiningen

*sayang* is available from Clojars. Add the following dependency to your *project.clj*:

![](https://clojars.org/me.lomin/sayang/latest-version.svg)
## Usage

```clojure
(ns my-test
    (:require [clojure.test :refer :all]
              [clojure.spec.test.alpha :as spec-test]
              [me.lomin.sayang :as sayang])
    (:import (clojure.lang ExceptionInfo)))

(sayang/sdefn test-fn {:ret string?}
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
```

## Limitations

Does not work with varargs or within destructuring. If *sayang* turns out to be useful, this could be fixed in the future. 

## About

Sayang (사양) means specification in Korean.

## License

Copyright © 2017 Steven Collins

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

(ns cond-val.core-test
  (:require [clojure.test :refer :all]
            [cond-val.core :refer :all]))

(deftest test
  (testing "apply inc using -> to successfully calculated value, here by lookup (:a x). Value should not be decreased since there are no :c, and catch all with 0 as value"
    (is (= 2
           (cond-val-> {:a 1 :b 10}
                       (:c) (dec) ;; should not match
                       (:a) (inc)
                       0)))) ;; there is an :a, so default should not apply

  (testing "apply (- 3) using ->>, to sucessfully calculated value, by looking it up. (- 1) should not be aplied since there are no :c, and catch-all as 0 ")
  (is (= 2
         (cond-val->> {:a 1 :b 10}
                      (:c) (- 1)
                      (:a) (- 3) ;; (- 3 1)
                      0)))

  (testing "default value "
    (is (= 0
           (cond-val-> {:b 10}
                       (:c) (dec)
                       (:a) (inc)
                       0)))
    (is (= 0
           (cond-val->> {:b 10}
                        (:c) (- 1)
                        (:a) (- 3)
                        0))))
;;; compliance testing
  
  (testing "terminating"
    (is (= 2
           (cond-val-> {:c 3}
                       (:c) (dec)
                       (:a) (inc)
                       0)))
    (is (= -2
           (cond-val->> {:c 3}
                        (:c) (- 1)
                        (:a) (- 3)
                        0))))

  (testing "double-eval - first, here with swap! effect, should only be evaluated once"
    (let [n (atom 0)]
      (cond-val->> ((fn []
                      (swap! n inc)
                      {:a 3}))
                   (:c) (- 1)
                   (:a) (- 3)
                   0)
      (is (= 1 @n)))
    (let [n (atom 0)]
      (cond-val-> ((fn []
                     (swap! n inc)
                     {:a 3}))
                  (:c) (dec)
                  (:a) (inc)
                  0)
      (is (= 1 @n)))

    (comment
      (cond-val-> {:a 1})
      (cond-val->> {:a 1}))) ;; should complain lacking clauses, at read

  (testing "only default"
    (is (= 2 (cond-val-> {:a 1} 2)))
    (is (= 2 (cond-val->> {:a 1} 2))))

  (testing "first is nil"
    (is (= 2 (cond-val-> nil
                         (:a) (inc)
                         2)))
    (is (= 2 (cond-val->> nil
                          (:a) (- 3)
                          2)))))


(ns bitfinex-api.core-test
  (:require [clojure.test :refer :all]
            [bitfinex-api.core :refer :all]))

(def cfg {})
(deftest should-have-server
  (testing "Correctly interprets server URL from config."
    (is (= (server)
         "https://api.bitfinex.com/v1"))))

(deftest make-correct-indeces
  (testing "Make list of lists of  correct indices to individual
elements."
    (is (= (make-index-list {:a [{:b 0} {:c 1} {:d 2}] :b [{:a 3}]})
           [[:a 0 :b] [:a 1 :c] [:a 2 :d] [:b 0 :a]]))))

(deftest only
  (is (= 1 1)))

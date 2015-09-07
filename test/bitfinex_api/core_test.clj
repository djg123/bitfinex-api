(ns bitfinex-api.core-test
  (:require [clojure.test :refer :all]
            [bitfinex-api.core :refer :all]))

(def cfg {})
(deftest should-have-server
  (testing "Correctly interprets server URL from config."
    (is (= (server)
         "https://api.bitfinex.com/v1"))))

(deftest only
  (is (= 1 1)))

(ns bitfinex-api.core
  (:require [clj-http.client :as client]
            [clojure.string :as s]
            [clojure.data.json :as json]
            [clojure.algo.generic.functor :refer [ fmap]]))

(declare build-parameters get-request convert-to-floats
         make-index-list)

(def protocol "https")
(def host  "api.bitfinex.com")
(def version "v1")

(def path-symbols "symbols")
(def path-ticker "ticker/%s")
(def path-today "today/%s")
(def path-stats  "stats/%s")
(def path-lendbook "lendbook/%s")
(def path-orderbook "book/%s")
(def default-timeout 5000)


(defn server []
  "Creates server url from config information."
  (format "%s://%s/%s" protocol host version))


(defn url-for 
  ([ path]
   (url-for  path nil nil))
  ([ path path-arg]
   (url-for  path path-arg nil))
  ([ path path-arg parameters]
   (let [url (format "%s/%s" (server) path)
         with-path-arg (if path-arg
                         (format url path-arg)
                         url)]
     (if parameters
       (format url with-path-arg (build-parameters parameters))
       with-path-arg))))


(defn symbols
  "GET /symbols
  curl https://api.bitfinex.com/v1/symbols
  ['btcusd', 'ltcusd', 'ltcbtc']
  "
  []
  (json/read-json
   (:body (get-request (url-for path-symbols)))))

(defn ticker "GET /ticker/:symbol
  curl https://api.bitfinex.com/v1/ticker/btcusd
  {
  'ask': '562.9999',
  'timestamp': '1395552290.70933607',
  'bit': '562.25',
  'last_price': u'562.25',
  'mid': u'562.62495'}"
  [ticker-symbol]
  (convert-to-floats

   ((comp json/read-json :body)
    (get-request (url-for path-ticker ticker-symbol)))))

(defn today "GET /today/:symbol
        curl \\\"https://api.bitfinex.com/v1/today/btcusd\\\"
        {\\\" low \\\":\\\"550.09\\\",\\\"high\\\":\\\"572.2398\\\",\\\"volume\\\":\\\"7305.33119836\\\"}"
  [ ticker-symbol]
  (convert-to-floats
   (get-request (url-for path-today ticker-symbol))))

(defn stats "curl https://api.bitfinex.com/v1/stats/btcusd
        [
            {\"period\":1,\"volume\":\"7410.27250155\"},
            {\"period\":7,\"volume\":\"52251.37118006\"},
            {\"period\":30,\"volume\":\"464505.07753251\"}
        ]"
  [ ticker-symbol]
  (convert-to-floats
   (let [period (get-request
                 (url-for  path-stats  ticker-symbol))]
     (reduce (fn [a [k v]]
               (conj a
                     (case k
                       "period" [k (int v)]
                       "volume" [k (float v)])))
             {} period))))

(defn lendbook  "curl \"https://api.bitfinex.com/v1/lendbook/btc\"
        {\"bids\":[{\"rate\":\"5.475\",\"amount\":\"15.03894663\",\"period\":30,\"timestamp\":\"1395112149.0\",\"frr\":\"No\"},{\"rate\":\"2.409\",\"amount\":\"14.5121868\",\"period\":7,\"timestamp\":\"1395497599.0\",\"frr\":\"No\"}],\"asks\":[{\"rate\":\"6.351\",\"amount\":\"15.5180735\",\"period\":5,\"timestamp\":\"1395549996.0\",\"frr\":\"No\"},{\"rate\":\"6.3588\",\"amount\":\"626.94808249\",\"period\":30,\"timestamp\":\"1395400654.0\",\"frr\":\"Yes\"}]}
        Optional parameters
        limit_bids (int): Optional. Limit the number of bids (loan demands) returned. May be 0 in which case the array of bids is empty. Default is 50.
        limit_asks (int): Optional. Limit the number of asks (loan offers) returned. May be 0 in which case the array of asks is empty. Default is 50."
  ([ currency params]
   (let [data (get-request (url-for path-lendbook
                                    currency params))]
     (for [lend-type data
           lend (:lend-type data)]
       (reduce (fn [a [k v]]
                 (conj a
                       (condp #(some #{%2} %1) k
                         ["rate" "amount" "timestamp"]
                         [k (float v)]

                         ["period"]
                         [k (int v)]

                         ["frr"]
                         [k (= v "Yes")]))))))))

(defn order-book  "curl \"https://api.bitfinex.com/v1/book/btcusd\"
        {\"bids\":[{\"price\":\"561.1101\",\"amount\":\"0.985\",\"timestamp\":\"1395557729.0\"}],\"asks\":[{\"price\":\"562.9999\",\"amount\":\"0.985\",\"timestamp\":\"1395557711.0\"}]}
        The 'bids' and 'asks' arrays will have multiple bid and ask dicts.
        Optional parameters
        limit_bids (int): Optional. Limit the number of bids returned. May be 0 in which case the array of bids is empty. Default is 50.
        limit_asks (int): Optional. Limit the number of asks returned. May be 0 in which case the array of asks is empty. Default is 50.
        eg.
        curl \"https://api.bitfinex.com/v1/book/btcusd?limit_bids=1&limit_asks=0\"
        {\"bids\":[{\"price\":\"561.1101\",\"amount\":\"0.985\",\"timestamp\":\"1395557729.0\"}],\"asks\":[]}"
  ([ ticker-symbol]
   (order-book  ticker-symbol nil))
  ([ ticker-symbol params]
   (let [data (get-request
               (url-for  path-orderbook ticker-symbol params))]
     (reduce (fn [a indeces]
               (update-in a indeces float)) data
               (make-index-list data)))))
;         (update-in data [type index sub-key] float)

(defn make-index-list
  [data]
  (for [type (keys data)
        index (range 0 (count (type data)))
        sub-key (keys (get-in data [type index]))]
    [type index sub-key]))


(defn convert-to-floats [data]
  (reduce
   (fn [a [k v]]
     (assoc a k (Double/parseDouble v))) {} data))

(defn build-parameters [parameters]
  (s/join "&"
          (map (fn [[k v]] (format "%s=%s" k v))
               parameters)))

(defn get-request ([url timeout]
                   (client/get url {:accept :json
                                    :conn-timeout timeout}))
  ([url ]
   (get-request url default-timeout)))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

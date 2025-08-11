(ns await-cps.tests
  (:require [await-cps :as ac]
            [cljs.test :refer [is deftest async testing]]))

(deftest await-sync-short-circuits
  (testing "if a wrapped function returns synchronously, it resolves synchronously"
    (let [events (atom [])
          r (fn [v] (swap! events conj [:r v]))
          e (fn [t] (swap! events conj [:e t]))
          f (fn [r e] (r :ok))
          ret (ac/do-await r e f)]
      (is (nil? ret))
      (is (= [[:r :ok]] @events)))))

(deftest await-async-defers
  (async done
    (testing "if a wrapped function returns asynchronously, it resolves asynchronously"
      (let [events (atom [])
            r (fn [v] (swap! events conj [:r v]) (done))
            e (fn [t] (swap! events conj [:e t]) (done))
            f (fn [r e] (js/setTimeout (fn [] (r :ok)) 0))
            ret (ac/do-await r e f)]
        (is (nil? ret))
        (is (empty? @events))))))

;; TODO
;; (ok|error) * (sync|async)
;; resolution racing (first always wins)
;; arguments, compound forms etc etc

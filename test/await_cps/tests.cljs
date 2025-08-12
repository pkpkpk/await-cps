(ns await-cps.tests
  (:require [await-cps :refer [defn-async afn await]]
            [cljs.test :refer [is deftest async testing]]))

(defn sync-ok [r e] (r :ok))

(deftest await-sync-short-circuits
  (testing "sync returns resolve in the same tick"
    (let [events (atom nil)
          r (fn [v] (reset! events [:r v]))
          e (fn [e] (reset! events [:e e]))]
      (and
       (is (nil? @events))
       (is (nil? ((afn [] (await sync-ok)) r e)))
       (is (= [:r :ok] @events))
       (is (nil? ((afn [] (throw (js/Error. "kaboom"))) r e)))
       (is (instance? js/Error (second @events)))))))

(defn async-ok [r e] (js/setTimeout #(r :ok) 0))

(deftest await-async-defers
  (async done
    (testing "async defers to a later tick"
      (let [events (atom nil)
            r (fn [v]
                (reset! events [:r v])
                (is (= [:r :ok] @events))
                (done))
            e (fn [t] (throw (js/Error. "unexpected")))]
        (and
         (is (nil? @events))
         (is (nil? ((afn [] (await async-ok)) r e))))))))


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
(defn async-err [r e] (js/setTimeout #(e :err) 0))

(deftest await-async-defers
  (async done
    (testing "async defers to a later tick"
      (let [fatal (fn [t] (throw (js/Error. (str "unexpected: " t))))
            expect-err (fn [e]
                         (is (= :err e))
                         (done))
            expect-ok (fn [v]
                        (if-not (is (= :ok v))
                          (done)
                          (is (nil? ((afn [] (await async-err)) fatal expect-err)))))]
        (is (nil? ((afn [] (await async-ok)) expect-ok fatal)))))))


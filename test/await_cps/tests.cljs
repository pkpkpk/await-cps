(ns await-cps.tests
  (:require [await-cps :refer [defn-async afn await]]
            [cljs.test :refer [is deftest async testing]]))

(defn sync-ok [r e] (r :ok))

(deftest await-sync-short-circuits
  (testing "sync returns resolve in the same tick"
    (let [state (atom nil)
          r (fn [v] (reset! state v))
          e (fn [e] (reset! state e))]
      (and
       (is (nil? @state))
       (is (nil? ((afn [] (await sync-ok)) r e)))
       (is (= :ok @state))
       (is (nil? ((afn [] (throw (js/Error. "kaboom"))) r e)))
       (is (and (instance? js/Error @state)
                (= "kaboom" (ex-message @state))))))))

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

(defn-async async-test []
  (and
   (is (= :ok (await async-ok)) "await returns ok value")
   (println "this should not print" (await async-err))))

(deftest defn-async-test
  (async done
    (let [on-ok  #(throw (js/Error. (str "unexpected: " %)))
          on-err #(do
                    (is (= :err %) "call to async-err takes on-err path")
                    (done))]
      (testing "defn-async functions containing await calls"
        (is (nil? (async-test on-ok on-err)) "invocation return is always nil")))))

(defn async-inc [n r e] (js/setTimeout #(r (inc n)) 0))

(defn-async async-loop-increments [n]
  (loop [i 0
         acc []]
    (if (< i n)
      (recur (await async-inc i) (conj acc i))
      acc)))

(deftest await-async-loop
  (let [n 10]
    (async done
      (testing "async loop increments the counter a few times"
        (async-loop-increments n
          (fn [result]
            (is (= (vec (range n)) result))
            (done))
          (fn [t]
            (throw (js/Error. (str "unexpected: " t)))))))))



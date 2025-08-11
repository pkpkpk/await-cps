(ns await-cps
  "CLJC async/await syntax for CPS-style functions (callbacks as last two args: resolve, raise)."
  (:refer-clojure :exclude [await bound-fn])
  #?(:clj  (:require [await-cps.ioc :refer [coroutine]]))
  #?(:cljs (:require-macros [await-cps :refer [afn defn-async bound-fn]])))

#?(:clj
   (defn ^:no-doc bound-fn
     [f]
     (let [bound-frame (clojure.lang.Var/getThreadBindingFrame)]
       (fn [& args]
         (let [call-site-frame (clojure.lang.Var/getThreadBindingFrame)]
           (clojure.lang.Var/resetThreadBindingFrame bound-frame)
           (try
             (apply f args)
             (finally
               (clojure.lang.Var/resetThreadBindingFrame call-site-frame)))))))
   :cljs
   (def ^:no-doc bound-fn identity))

#?(:clj
   (defn ^:no-doc swap-vals!* [a f] (clojure.core/swap-vals! a f))
   :cljs
   (defn ^:no-doc swap-vals!* [a f]
     (let [old# (atom nil)
           new# (swap! a (fn [s#] (reset! old# s#) (f s#)))]
       [@old# new#])))

(defn ^:no-doc do-await
  [r e f & args]
  (let [state   (atom [:start])
        resolve (fn [v]
                  (let [[[before r']]
                        (swap-vals!* state
                                     #(case (first %)
                                        :start [:resolved v]
                                        :async [:completed]
                                        %))]
                    (when (= before :async) (r' v))))
        raise   (fn [t]
                  (let [[[before _ e']]
                        (swap-vals!* state
                                     #(case (first %)
                                        :start [:raised t]
                                        :async [:completed]
                                        %))]
                    (when (= before :async) (e' t))))]
    (apply f (concat args [resolve raise]))
    (let [run (bound-fn trampoline)
          [[before x]]
          (swap-vals!* state
                       #(case (first %)
                          :start [:async (partial run r) (partial run e)]
                          :resolved [:completed]
                          :raised [:completed]
                          %))]
      (case before
        ;; short-circuit sync completion: no extra trampoline hop
        :resolved (do (r x) nil)
        :raised   (do (e x) nil)
        ;; async path keeps the prepared (partial run r/e) in state; do-await returns nil
        nil))))

(defn ^:no-doc run-async
  [f resolve raise]
  (let [run (bound-fn trampoline)]
    (run f resolve raise)
    nil))

(defn await
  "Signal suspension point. Must be used inside afn/defn-async bodies."
  [cps-fn & args]
  (throw (new #?(:clj IllegalStateException :cljs js/Error) "await called outside of asynchronous scope")))

(def ^:no-doc terminators
  {`await `do-await})

(defmacro afn
  "Defines an asynchronous function. Adds &resolve and &raise continuation params.
   Executes synchronously until the first (await ...)."
  {:arglists '([name? [params*] body])}
  [& args]
  (let [[a & [b & cs :as bs]] args
        [name params body] (if (symbol? a) [a b cs] [nil a bs])
        arg-names          (map #(if (symbol? %) % (gensym)) params)]
    `(fn ~@(when name [name]) [~@arg-names ~'&resolve ~'&raise]
       (run-async
         (coroutine ~terminators
                    (loop [~@(interleave params arg-names)] ~@body))
         ~'&resolve ~'&raise))))

(defmacro defn-async
  "Like defn, but defines an asynchronous function (see afn)."
  {:arglists '([name doc-string? attr-map? [params*] body])}
  [name & args]
  (let [[a & [b & [c & ds :as cs] :as bs]] args
        [doc attrs params body]
        (if (string? a)
          (if (map? b) [a b c ds] [a nil b cs])
          (if (map? a) [nil a b cs] [nil nil a bs]))
        arglists  `'([~@params ~'&resolve ~'&raise])
        attrs     (update attrs :arglists #(or % arglists))
        arg-names (map #(if (symbol? %) % (gensym)) params)]
    `(defn ~name ~@(when doc [doc]) ~attrs [~@arg-names ~'&resolve ~'&raise]
       (run-async
         (coroutine ~terminators
                    (loop [~@(interleave params arg-names)] ~@body))
         ~'&resolve ~'&raise))))

;; ---------- JVM-only blocking helpers ----------

#?(:clj
   (defn ^:no-doc either-promise
     [cps-fn & args]
     (let [p (promise)
           f (apply partial cps-fn args)
           r #(deliver p [%])
           e #(deliver p [nil %])]
       (try (f r e)
            (catch Throwable t (e t)))
       p)))

#?(:clj
   (defn await!
     "Blocking variant (JVM only). Do not use inside asynchronous functions."
     [cps-fn & args]
     (let [[v t] @(apply either-promise cps-fn args)]
       (if t (throw t) v))))

#?(:clj
   (defn ^:no-doc blocking* [f]
     (fn [r e]
       (future
         (let [[v t] (try [(f)]
                          (catch Throwable t [nil t]))]
           (if t (e t) (r v)))))))

#?(:clj
   (defmacro blocking
     "Runs body in a future and resumes within that future (JVM only)."
     [& body]
     `(blocking* (fn [] ~@body))))

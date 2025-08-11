(ns await-cps
  (:refer-clojure :exclude [await bound-fn])
  (:require [await-cps.ioc :refer [coroutine]]))

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
               (clojure.lang.Var/resetThreadBindingFrame call-site-frame))))))))
#?(:cljs
   (defmacro ^:no-doc bound-fn
     [f]
     `(cljs.core/bound-fn [& args#] (apply ~f args#))))

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
        :resolved (partial r x)
        :raised   (partial e x)
        nil))))

(defn ^:no-doc run-async
  [f resolve raise]
  (let [run (bound-fn trampoline)]
    (run f resolve raise)
    nil))

(defn await
  [cps-fn & args]
  (throw (new #?(:clj IllegalStateException :cljs js/Error)
              "await called outside of asynchronous scope")))

(def ^:no-doc terminators
  {`await `do-await})

(defmacro afn
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

(def
  ^{:macro true
    :deprecated "0.1.9"}
  fn-async
  #'afn)

(defmacro
  ^{:deprecated "0.1.12"}
  async
  [resolve raise & body]
  `((afn [] ~@body) ~resolve ~raise))

(defmacro defn-async
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
     [& body]
     `(blocking* (fn [] ~@body))))
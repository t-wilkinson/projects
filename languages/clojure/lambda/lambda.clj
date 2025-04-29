(ns lambda
  (:require [clojure.core.match :refer [match]]))

(defn eval-expr
  "a lambda calculus in clojure"
  [expr env]
  (match expr
         (x :guard symbol?) (env x)
         (['l ([x] :seq) body] :seq) (fn [arg]
                                       (eval-expr body (fn [y]
                                                         (if (= x y)
                                                           arg
                                                           (env y)))))
         ([rator rand] :seq) ((eval-expr rator env)
                              (eval-expr rand env))
         x x))

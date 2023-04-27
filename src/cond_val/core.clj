(ns cond-val.core)

(defmacro cond-val->
  "Like cond but the function is applied to value of the test, using ->.
  Whentempted to do the test lookup twice, and picking each case in
  surrounding let feel awkward. And when core.match seems a bit too complex"
  [expr & clauses]
  (assert (< 0 (count clauses))
          (str "There are no clauses: " &form))
  (if-not (next clauses)
    (first clauses)
    (let [value (gensym "value-")
          ex (gensym "expr-value-")
          cls (partition 2 clauses)
          fun (fn fun [[[t v] & remaining]]
                (list 'clojure.core/if-let
                      [value (list 'clojure.core/-> ex t)]
                      (list 'clojure.core/-> value v)
                      (if remaining
                        (fun remaining)
                        (if (even? (count clauses))
                          'nil
                          (last clauses)))))]
      (list 'let [ex expr] (fun cls)))))

(defmacro cond-val->> 
  "Like cond but the function is applied to value of the test, using ->>.
  When tempted to do the test lookup twice, and picking each case in
  surrounding let feel awkward. And when core.match seems a bit too complex"
  [expr & clauses]
  (assert (< 0 (count clauses))
          (str "There are no clauses: " &form))
  (if-not (next clauses)
    (first clauses)
    (let [value (gensym "value-")
          ex (gensym "expr-value-")
          cls (partition 2 clauses)
          fun (fn fun [[[t v] & remaining]]
                (list 'clojure.core/if-let
                      [value (list 'clojure.core/-> ex t)]
                      (list 'clojure.core/->> value v)
                      (if remaining
                        (fun remaining)
                        (if (even? (count clauses))
                          'nil
                          (last clauses)))))]
      (list 'let [ex expr] (fun cls)))))


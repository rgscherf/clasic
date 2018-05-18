(ns clasic.evaluator
  (:require [clasic.lexer :as lexer]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(declare new-binding)
(declare eval-clause)
(declare eval-clauses)

(def initial-env  {:bindings (list {:plus +
                                    :minus -
                                    :mult *
                                    :print println
                                    :div /})})

(defn- remove-str-single-quotes
  [s]
  (string/replace s #"'" ""))

(defn- lookup
  "Look up a symbol in the binding stack."
  [envs kw]
  (if (empty? envs)
    (throw (Exception. (str "Symbol <<" (name kw) ">> was not found in environment.")))
    (let [[first-env & rest-envs] envs
          result (get first-env kw)]
      (if result
        result
        (lookup rest-envs kw)))))

(defn- eval-identifier
  "Retrieve the value of a (string) identifier from the binding stack."
  [{:keys [bindings]} [identifier]]
  (->> identifier keyword (lookup bindings)))

(defn- expr-ret
  "Pack the result of an expression into a return map"
  [env result]
  {:env env :result result})

(defn- increment-stack
  [{:keys [bindings] :as env}]
  (assoc env :bindings (cons {} bindings)))

(defn- decrement-stack
  [{:keys [bindings] :as env}]
  (assoc env :bindings (rest bindings)))

(defn- eval-ctx
  "Evaluate a context expression"
  [env expr-args]
  (let [ret (eval-clauses {:env (increment-stack env)}
                          expr-args)]
    (expr-ret (-> ret :env decrement-stack)
              (:result ret))))

(defn eval-expr
  "Evaluate a single expression."
  [env [_ [expr-type & expr-args]]]
  (case expr-type
    :CTX (eval-ctx env expr-args)
    :LET  (new-binding env expr-args)
    :SYMBOL (expr-ret env (eval-identifier env expr-args))
    :NUMBER (expr-ret env (-> expr-args first edn/read-string))
    :STRING (expr-ret env (-> expr-args first remove-str-single-quotes))
    ;; TODO rewrite CALL in terms of eval-clauses 
    :CALL (expr-ret env (eval (map (comp :result (partial eval-clause env)) expr-args)))))

(defn- new-binding
  "Add a new binding to the current stack frame."
  [env [[_ id-string] expr]]
  (let [binding-stack (-> env :bindings)
        letval (:result (eval-expr env expr))]
    {:result letval
     :env (assoc env :bindings
                 (cons (assoc (first binding-stack)
                              (keyword id-string)
                              letval)
                       (rest binding-stack)))}))

(defn- eval-clause
  "Eval a single clause."
  [env [clause-type & _ :as clause-expr]]
  (case clause-type
    :EXPR (eval-expr env clause-expr)))

(defn- eval-clauses
  "Reduce return maps through a structure of clauses."
  [init-env exprs]
  (reduce (fn [env new-expr]
            (eval-clause (:env env) new-expr))
          init-env
          exprs))

(defn evaluate-str
  "Take an input string and evaluate it using language grammar."
  [instr]
  (->> instr
       (lexer/clasic-lexer)
       (eval-clauses {:env initial-env})))

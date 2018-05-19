(ns clasic.evaluator
  (:require [clasic.lexer :as lexer]
            [clojure.spec.alpha :as spec]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(declare new-binding)
(declare eval-exprs)
(declare eval-call)

(def initial-env  {:result nil
                   :env {:bindings (list {:plus +
                                          :minus -
                                          :mult *
                                          :print println
                                          :list list
                                          :div /})}})

(defn- remove-str-single-quotes
  [s]
  (string/replace s #"'" ""))

(defn- lookup
  "Look up a symbol in the binding stack."
  [envs kw]
  (println "lookup envs:" envs)
  (if (empty? envs)
    (throw (Exception. (str "Symbol <<" (name kw) ">> was not found in environment.")))
    (let [[first-env & rest-envs] envs
          result (get first-env kw)]
      (if result
        result
        (lookup rest-envs kw)))))

(comment (evaluate-str "print(1 2 3)"))

(defn- eval-identifier
  "Retrieve the value of a (string) identifier from the binding stack."
  [{:keys [bindings] :as received-env} [identifier]]
  (println "in eval-identifier, here is received env: " received-env)
  (println "in eval-identifier, here is identifier: " identifier)
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
  (let [ret (eval-exprs {:env (increment-stack env)}
                        expr-args)]
    (expr-ret (-> ret :env decrement-stack)
              (:result ret))))

(defn eval-expr
  "Evaluate a single expression."
  [env [expr-type & expr-args]]
  (println "evaluating expr, here is env:" env)
  (case expr-type
    :CTX (eval-ctx env expr-args)
    :LET  (new-binding env expr-args)
    :SYMBOL (expr-ret env (eval-identifier env expr-args))
    :NUMBER (expr-ret env (-> expr-args first edn/read-string))
    :STRING (expr-ret env (-> expr-args first remove-str-single-quotes))
    :CALL (expr-ret env (eval-call (:env env) expr-args))))

(defn- eval-call
  [env exprs]
  (->> exprs
       (map (partial eval-exprs {:env env}))
       (map :result)
       (apply list)
       eval))

(defn- new-binding
  "Add a new binding to the current stack frame."
  [env [[_ id-string :as _] expr]]
  (let [binding-stack (-> env :bindings)
        letval (:result (eval-expr env expr))]
    {:result letval
     :env (assoc env :bindings
                 (cons (assoc (first binding-stack)
                              (keyword id-string)
                              letval)
                       (rest binding-stack)))}))

(defn- eval-exprs
  "Reduce return maps through a structure of clauses."
  [init-env exprs]
  (reduce (fn [env new-expr]
            (eval-expr (:env env) new-expr))
          init-env
          exprs))

(defn evaluate-str
  "Take an input string and evaluate it using language grammar."
  [instr]
  (->> instr
       (lexer/clasic-lexer)
       (eval-exprs initial-env)))

(comment

  (eval-exprs initial-env (lexer/clasic-lexer "print(1)"))

  (evaluate-str "print(1)")

  (evaluate-str "10")

  (evaluate-str "let x = 4
                    plus(1 2 3 x)"))

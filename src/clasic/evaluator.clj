(ns drty.evaluator
  (:require [drty.lexer :as lexer]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(s/def :compiler/bindings list?)
(s/def :compiler/env (s/keys :req [:compiler/bindings]))
(s/def :compiler/evalresult (s/keys :req [:compiler/result :compiler/env]))

(declare new-binding)
(declare eval-exprs)
(declare eval-call)

(defn- explain-invalid-spec
  [tag spec expr]
  (if (not (s/valid? spec expr))
    (do
      (println tag "failed spec with explaination:")
      (s/explain spec expr))))

(def initial-env  {:compiler/result nil
                   :compiler/env {:compiler/bindings (list {:plus +
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
  (if (empty? envs)
    (throw (Exception. (str "Symbol <<" (name kw) ">> was not found in environment.")))
    (let [[first-env & rest-envs] envs
          result (get first-env kw)]
      (if result
        result
        (lookup rest-envs kw)))))

(defn- eval-identifier
  "Retrieve the value of a (string) identifier from the binding stack."
  [env [identifier]]
  (let [binding-seq (get-in env [:compiler/env :compiler/bindings])]
    (->> identifier keyword (lookup binding-seq))))

(defn- expr-ret
  "Pack the result of an expression into a return map"
  [env result]
  (explain-invalid-spec "expr-ret" :compiler/evalresult env)
  (assoc env :compiler/result result))

(defn- increment-stack
  [{:keys [bindings] :as env}]
  (assoc env :compiler/bindings (cons {} bindings)))

(defn- decrement-stack
  [{:keys [bindings] :as env}]
  (assoc env :compiler/bindings (rest bindings)))

(defn- eval-ctx
  "Evaluate a context expression"
  [env expr-args]
  (let [ret (eval-exprs {:compiler/env (increment-stack env)}
                        expr-args)]
    (expr-ret (-> ret :compiler/env decrement-stack)
              (:compiler/result ret))))

(defn eval-expr
  "Evaluate a single expression."
  [env [expr-type & expr-args]]
  (explain-invalid-spec "eval-expr" :compiler/evalresult env)
  (case expr-type
    :CTX (eval-ctx env expr-args)
    :LET  (new-binding env expr-args)
    :SYMBOL (expr-ret env (eval-identifier env expr-args))
    :NUMBER (expr-ret env (-> expr-args first edn/read-string))
    :STRING (expr-ret env (-> expr-args first remove-str-single-quotes))
    :CALL (expr-ret env (eval-call env expr-args))))

(defn- eval-call
  [env exprs]
  (explain-invalid-spec "eval-call" :compiler/evalresult env)
  (->> (map (partial eval-expr env) exprs)
       (map :compiler/result)
       (apply list)
       eval))

(defn- new-binding
  "Add a new binding to the current stack frame."
  [env [[_ id-string :as _] expr]]
  (let [envbindings (:compiler/env env)
        binding-stack (:compiler/bindings envbindings)
        letval (:compiler/result (eval-expr env expr))]
    {:compiler/result letval
     :compiler/env (assoc envbindings
                          :compiler/bindings
                          (apply list
                                 (cons (assoc (first binding-stack)
                                              (keyword id-string)
                                              letval)
                                       (rest binding-stack))))}))

(defn- eval-exprs
  "Reduce return maps through a structure of clauses."
  [init-env exprs]
  (reduce (fn [env new-expr]
            (eval-expr env new-expr))
          init-env
          exprs))

(defn evaluate-str
  "Take an input string and evaluate it using language grammar."
  [instr]
  (->> instr
       (lexer/drty-lexer)
       (eval-exprs initial-env)))

(comment

  initial-env initial-env

  (eval-exprs initial-env (lexer/drty-lexer "print(1)"))

  (lexer/drty-lexer "print(1)")

  (evaluate-str "plus(1 plus(2 1))")

  (evaluate-str "10")

  (evaluate-str "let x = 4")

  (evaluate-str "let x = 4
                    plus(1 2 3 x)")

  "end comment")

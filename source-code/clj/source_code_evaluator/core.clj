
(ns source-code-evaluator.core
    (:require [string.api :as string]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn invoke
  ; @warning
  ; It's really sad, but the compiler cannot resolve functions like this :(
  ; This function is unavailable until the problem is resolved.
  ;
  ; @description
  ; Invokes the given 'function-name' as a symbol of function.
  ;
  ; @param (string) function-name
  ; @param (list of *) params
  ;
  ; @usage
  ; (invoke "conj" [:a :b] :c)
  ;
  ; @example
  ; (invoke "conj" [:a :b] :c)
  ; =>
  ; [:a :b :c]
  ;
  ; @return (*)
  [function-name & params]
  (-> function-name symbol resolve (apply params)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn evaluate
  ; @param (string) source-code
  ; @param (vectors in vector)(opt) env-vars
  ; [[(string) var-name
  ;   (*) var-value]
  ;  [...]]
  ;
  ; @usage
  ; (evaluate "(println (my-function my-var))"
  ;           [["my-function" "my-namespace/my-function"]
  ;            ["my-var"      :my-value]])
  ([source-code]
   (evaluate source-code []))

  ([source-code env-vars]
   ; - The 'load-string' function evaluates the given source-code within the 'clojure.core' namespace.
   ;   Therefore, the given environment variables must be defined with '{:private true}' setting
   ;   to prevent name conflicts in other namespaces (that are use the 'clojure.core' namespace).
   ; - Hot reload tools like Ring 'wrap-reload', redefines constants and functions in watched
   ;   namespaces when the code changes and it would cause name conflicts if the
   ;   'run-code!' function didn't define vars only in a private scope.
   (if (string/nonempty? source-code)
       (letfn [(f0 [environment [var-name var-value]]
                   (str environment "(def ^{:private true} "var-name" "var-value")\n"))]
              (let [environment (reduce f0 nil env-vars)
                    source-code (str environment source-code)]
                   (try (load-string source-code)
                        (catch Exception e (println e))))))))

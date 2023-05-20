(ns applied-science.clerk-helpers
  "Clerk helper fns
  Many adapted from clojure.repl.
  Many used to make the bridge from maria.cloud to Clerk."
  (:require [clojure.repl]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.spec.alpha :as spec]
            [nextjournal.clerk :as clerk]))

(def special-doc-map
  "From the unfortunately private `clojure.repl/special-doc-map`"

  ;;   Copyright (c) Chris Houser, Dec 2008. All rights reserved.
  ;;   The use and distribution terms for this software are covered by the
  ;;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
  ;;   which can be found in the file CPL.TXT at the root of this distribution.
  ;;   By using this software in any fashion, you are agreeing to be bound by
  ;;   the terms of this license.
  ;;   You must not remove this notice, or any other, from this software.
  
  '{. {:url "java_interop#dot"
       :forms [(.instanceMember instance args*)
               (.instanceMember Classname args*)
               (Classname/staticMethod args*)
               Classname/staticField]
       :doc "The instance member form works for both fields and methods.
  They all expand into calls to the dot operator at macroexpansion time."}
    def {:forms [(def symbol doc-string? init?)]
         :doc "Creates and interns a global var with the name
  of symbol in the current namespace (*ns*) or locates such a var if
  it already exists.  If init is supplied, it is evaluated, and the
  root binding of the var is set to the resulting value.  If init is
  not supplied, the root binding of the var is unaffected."}
    do {:forms [(do exprs*)]
        :doc "Evaluates the expressions in order and returns the value of
  the last. If no expressions are supplied, returns nil."}
    if {:forms [(if test then else?)]
        :doc "Evaluates test. If not the singular values nil or false,
  evaluates and yields then, otherwise, evaluates and yields else. If
  else is not supplied it defaults to nil."}
    monitor-enter {:forms [(monitor-enter x)]
                   :doc "Synchronization primitive that should be avoided
  in user code. Use the 'locking' macro."}
    monitor-exit {:forms [(monitor-exit x)]
                  :doc "Synchronization primitive that should be avoided
  in user code. Use the 'locking' macro."}
    new {:forms [(Classname. args*) (new Classname args*)]
         :url "java_interop#new"
         :doc "The args, if any, are evaluated from left to right, and
  passed to the constructor of the class named by Classname. The
  constructed object is returned."}
    quote {:forms [(quote form)]
           :doc "Yields the unevaluated form."}
    recur {:forms [(recur exprs*)]
           :doc "Evaluates the exprs in order, then, in parallel, rebinds
  the bindings of the recursion point to the values of the exprs.
  Execution then jumps back to the recursion point, a loop or fn method."}
    set! {:forms[(set! var-symbol expr)
                 (set! (. instance-expr instanceFieldName-symbol) expr)
                 (set! (. Classname-symbol staticFieldName-symbol) expr)]
          :url "vars#set"
          :doc "Used to set thread-local-bound vars, Java object instance
fields, and Java class static fields."}
    throw {:forms [(throw expr)]
           :doc "The expr is evaluated and thrown, therefore it should
  yield an instance of some derivee of Throwable."}
    try {:forms [(try expr* catch-clause* finally-clause?)]
         :doc "catch-clause => (catch classname name expr*)
  finally-clause => (finally expr*)

  Catches and handles Java exceptions."}
    var {:forms [(var symbol)]
         :doc "The symbol must resolve to a var, and the Var object
itself (not its value) is returned. The reader macro #'x expands to (var x)."}})

(defn- ->meta [m sym]
  (-> m
      (set/rename-keys {:forms :arglists})
      (assoc :name sym
             :special-form true)))

(defn doc-map [sym]
  (some-> (special-doc-map sym)
          (->meta sym)))

(defn what-is* [thing]
  (cond (vector? thing) "a vector: a collection of values, indexable by number",
        (keyword? thing) "a keyword: a special symbolic identifier",
        (boolean? thing) (str "the Boolean value '" thing "'"),
        (var? thing) "a Clojure var",
        (seq? thing) "a sequence: a sequence of values, each followed by the next",
        (set? thing) "a set: a collection of unique values",
        ;; TODO restore Shapes functionality
        ;; FIXME this should be extensible, not hardcoded here
        ;; (instance? applied_science.shapes.Shape thing) "a shape: some geometry that Maria can draw",
        (record? thing) (str "an instance of " (pr-str (type thing)) " (a record)")
        (map? thing) "a map: a collection of key/value pairs, where each key 'maps' to its corresponding value",
        (list? thing) "a list: a sequence, possibly 'lazy'",
        (char? thing) "a character: a unit of writing (letter, emoji, and so on)",
        (nil? thing) "nil: a special value meaning nothing",
        (number? thing) "a number: it can be whole, a decimal, or even a ratio",
        (string? thing) "a string: a run of characters that can make up a text",
        (inst? thing) "an instant: a representation of a point in time",
        (uuid? thing) "a UUID: a universally unique identifier",
        (symbol? thing) "a symbol: a name that usually refers to something",
        (class? thing) "a Java Class"
        (instance? clojure.lang.Atom thing) "a Clojure atom, a way to manage data that can change"
        (instance? Error thing) "an Error, a Java class indicating a serious problem that a reasonable application should not try to catch"
        (instance? Exception thing) "an Exception, a Java class indicating a failure that a reasonable application might want to catch"
        (fn? thing) "a function: something you call with input that returns output"))

(defmacro what-is
  "Returns a string describing what kind of thing `thing` is."
  [thing]
  ;; TODO add links to e.g. https://clojure.org/reference/data_structures#Keywords
  (let [{:keys [special-form macro]} (doc-map thing)]
    (cond special-form "a special form: a primitive which is evaluated in a special way"
          macro "a macro: a function that transforms source code before it is evaluated." ;; XXX not possible in Clojureland
          :else `(what-is* ~thing))))

(comment
  (what-is inc)

  (what-is var)

  (what-is def)

  (what-is if)

  (what-is do)

  (what-is ())

  (what-is '())

  (what-is 'when) ;; FIXME detect macro
  
  ;; XXX not supported in Clojureland
  (what-is what-is)
  
  (what-is when)

  (what-is fn)

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Doc

(defn- special-doc [name-symbol]
  (assoc (or (special-doc-map name-symbol)
             (meta (resolve name-symbol)))
         :name name-symbol
         :ns "clojure.core" ;; HACK --DAL 2023-05-12
         :special-form true))

(defmacro doc*
  "Returns documentation for a var or special form given its name,
   or for a spec if given a keyword.
  Lightly paraphrased from `clojure.repl/doc`."
  [name]
  (if-let [special-name ('{& fn catch try finally try} name)]
    `(#'special-doc '~special-name)
    (cond
      (special-doc-map name) `(#'special-doc '~name)
      (keyword? name) `{:spec '~name :doc '~(spec/describe name)}
      (find-ns name) `(#'namespace-doc (find-ns '~name))
      (resolve name) `(meta (var ~name)))))

(defn cd-encode [s]
  ;; COPIED from clojuredocs.util: https://github.com/zk/clojuredocs/blob/master/src/cljc/clojuredocs/util.cljc
  ;; Copyright © 2010-present Zachary Kim
  ;; Distributed under the Eclipse Public License version 1.0
  (when s
    (cond
      (= "." s) "_."
      (= ".." s) "_.."
      :else (-> s
                (string/replace #"/" "_fs")
                (string/replace #"\\" "_bs")
                (string/replace #"\?" "_q")))))

;; originally from `maria.views.repl-specials`
(defn docs-link [namespace name]
  (when (re-find #"^(cljs|clojure)\.core(\$macros)?$" namespace)
    [:a.f7.black {:href   (str "https://clojuredocs.org/clojure.core/"
                               (cd-encode name))
                  :target "_blank"
                  :rel    "noopener noreferrer"} "clojuredocs ↗"]))

(defmacro doc
  "Clerk documentation string display fn.
  Shows documentation for a var or special form given its name, or for
  a spec if given a keyword."
  [thing]
  `(let [d# (doc* ~thing)]
     (clerk/html
      [:div.bg-slate-50.font-mono.p-4
       [:section.m-2
        (str (:ns d#) "/") [:span.font-bold (:name d#)]]
       [:section.m-2.text-sky-700 (string/join ", " (:arglists d#))]
       [:section.m-2 (:doc d#)] ;; FIXME doesn't preserve line breaks
       [:section.m-2 (docs-link (str (:ns d#))
                                (str (:name d#)))]])))

(comment
  (doc true) ;; TODO handle this more elegantly

  (doc 5) ;; TODO here too
  
  (doc juxt)

  (doc when)

  (doc new)

  (doc do)

  (doc .)

  (doc* do)

  (doc doc)

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Source

(defmacro source
  "Clerk source code display fn.
  Shows the source code for the given symbol, if it can find it.
  This requires that the symbol resolve to a Var defined in a
  namespace for which the .clj is in the classpath.

  Example: (source filter)"
  [n]
  `(clerk/code (or (clojure.repl/source-fn '~n)
                   (str "Source not found"))))

(comment
  clojure.repl/source

  (source filter)

  ;; special forms may not be possible
  (source do)

  (clojure.repl/source do)
  
  )

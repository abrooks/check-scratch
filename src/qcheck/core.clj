(ns qcheck.core
  (:require [clojure.string :as s]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]))

;; Examples
#_{:proj :clojure/core.async :version 1}
#_ [{:proj :clojure/core.async :version 1}
    {:proj :clojure/core.async :version 1}
    {:proj :clojure/core.async :version 2}
    {:proj :clojure/core.async :version 3}]

(def proj-name (gen/elements [:a :b :c]))

(def ver-range [1 2 3])
#_
(def proj-ver (gen/elements (for [M ver-range
                                  m ver-range
                                  i ver-range]
                              (s/join "." [M m i]))))

(def proj-ver (gen/elements [1 2 3]))

(defn do-edit [commit edit]
  (let [{:keys [proj version]} commit]
    {:proj proj
     :version (case edit
                :inc-inc (inc version)
                :dec-inc (dec version)
                :noop-inc version)}))

(def proj-edit (gen/frequency [[80 (gen/return :inc-inc)]
                               [19 (gen/return :noop-inc)]
                               [1 (gen/return :dec-inc)]]))

(def proj-gen (gen/hash-map :proj proj-name :version proj-ver))

(def commit-and-edits-gen (gen/tuple proj-gen (gen/vector proj-edit)))

(defn editor [commit edits]
  (reductions do-edit commit edits))

(def history (gen/fmap (partial apply editor) commit-and-edits-gen))

;; [tree + hints + goal ] --> [new tree + new hints + new goal] --> ...
;; tree <- pushes <- commits <- edits

;; repo is vector of commits
;; commit is map
;; push picks base (and merges) or tip
;; push adds one or more commits
#_
(def new-history
  (gen/fmap (partial apply )))


;; From test.check/doc/intro.md # Recursive Generators
(defn tree
  [size]
  (if (= size 0)
    gen/nat
    (let [new-size (quot size 2)
          smaller-tree (gen/resize new-size (gen/sized tree))]
      (gen/one-of ;; choose either a natural number, or a node
       [gen/nat
        (gen/tuple gen/nat
                   (gen/one-of [(gen/return nil) smaller-tree])
                   (gen/one-of [(gen/return nil) smaller-tree]))]))))

#_
(defn derp
  [size]
  (gen/bind )
  (if (= size 0)
    (gen/return so-far)
    (gen/bind (gen/tuple (gen/return so-far) ))))

(defn- sequence
  "Haskell type:
  Monad m => [m a] -> m [a]

  Specfically used here to turn a list of generators
  into a generator of a list."
  [bind-fn return-fn ms]
  (reduce (fn [acc elem]
            (bind-fn acc
                     (fn [xs]
                       (bind-fn elem
                                (fn [y]
                                  (return-fn (conj xs y)))))))
          (return-fn [])
          ms))

(defn gen-set
  "Create a generator whose elements are chosen from `gen`. The count of the
  set will be bounded by the `size` generator parameter."
  ([generator]
     (gen/gen-bind
      (gen/sized #(gen/choose 0 %))
      (fn [num-elements-rose]
        (gen/gen-bind (sequence gen/gen-bind gen/gen-pure
                            (repeat (gen/rose-root num-elements-rose)
                                    generator))
                  (fn [roses]
                    (gen/gen-pure (gen/shrink-rose (comp clojure.core/set clojure.core/list)
                                           roses)))))))
  ([generator num-elements]
     (gen-set generator num-elements num-elements))
  ([generator min-elements max-elements]
     (gen/gen-bind
      (gen/choose min-elements max-elements)
      (fn [num-elements-rose]
        (gen/gen-bind (sequence gen/gen-bind gen/gen-pure
                            (repeat (gen/rose-root num-elements-rose)
                                    generator))
                  (fn [roses]
                    (gen/gen-bind
                     (gen/gen-pure (gen/shrink-rose (comp clojure.core/set clojure.core/list)
                                            roses))
                     (fn [rose]
                       (prn :food (first (gen/rose-seq rose)))
                       (gen/gen-pure (gen/rose-filter (fn [v]
                                                        (println "hello")
                                                        (prn :v v)
                                                        (and (>= (count (set v)) min-elements)
                                                             (<= (count (set v)) max-elements)))
                                                      rose))))))))))

(comment

  (gen/sample proj-gen)
  (gen/sample proj-gen 50)

  (def init-commit {:proj :foo :version 1})
  (def my-edits (gen/sample proj-edit 5))
  (editor init-commit my-edits)

  (last (gen/sample commit-and-edits-gen))
  (last (gen/sample history))

  (gen/sample (gen/fmap #(assoc % :name :reid) proj-gen))

  ;; Reid's 'gen/bind example from his presentation
  (clojure.pprint/pprint
   (gen/sample (gen/bind
                (gen/not-empty (gen/vector (gen/set  gen/int)))
                #(gen/tuple (gen/return %) (gen/elements %)))))

  (gen/sample (gen/frequency [[300 (gen/return 1)]
                              [100 (gen/return 2)]
                              [2 (gen/return 3)]
                              [1 (gen/return 0)]]))

  )




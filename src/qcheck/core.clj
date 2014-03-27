(ns qcheck.core
  (:require [clojure.string :as s]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]))

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


(comment


  (gen/sample proj-gen)
  (gen/sample proj-gen 50)

  (def init-commit {:proj :foo :version 1})
  (def my-edits (gen/sample proj-edit 5))
  (editor init-commit my-edits)

  (last (gen/sample commit-and-edits-gen))
  (last (gen/sample history))

  (gen/sample (gen/fmap #(assoc % :name :reid) proj-gen))

  )




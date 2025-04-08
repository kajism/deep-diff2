(ns lambdaisland.deep-diff2.minimize-impl
  "Provide API for manipulate the diff structure data "
  (:require [clojure.walk :refer [prewalk]]
            #?(:clj  [lambdaisland.deep-diff2.diff-impl]
               :cljs [lambdaisland.deep-diff2.diff-impl :refer [Mismatch Deletion Insertion]]))
  #?(:clj (:import [lambdaisland.deep_diff2.diff_impl Mismatch Deletion Insertion])))

(defn diff-item?
  "Checks if x is a Mismatch, Deletion, or Insertion"
  [x]
  (or (instance? Mismatch x)
      (instance? Deletion x)
      (instance? Insertion x)))

(defn has-diff-item?
  "Checks if there are any diff items in x or sub-tree of x"
  [x]
  (or (diff-item? x)
      (and (map? x) (some #(or (has-diff-item? (key %))
                               (has-diff-item? (val %))) x))
      (and (coll? x) (some has-diff-item? x))))

(defn minimize
  "Recursive version of minimize showing full values of added/removed map keys"
  [x]
  (let [y (cond
            (diff-item? x)
            x

            (map-entry? x)
            (cond
              (diff-item? (key x))
              x

              (has-diff-item? (val x))
              [(key x) (minimize (val x))])

            (map? x)
            (->> x
                 (keep #(minimize %))
                 (into {}))

            (coll? x)
            (into (empty x) (keep #(minimize %) x))

            :else
            x)]
    (cond
      (coll? y) y
      :else nil)))

(comment
  (minimize {(Insertion. :b) [1 2]})
  )

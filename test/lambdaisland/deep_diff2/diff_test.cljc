(ns lambdaisland.deep-diff2.diff-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [lambdaisland.deep-diff2.diff-impl :as diff]))

(defrecord ARecord [])

(deftest diff-test
  (testing "diffing atoms"
    (testing "nil"
      (is (= (diff/->Mismatch nil 1)
             (diff/diff nil 1))))

    (testing "when different"
      (is (= (diff/->Mismatch :a :b)
             (diff/diff :a :b))))

    (testing "when equal"
      (is (= :a
             (diff/diff :a :a)))))

  (testing "diffing collections"
    (testing "different types"
      (is (= (diff/->Mismatch [1 2 3] #{1 2 3})
             (diff/diff [1 2 3] #{1 2 3}))))

    (testing "sequences"
      (is (= []
             (diff/diff [] [])))

      (is (= [1 2 3]
             (diff/diff (into-array [1 2 3]) [1 2 3])))

      (is (= [:a]
             (diff/diff [:a] [:a])))

      (is (= [:a (diff/->Deletion :b) :c (diff/->Insertion :d)]
             (diff/diff [:a :b :c] [:a :c :d])))

      (is (= [:a (diff/->Deletion :b) :c (diff/->Insertion :d)]
             (diff/diff (list :a :b :c) (list :a :c :d))))

      (is (= [(diff/->Insertion :a)]
             (diff/diff [] [:a])))

      (is (= [(diff/->Deletion :a)]
             (diff/diff [:a] [])))

      (is (= [(diff/->Insertion :x) (diff/->Insertion :y) :a]
             (diff/diff [:a] [:x :y :a])))

      (is (= [:a (diff/->Mismatch :b :x) (diff/->Insertion :y) :c]
             (diff/diff [:a :b :c] [:a :x :y :c])))

      (is (= [{:x (diff/->Mismatch 1 2)}]
             (diff/diff [{:x 1}] [{:x 2}])))

      (is (= [(diff/->Insertion []) {} (diff/->Insertion [])]
             (diff/diff [{}]  [[] {} []])))

      (is (= [0 (diff/->Deletion 1) (diff/->Mismatch 2 :x) (diff/->Insertion :y) (diff/->Insertion :z)]
             (diff/diff [0 1 2] [0 :x :y :z]))))

    (testing "sets"
      (is (= #{:a}
             (diff/diff #{:a} #{:a})))

      (is (= #{(diff/->Insertion :a)}
             (diff/diff #{} #{:a})))

      (is (= #{(diff/->Deletion :a)}
             (diff/diff #{:a} #{})))

      (is (= #{(diff/->Deletion :a) :b :c}
             (diff/diff #{:a :b :c} #{:c :b}))))

    (testing "maps"
      (is (= {} (diff/diff {} {})))

      (is (= {:a (diff/->Mismatch 1 2)}
             (diff/diff {:a 1} {:a 2})))

      (is (= {:a (diff/->Mismatch 1 2)
              (diff/->Deletion :b) 2
              (diff/->Insertion :x) 2
              :c 3}
             (diff/diff {:a 1 :b 2 :c 3} {:a 2 :x 2 :c 3})))

      (is (= {:a [1 (diff/->Deletion 2) 3]}
             (diff/diff {:a [1 2 3]} {:a [1 3]}))))

    (testing "map key order doesn't impact diff result"
      (is (= {:name (diff/->Mismatch "Alyysa P Hacker" "Alyssa P Hacker"), :age 40}

             (diff/diff (array-map :name "Alyysa P Hacker" :age 40)
                        (array-map :age 40 :name "Alyssa P Hacker"))

             (diff/diff (array-map :age 40 :name "Alyysa P Hacker")
                        (array-map :age 40 :name "Alyssa P Hacker")))))

    (testing "records"
      (is (= {:a (diff/->Mismatch 1 2)}
             (diff/diff (map->ARecord {:a 1}) (map->ARecord {:a 2}))))
      (is (= {(diff/->Insertion :a) 1}
             (diff/diff (map->ARecord {}) (map->ARecord {:a 1}))))
      (is (= {(diff/->Deletion :a) 1}
             (diff/diff (map->ARecord {:a 1}) (map->ARecord {}))))))

  (is (= [{:x (diff/->Mismatch 1 2)}]
         (diff/diff [{:x 1}] [{:x 2}])))

  (is (= [(diff/->Mismatch 0 [[]]) (diff/->Mismatch 0 [])]
         (diff/diff [0 0] [[[]] []])))

  (is (= [(diff/->Mismatch 0 []) (diff/->Deletion 0)]
         (diff/diff [0 0] [[]])))

  (is (= [(diff/->Mismatch 0 false)]
         (diff/diff [0] [false]))))

(deftest undiff-test
  (is (= {:a [1 2 [9 8 7]] :b 2 :c 3}
         (diff/left-undiff
          (diff/diff {:a [1 2 [9 8 7]] :b 2 :c 3}
                     {:a [1 3 [9 8]] :x 2 :c 3}))))

  (is (= [-1 1 true 4 5]
         (diff/right-undiff
          (diff/diff-seq [true 0] [-1 1 true 4 5]))))

  (is (= [-1 1 true]
         (diff/right-undiff
          (diff/diff-seq [true 0] [-1 1 true])))))

(deftest replacements-test
  (is (= [{} #{} {}]
         (diff/replacements [#{} {}])))

  (is (= [{1 :x} #{} {}]
         (diff/replacements [#{1} {1 [:x]}])))

  (is (= [{2 :x} #{} {}]
         (diff/replacements [#{2} {1 [:x]}])))

  (is (= [#{1 2} {2 [:x :y :z]}]
         (diff/del+ins [0 1 2] [0 :x :y :z])))

  (is (= [{2 :x} #{1} {2 '(:y :z)}]
         (diff/replacements [#{1 2} {2 [:x :y :z]}])))

  (is (= [{} #{} {-1 [1], 1 [3], 3 [5]}]
         (diff/replacements (diff/del+ins [2 4]  [1 2 3 4 5]))))

  (is (= [{1 4} #{} {-1 [-1 1], 3 [5]}]
         (diff/replacements
          (diff/del+ins [true 0] [-1 1 true 4 5]))))

  (is (= [{} #{1} {-1 [-1 1]}]
         (diff/replacements
          (diff/del+ins [true 0] [-1 1 true]))))

  (is (= [{} #{1} {-1 [-1 1]}]
         (diff/replacements
          (diff/del+ins [true 0] [-1 1 true]))))

  (is (= [{} #{1} {2 [:d]}]
         (-> (diff/del+ins [:a :b :c] [:a :c :d])
             (diff/replacements))))

  (is (= [{0 [[]], 1 []} #{} {}]
         (-> (diff/del+ins [0 0] [[[]] []])
             diff/replacements)))

  (is (= [{0 :x, 1 :y} #{} {}]
         (-> (diff/del+ins [1 2] [:x :y])
             diff/replacements)))

  (is (= [{1 :x} #{} {1 [:y]}]
         (-> (diff/del+ins [1 2] [1 :x :y])
             diff/replacements)))

  (is (= [{0 []} #{1} {}]
         (diff/replacements (diff/del+ins [0 0] [[]]))))

  (is (= [{0 false} #{} {}]
         (diff/replacements (diff/del+ins [0] [false]))))

  (is (= [{0 {:x 2}} #{} {}]
         (diff/replacements (diff/del+ins [{:x 1}] [{:x 2}]))))

  (is (= [{1 {:x 2}} #{} {}]
         (diff/replacements (diff/del+ins [:a {:x 1}] [:a {:x 2}])))))

(deftest del+ins-test
  (is (= [#{} {-1 [1], 0 [3], 1 [5]}]
         (diff/del+ins [2 4]  [1 2 3 4 5])))

  (is (= [#{} {-1 [1], 0 [3]}]
         (diff/del+ins [2]  [1 2 3])))

  (is (= [#{1} {-1 [-1 1]}]
         (diff/del+ins [true 0] [-1 1 true])))

  (is (= [#{0 1} {-1 [[]]}]
         (diff/del+ins [0 0] [[]]))))

;; (not= ##NaN ##NaN), which messes up test results
;; https://stackoverflow.com/questions/16983955/check-for-nan-in-clojurescript
(defn NaN? [node]
;; Need to confirm that it's a Double first.
  #?(:clj (and (instance? Double node) (Double/isNaN node))
     :cljs
     (and (= (.call js/toString node) (str "[object Number]"))
          (js/eval (str node " != +" node )))))

(def gen-any-except-NaN (gen/recursive-gen
                         gen/container-type
                         (gen/such-that (complement NaN?) gen/simple-type)))

(defspec round-trip-diff 100
  (prop/for-all
   [x gen-any-except-NaN
    y gen-any-except-NaN]
   (let [diff (diff/diff x y)]
     (= [x y] [(diff/left-undiff diff) (diff/right-undiff diff)]))))

(defspec diff-same-is-same 100
  (prop/for-all
   [x gen-any-except-NaN]
   (= x (diff/diff x x))))

(deftest diff-seq-test
  (is (= [(diff/->Insertion 1) 2 (diff/->Insertion 3)]
         (diff/diff-seq [2]  [1 2 3])))

  (is (= [(diff/->Insertion 1) 2 (diff/->Insertion 3) 4 (diff/->Insertion 5)]
         (diff/diff-seq [2 4]  [1 2 3 4 5])))

  (is (= [(diff/->Insertion -1) (diff/->Insertion 1) true (diff/->Deletion 0)]
         (diff/diff-seq [true 0] [-1 1 true])))

  (is (= [1 (diff/->Mismatch 2 3) 3 4]
         (diff/diff-seq-replacements {1 3} [1 2 3 4])))

  (is (= [1 (diff/->Deletion 2) 3 4]
         (diff/diff-seq-deletions #{1} [1 2 3 4])))

  (is (= [1 (diff/->Mismatch 2 :x) (diff/->Insertion :y)]
         (diff/diff-seq [1 2] [1 :x :y])))

  (is (= [(diff/->Mismatch 0 false)]
         (->> [0]
              (diff/diff-seq-replacements {0 false}))))

  (is (= [(diff/->Insertion :y) (diff/->Mismatch 1 :x) (diff/->Deletion 2)]
         (->> [1 2]
              (diff/diff-seq-replacements {0 :x})
              (diff/diff-seq-deletions #{1})
              (diff/diff-seq-insertions {-1 [:y]}))))

  (is (= [1 2 (diff/->Insertion :x) (diff/->Insertion :y) 3 4]
         (diff/diff-seq-insertions {1 [:x :y]} [1 2 3 4])))

  (is (= [(diff/->Insertion -1)
          (diff/->Insertion 1)
          true
          (diff/->Mismatch 0 4)
          (diff/->Insertion 5)]
         (->> [true 0]
              (diff/diff-seq-replacements {1 4})
              (diff/diff-seq-deletions #{})
              (diff/diff-seq-insertions {-1 [-1 1] 3 [5]}))))


  (is (= [(diff/->Insertion -1)
          (diff/->Insertion 1)
          true
          (diff/->Deletion 0)]
         (diff/diff-seq [true 0] [-1 1 true])))


  (is (= [:a (diff/->Deletion :b) :c (diff/->Insertion :d)]
         (diff/diff-seq [:a :b :c] [:a :c :d]))))


(comment
  (use 'kaocha.repl)
  (run)

  (defmethod clojure.core/print-method lambdaisland.deep-diff2.diff.Insertion [v writer]
    (.write writer (pr-str `(diff/->Insertion ~(:+ v)))))

  (defmethod clojure.core/print-method lambdaisland.deep-diff2.diff.Deletion [v writer]
    (.write writer (pr-str `(diff/->Deletion ~(:- v)))))

  (defmethod clojure.core/print-method lambdaisland.deep-diff2.diff.Mismatch [v writer]
    (.write writer (pr-str `(diff/->Mismatch ~(:- v) ~(:+ v))))))

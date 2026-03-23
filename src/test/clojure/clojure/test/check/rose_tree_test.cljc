;   Copyright (c) Rich Hickey, Reid Draper, and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test.check.rose-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.walk :as walk]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.clojure-test :as ct :refer [defspec]]))

(defn depth-one-children
  [rose]
  (into [] (map rose/root (rose/children rose))))

(defn depth-one-and-two-children
  [rose]
  (let [the-children (rose/children rose)]
    (into []
          (concat
           (map rose/root the-children)
           (map rose/root (mapcat rose/children the-children))))))

(defspec test-collapse-rose
  100
  (prop/for-all [i gen/small-integer]
    (let [tree (#'gen/int-rose-tree i)]
      (= (depth-one-and-two-children tree)
         (depth-one-children (rose/collapse tree))))))

(defrecord RoseTreeComparable [root children])

(defn make-rose-comparable [root children]
  (RoseTreeComparable. root children))

(defn RoseTree->RoseTreeComparable [rose]
  (if (instance? clojure.test.check.rose_tree.RoseTree rose)
    (->RoseTreeComparable (RoseTree->RoseTreeComparable (rose/root rose))
                          (mapv RoseTree->RoseTreeComparable (rose/children rose)))
    (walk/postwalk (fn [x]
                     (cond-> x
                       (instance? clojure.test.check.rose_tree.RoseTree x) RoseTree->RoseTreeComparable))
                   rose)))

(defn =-rose-tree [r1 r2]
  (= (RoseTree->RoseTreeComparable r1)
     (RoseTree->RoseTreeComparable r2)))

(deftest join-test
  (is (=-rose-tree (rose/make-rose 42 ())
                   (rose/join
                     (rose/make-rose (rose/make-rose 42 []) ()))))
  (is (not (=-rose-tree (rose/make-rose 43 ())
                        (rose/join
                          (rose/make-rose (rose/make-rose 42 []) ()))))))

(defn test-size [x]
  (let [v (volatile! 0)]
    (walk/postwalk (fn [x]
                     (vswap! v inc)
                     x)
                   x)
    @v))

(def this-ns (ns-name *ns*))
(deftest join-examples-from-test-suite-test
  (binding [*ns* (the-ns this-ns)]
    (doseq [assertion (read-string (str "[" (slurp "joins.txt") "]"))
            :when (< (test-size assertion) 5000)]
      #_(prn assertion)
      (eval assertion)
      )))

(deftest permutations-test
  (is (=-rose-tree (rose/permutations [(rose/make-rose 0 [])]) []))
  (is
    (=-rose-tree
      (rose/permutations [(rose/make-rose 1 [(rose/make-rose 0 [])])])
      [[(rose/make-rose 0 [])]]))

  (is
    (=-rose-tree
      (rose/permutations
        [(rose/make-rose
           2
           [(rose/make-rose 0 [])
            (rose/make-rose 1 [(rose/make-rose 0 [])])])])
      [[(rose/make-rose 0 [])]
       [(rose/make-rose 1 [(rose/make-rose 0 [])])]])))

(deftest permutations-examples-from-test-suite-test
  (binding [*ns* (the-ns this-ns)]
    (doseq [assertion (read-string (str "[" (slurp "permutations.txt") "]"))
            :when (< (test-size assertion) 5000)]
      #_(prn assertion)
      (eval assertion)
      )))

(deftest collapse-test
  (is
    (=-rose-tree
      (rose/collapse (rose/make-rose 0 []))
      (rose/make-rose 0 [])))
  (is
    (=-rose-tree
      (rose/collapse
        (rose/make-rose
          2
          [(rose/make-rose 0 [])
           (rose/make-rose 1 [(rose/make-rose 0 [])])]))
      (rose/make-rose
        2
        [(rose/make-rose 0 [])
         (rose/make-rose 1 [(rose/make-rose 0 [])])
         (rose/make-rose 0 [])])))
  (is
    (=-rose-tree
      (rose/collapse
        (rose/make-rose
          3
          [(rose/make-rose 0 [])
           (rose/make-rose
             2
             [(rose/make-rose 0 [])
              (rose/make-rose 1 [(rose/make-rose 0 [])])])]))
      (rose/make-rose
        3
        [(rose/make-rose 0 [])
         (rose/make-rose
           2
           [(rose/make-rose 0 [])
            (rose/make-rose 1 [(rose/make-rose 0 [])])
            (rose/make-rose 0 [])])
         (rose/make-rose 0 [])
         (rose/make-rose 1 [(rose/make-rose 0 [])])])))
  )

(deftest collapse-examples-from-test-suite-test
  (binding [*ns* (the-ns this-ns)]
    (doseq [assertion (read-string (str "[" (slurp "collapse.txt") "]"))
            :when (< (test-size assertion) 5000)]
      #_(prn assertion)
      (eval assertion)
      )))

(deftest pure-test
  (is (=-rose-tree (rose/make-rose 42 [])
                   (rose/pure 42))))

(deftest fmap-test
  (is (=-rose-tree (rose/make-rose 42 [(rose/make-rose 43 [])])
                   (rose/fmap inc (rose/make-rose 41 [(rose/make-rose 42 [])])))))

(deftest bind-test
  (is (=-rose-tree (rose/make-rose 43 ())
                   (rose/bind (rose/make-rose (rose/make-rose 42 []) ())
                              #(rose/make-rose (inc (rose/root %)) (rose/children %))))))

(deftest seq-test
  (is (=-rose-tree (rose/seq (rose/make-rose 1 [])) [1]))
  (is
    (=-rose-tree
      (rose/seq (rose/make-rose 2 [(rose/make-rose 1 [])]))
      [2 1]))
  (is
    (=-rose-tree
      (rose/seq
        (rose/make-rose
          -2
          [(rose/make-rose 0 [])
           (rose/make-rose -1 [(rose/make-rose 0 [])])]))
      [-2 0 -1])))

(deftest rose-seq-examples-from-test-suite-test
  (binding [*ns* (the-ns this-ns)]
    (doseq [assertion (read-string (str "[" (slurp "rose-seq.txt") "]"))
            :when (< (test-size assertion) 100)]
      #_(prn assertion)
      (eval assertion)
      )))

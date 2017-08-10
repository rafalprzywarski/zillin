(ns zillin.math-test
  (:require [clojure.test :refer :all]
            [zillin.math :refer :all]))


(deftest vec3-test
  (testing "addition"
    (is (= (add (vec3 10 20 30) (vec3 3 2 1)) (vec3 13 22 31)))
    (is (= (add (vec3 1.5 2.25 3.125) (vec3 7 8 9)) (vec3 8.5 10.25 12.125))))

  (testing "subtruction"
    (is (= (sub (vec3 10 20 30) (vec3 3 2 1)) (vec3 7 18 29)))
    (is (= (sub (vec3 1.5 2.25 3.125) (vec3 7 8 9)) (vec3 -5.5 -5.75 -5.875))))

  (testing "dot product"
    (is (= (dot (vec3 1 0 0) (vec3 3 4 5)) 3.0))
    (is (= (dot (vec3 0 1 0) (vec3 3 4 5)) 4.0))
    (is (= (dot (vec3 0 0 1) (vec3 3 4 5)) 5.0))
    (is (= (dot (vec3 1.5 2.5 3.5) (vec3 3 4 5)) (+ (* 1.5 3) (* 2.5 4) (* 3.5 5)))))

  (testing "cross product"
    (is (= (cross (vec3 1 0 0) (vec3 0 1 0)) (vec3 0 0 1)))
    (is (= (cross (vec3 0 1 0) (vec3 1 0 0)) (vec3 0 0 -1)))
    (is (= (cross (vec3 1 0 0) (vec3 0 0 1)) (vec3 0 -1 0)))
    (is (= (cross (vec3 0 0 1) (vec3 1 0 0)) (vec3 0 1 0)))
    (is (=
         (cross (vec3 3 4 5) (vec3 -2.5 1.5 3.5))
         (vec3 (- (* 4 3.5) (* 5 1.5)) (- (* 5 -2.5) (* 3 3.5)) (- (* 3 1.5) (* 4 -2.5)))))))

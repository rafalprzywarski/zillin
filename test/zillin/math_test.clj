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
         (vec3 (- (* 4 3.5) (* 5 1.5)) (- (* 5 -2.5) (* 3 3.5)) (- (* 3 1.5) (* 4 -2.5))))))
  (testing "length"
    (is (= 1.0 (length (vec3 1 0 0))))
    (is (= 3.0 (length (vec3 3 0 0))))
    (is (= 5.0 (length (vec3 0 3 4))))
    (is (= 5.0 (length (vec3 -3 0 4)))))
  (testing "normalisation"
    (is (= (vec3 1 0 0) (normalize (vec3 1 0 0))))
    (is (= (vec3 1 0 0) (normalize (vec3 3 0 0))))
    (is (= (vec3 0 (/ 3.0 5) (/ 4.0 5)) (normalize (vec3 0 3 4))))
    (is (= (vec3 (/ -3.0 5) 0 (/ 4.0 5)) (normalize (vec3 -3 0 4))))))

(deftest mat4-test
  (testing "multiplication"
    (is (= (mmult
            (mat4 1 2 3 4
                  5 6 7 8
                  9 0 1 2
                  3 4 5 6)
            (mat4 1 0 0 0
                  0 1 0 0
                  0 0 1 0
                  0 0 0 1))
           (mat4 1 2 3 4
                 5 6 7 8
                 9 0 1 2
                 3 4 5 6)))
    (is (= (mmult
            (mat4 1 2 3 4
                  5 6 7 8
                  9 0 1 2
                  3 4 5 6)
            (mat4 4 3 2 1
                  8 7 6 5
                  2 1 0 9
                  6 5 4 3))
           (mat4 50  40  30 50
                 130 104 78 122
                 50  38  26 24
                 90  72  54 86))))
  (testing "matrix vector multiplication"
    (is (= (mvmult (mat4 1 0 0 0
                         0 1 0 0
                         0 0 1 0
                         0 0 0 1)
                   (vec3 3 4 5))
           (vec3 3 4 5)))
    (is (= (mvmult (mat4 1 2 3 4
                         5 6 7 8
                         9 0 1 2
                         3 4 5 6)
                   (vec3 3 4 5))
           (vec3 (/ 30.0 56) (/ 82.0 56) (/ 34.0 56))))))

(deftest transform-test
  (testing "translation"
    (is (= (vec3 37 58 79)
           (mvmult (translation (vec3 30 50 70)) (vec3 7 8 9)))))
  (testing "rotation"
    (let [eps (- (Math/nextUp 1.0) 1.0)]
      (is (>= (* 5 eps)
              (length (sub (vec3 -3 5 4)
                           (mvmult (rotation (vec3 0 0 10) Math/PI) (vec3 3 -5 4))))))
      (is (>= (* 5 eps)
              (length (sub (vec3 5 3 4)
                           (mvmult (rotation (vec3 0 0 10) (* 0.5 Math/PI)) (vec3 3 -5 4))))))
      (is (>= (* 5 eps)
              (length (sub (vec3 -5 -3 4)
                           (mvmult (rotation (vec3 0 0 10) (* -0.5 Math/PI)) (vec3 3 -5 4))))))
      (is (>= (* 10 eps)
              (length (sub (vec3 4 3 -5)
                           (mvmult (rotation (vec3 5 5 5) (/ Math/PI 1.5)) (vec3 3 -5 4))))))
      (is (>= (* 16 eps)
              (length (sub (vec3 -5 4 3)
                           (mvmult (rotation (vec3 5 5 5) (/ Math/PI 0.75)) (vec3 3 -5 4))))))))
  (testing "perspective projection"
    (let [proj (perspective-projection 1.0 32 24)
          examples [
                    [[0 0 7] [16 12 (/ 7.0)]]
                    [[0 0 16] [16 12 (/ 16.0)]]
                    [[12 12 12] [28 24 (/ 12.0)]]
                    [[-12 12 12] [4 24 (/ 12.0)]]
                    [[-12 -12 12] [4 0 (/ 12.0)]]
                    [[12 -12 12] [28 0 (/ 12.0)]]
                    [[12 12 24] [22 18 (/ 24.0)]]
                    [[-12 12 24] [10 18 (/ 24.0)]]
                    [[-12 -12 24] [10 6 (/ 24.0)]]
                    [[12 -12 24] [22 6 (/ 24.0)]]]]
      (doseq [[source projected] examples]
        (is (= (apply vec3 projected)
               (mvmult proj (apply vec3 source))))))
    (let [proj (perspective-projection 0.5 16 8)]
      (is (= (vec3 11.5 6.5 (/ 16.0))
             (mvmult proj (vec3 7 5 16)))))))

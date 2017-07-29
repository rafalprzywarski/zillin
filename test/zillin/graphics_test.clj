(ns zillin.graphics-test
    (:require [clojure.test :refer :all]
              [zillin.graphics :refer :all]))

(deftest framebuffer-test
    (testing "creation"
        (let [f (create-framebuffer 16 32 3)]
            (is (= 16 (.width f)))
            (is (= 32 (.height f)))
            (is (= 3 (.components f)))))
    (testing "component access"
        (let [f (create-framebuffer 8 16 3)]
            (is (= 0.0 (get-component f 0 0 0)))
            (is (= 0.0 (get-component f 7 3 1)))
            (is (= 0.0 (get-component f 7 15 1)))
            (set-component f 0 0 0 0.75)
            (set-component f 0 0 1 -0.75)
            (set-component f 0 0 2 -0.25)
            (set-component f 7 0 0 1.75)
            (set-component f 7 0 1 -1.75)
            (set-component f 7 0 2 -1.25)
            (set-component f 0 15 0 2.75)
            (set-component f 0 15 1 -2.75)
            (set-component f 0 15 2 -2.25)
            (set-component f 7 15 0 3.75)
            (set-component f 7 15 1 -3.75)
            (set-component f 7 15 2 -3.25)
            (is (= 0.75 (get-component f 0 0 0)))
            (is (= -0.75 (get-component f 0 0 1)))
            (is (= -0.25 (get-component f 0 0 2)))
            (is (= 1.75 (get-component f 7 0 0)))
            (is (= -1.75 (get-component f 7 0 1)))
            (is (= -1.25 (get-component f 7 0 2)))
            (is (= 2.75 (get-component f 0 15 0)))
            (is (= -2.75 (get-component f 0 15 1)))
            (is (= -2.25 (get-component f 0 15 2)))
            (is (= 3.75 (get-component f 7 15 0)))
            (is (= -3.75 (get-component f 7 15 1)))
            (is (= -3.25 (get-component f 7 15 2))))))

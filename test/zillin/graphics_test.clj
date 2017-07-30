(ns zillin.graphics-test
    (:require [clojure.test :refer :all]
              [zillin.graphics :refer :all]))

(deftest framebuffer-test
    (testing "creation"
        (let [fb (create-framebuffer 16 32 3)]
            (is (= 16 (.width fb)))
            (is (= 32 (.height fb)))
            (is (= 3 (.components fb)))))
    (testing "component access"
        (let [fb (create-framebuffer 8 16 3)]
            (is (= 0.0 (get-component fb 0 0 0)))
            (is (= 0.0 (get-component fb 7 3 1)))
            (is (= 0.0 (get-component fb 7 15 1)))
            (set-component! fb 0 0 0 0.75)
            (set-component! fb 0 0 1 -0.75)
            (set-component! fb 0 0 2 -0.25)
            (set-component! fb 7 0 0 1.75)
            (set-component! fb 7 0 1 -1.75)
            (set-component! fb 7 0 2 -1.25)
            (set-component! fb 0 15 0 2.75)
            (set-component! fb 0 15 1 -2.75)
            (set-component! fb 0 15 2 -2.25)
            (set-component! fb 7 15 0 3.75)
            (set-component! fb 7 15 1 -3.75)
            (set-component! fb 7 15 2 -3.25)
            (is (= 0.75 (get-component fb 0 0 0)))
            (is (= -0.75 (get-component fb 0 0 1)))
            (is (= -0.25 (get-component fb 0 0 2)))
            (is (= 1.75 (get-component fb 7 0 0)))
            (is (= -1.75 (get-component fb 7 0 1)))
            (is (= -1.25 (get-component fb 7 0 2)))
            (is (= 2.75 (get-component fb 0 15 0)))
            (is (= -2.75 (get-component fb 0 15 1)))
            (is (= -2.25 (get-component fb 0 15 2)))
            (is (= 3.75 (get-component fb 7 15 0)))
            (is (= -3.75 (get-component fb 7 15 1)))
            (is (= -3.25 (get-component fb 7 15 2))))))

(defn- check-rasterize-triangle [[[x1 y1] [x2 y2] [x3 y3]] [w h] expected]
    (let [fb (create-framebuffer w h 1)
          shader (fn [l1 l2 l3] (+ l1 l2 l3))]
        (rasterize-triangle! fb shader x1 y1 x2 y2 x3 y3)
        (is (= (map double expected) (seq (.pixels fb))))))

(deftest rasterize-triangle-test
    (testing "fill rules"
        (check-rasterize-triangle [[1 1] [5 1] [1 5]] [6 6]
            [0 0 0 0 0 0
             0 1 1 1 0 0
             0 1 1 0 0 0
             0 1 0 0 0 0
             0 0 0 0 0 0
             0 0 0 0 0 0])
        (check-rasterize-triangle [[1 1] [6 2] [2 4]] [7 5]
            [0 0 0 0 0 0 0
             0 1 1 0 0 0 0
             0 1 1 1 1 0 0
             0 0 1 0 0 0 0
             0 0 0 0 0 0 0])
        (check-rasterize-triangle [[0.5 1.5] [1.5 0.5] [1.5 1.5]] [2 2]
            [0 0
             0 0])
        (check-rasterize-triangle [[0.25 1.25] [1.25 0.25] [1.25 1.25]] [2 2]
            [0 0
             0 0])
        (check-rasterize-triangle [[0.5 0.5] [2.5 0.5] [0.5 2.5]] [3 3]
            [1 1 0
             1 0 0
             0 0 0])
        (check-rasterize-triangle [[0.5 2.5] [2.5 0.5] [2.5 2.5]] [3 3]
            [0 0 0
             0 1 0
             0 0 0])
        (check-rasterize-triangle [[0.5 0.5] [2.5 0.5] [0.5 2.5]] [3 3]
            [1 1 0
             1 0 0
             0 0 0])
        (check-rasterize-triangle [[0 3] [6 1] [4 3]] [6 4]
            [0 0 0 0 0 0
             0 0 0 0 1 0
             0 1 1 1 0 0
             0 0 0 0 0 0])
        (check-rasterize-triangle [[1 3] [3 1] [4 4]] [5 5]
            [0 0 0 0 0
             0 0 1 0 0
             0 1 1 0 0
             0 0 1 1 0
             0 0 0 0 0])
        (check-rasterize-triangle [[1 1] [3.5 2.5] [2 4]] [4 4]
            [0 0 0 0
             0 1 0 0
             0 1 1 0
             0 0 0 0])
        (check-rasterize-triangle [[0.75 2.5] [2.75 0.75] [4.75 2.5]] [5 3]
            [0 0 0 0 0
             0 0 1 1 0
             0 0 0 0 0])
        (check-rasterize-triangle [[0.75 1.5] [4.75 1.5] [2.5 4.25]] [6 5]
            [0 0 0 0 0 0
             0 1 1 1 1 0
             0 1 1 1 0 0
             0 0 1 0 0 0
             0 0 0 0 0 0])
        (check-rasterize-triangle [[1.5 2.5] [3 1] [2.5 3.5]] [4 4]
            [0 0 0 0
             0 0 1 0
             0 1 1 0
             0 0 0 0]))
    (testing "clipping"
        (testing "left"
            (check-rasterize-triangle [[-1.25 1.5] [2.75 1.5] [0.5 4.25]] [4 5]
                [0 0 0 0
                 1 1 1 0
                 1 1 0 0
                 1 0 0 0
                 0 0 0 0]))
        (testing "right"
            (check-rasterize-triangle [[0.75 1.5] [4.75 1.5] [2.5 4.25]] [3 5]
                [0 0 0
                 0 1 1
                 0 1 1
                 0 0 1
                 0 0 0]))
        (testing "top"
            (check-rasterize-triangle [[0.75 1.5] [4.75 1.5] [2.5 4.25]] [6 2]
                [0 0 0 0 0 0
                 0 1 1 1 1 0]))
        (testing "bottom"
            (check-rasterize-triangle [[0.75 -0.5] [4.75 -0.5] [2.5 2.25]] [6 3]
                [0 1 1 1 0 0
                 0 0 1 0 0 0
                 0 0 0 0 0 0])))
    (testing "back-face culling"
        (check-rasterize-triangle [[0.75 1.5] [2.5 4.25] [4.75 1.5]] [6 5]
            [0 0 0 0 0 0
             0 0 0 0 0 0
             0 0 0 0 0 0
             0 0 0 0 0 0
             0 0 0 0 0 0]))
    (testing "degenerate"
        (check-rasterize-triangle [[0.75 0.75] [0.75 0.75] [2.75 1.5]] [3 3]
            [0 0 0
             0 0 0
             0 0 0])))

(deftest pixel-shading
    (testing "barycentric coordinates"
        (let [fb (create-framebuffer 4 4 1)]
            (rasterize-triangle! fb (fn [l1 l2 l3] l1) 4 4 0 4 4 0)
            (is (= [0.00 0.00 0.00 0.00
                    0.00 0.00 0.00 0.25
                    0.00 0.00 0.25 0.50
                    0.00 0.25 0.50 0.75]
                   (seq (.pixels fb))))
            (rasterize-triangle! fb (fn [l1 l2 l3] l2) 4 4 0 4 4 0)
            (is (= [0.000 0.000 0.000 0.125
                    0.000 0.000 0.375 0.125
                    0.000 0.625 0.375 0.125
                    0.875 0.625 0.375 0.125]
                   (seq (.pixels fb))))
            (rasterize-triangle! fb (fn [l1 l2 l3] l3) 4 4 0 4 4 0)
            (is (= [0.000 0.000 0.000 0.875
                    0.000 0.000 0.625 0.625
                    0.000 0.375 0.375 0.375
                    0.125 0.125 0.125 0.125]
                   (seq (.pixels fb)))))))

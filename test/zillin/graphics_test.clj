(ns zillin.graphics-test
  (:require [clojure.test :refer :all]
            [zillin.graphics :refer :all]))


(deftest framebuffer-test
  (testing "creation"
    (let [fb (create-framebuffer 16 32 3)]
      (is (= 16 (framebuffer-width fb)))
      (is (= 32 (framebuffer-height fb)))
      (is (= 3 (framebuffer-components fb)))))
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
      (is (= -3.25 (get-component fb 7 15 2)))))
  (testing "filling"
    (let [fb (create-framebuffer 2 3 4)]
      (framebuffer-fill! fb 1.25)
      (is (= (repeat (* 2 3 4) 1.25) (seq (.pixels fb))))))
  (testing "Z buffer creation"
    (let [zb (create-z-buffer 16 32)]
      (is (= 16 (framebuffer-width zb)))
      (is (= 32 (framebuffer-height zb)))
      (is (= 1 (framebuffer-components zb)))))
  (testing "Z buffer filled with max float"
    (let [zb (create-z-buffer 4 4)]
      (is (= Float/MAX_VALUE (get-component zb 0 0 0)))
      (is (= Float/MAX_VALUE (get-component zb 1 0 0)))
      (is (= Float/MAX_VALUE (get-component zb 0 3 0)))
      (is (= Float/MAX_VALUE (get-component zb 3 3 0))))))


(defn- check-rasterize-triangle [[[x1 y1] [x2 y2] [x3 y3]] [w h] expected]
  (let [fb (create-framebuffer w h 1)
        zb (create-z-buffer w h)
        shader (fn [l1 l2 l3] [(+ l1 l2 l3)])]
    (rasterize-triangle! fb zb shader x1 y1 0 x2 y2 0 x3 y3 0)
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
    (let [fb (create-framebuffer 4 4 1)
          zb (create-z-buffer 4 4)]
      (rasterize-triangle! fb zb (fn [l1 l2 l3] [l1]) 4 4 0 0 4 0 4 0 0)
      (is (= [0.00 0.00 0.00 0.00
              0.00 0.00 0.00 0.25
              0.00 0.00 0.25 0.50
              0.00 0.25 0.50 0.75]
             (seq (.pixels fb))))
      (rasterize-triangle! fb zb (fn [l1 l2 l3] [l2]) 4 4 0 0 4 0 4 0 0)
      (is (= [0.000 0.000 0.000 0.125
              0.000 0.000 0.375 0.125
              0.000 0.625 0.375 0.125
              0.875 0.625 0.375 0.125]
             (seq (.pixels fb))))
      (rasterize-triangle! fb zb (fn [l1 l2 l3] [l3]) 4 4 0 0 4 0 4 0 0)
      (is (= [0.000 0.000 0.000 0.875
              0.000 0.000 0.625 0.625
              0.000 0.375 0.375 0.375
              0.125 0.125 0.125 0.125]
             (seq (.pixels fb)))))))


(deftest rasterising-multiple-components
  (testing "3 components"
    (let [fb (create-framebuffer 4 4 3)
          zb (create-z-buffer 4 4)]
      (rasterize-triangle! fb zb (fn [l1 l2 l3] [l1 l2 l3]) 4 4 0 0 4 0 4 0 0)
      (is (= [0.000 0.000 0.000, 0.000 0.000 0.000, 0.000 0.000 0.000, 0.000 0.125 0.875
              0.000 0.000 0.000, 0.000 0.000 0.000, 0.000 0.375 0.625, 0.250 0.125 0.625
              0.000 0.000 0.000, 0.000 0.625 0.375, 0.250 0.375 0.375, 0.500 0.125 0.375
              0.000 0.875 0.125, 0.250 0.625 0.125, 0.500 0.375 0.125, 0.750 0.125 0.125]
             (seq (.pixels fb)))))))


(deftest depth-test
  (testing "rasterising depth"
    (let [fb (create-framebuffer 4 4 1)
          zb (create-z-buffer 4 4)
          MAXZ Float/MAX_VALUE]
      (rasterize-triangle! fb zb (constantly [1]) 0 4 0, 4 0 1, 4 4 0.5)
      (is (= [MAXZ  MAXZ  MAXZ  0.875
              MAXZ  MAXZ  0.625 0.750
              MAXZ  0.375 0.500 0.625
              0.125 0.250 0.375 0.500]
             (seq (.pixels zb))))))
  (let [fb (create-framebuffer 4 4 1)
        zb (create-z-buffer 4 4)
        MAXZ Float/MAX_VALUE]
    (rasterize-triangle! fb zb (constantly [8]) 0 4 2, 4 0 0, 4 4 2)
    (is (= [MAXZ MAXZ MAXZ 0.25
            MAXZ MAXZ 0.75 0.75
            MAXZ 1.25 1.25 1.25
            1.75 1.75 1.75 1.75]
           (seq (.pixels zb))))
    (rasterize-triangle! fb zb (constantly [1]) 0 4 0, 4 0 2, 4 4 1)
    (testing "only rasterising samples with depth lesser or equal to the depth in the Z buffer"
      (is (= [0.0 0.0 0.0 8.0
              0.0 0.0 8.0 8.0
              0.0 1.0 1.0 1.0
              1.0 1.0 1.0 1.0]
             (seq (.pixels fb)))))
    (testing "putting passing depth samples in the Z buffer"
      (is (= [MAXZ MAXZ MAXZ 0.25
              MAXZ MAXZ 0.75 0.75
              MAXZ 0.75 1.00 1.25
              0.25 0.50 0.75 1.00]
             (seq (.pixels zb)))))))

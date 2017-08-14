(ns zillin.graphics-test
  (:require [clojure.test :refer :all]
            [zillin.graphics :refer :all]
            [zillin.graphics :as g]
            [zillin.math :as m]))


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
  (testing "single component framebuffer component access"
    (let [fb (create-framebuffer 8 16 1)]
      (is (= 0.0 (get-component fb 0 0)))
      (is (= 0.0 (get-component fb 7 3)))
      (is (= 0.0 (get-component fb 7 15)))
      (set-component! fb 0 0 0.75)
      (set-component! fb 7 0 1.75)
      (set-component! fb 0 15 2.75)
      (set-component! fb 7 15 3.75)
      (is (= 0.75 (get-component fb 0 0)))
      (is (= 1.75 (get-component fb 7 0)))
      (is (= 2.75 (get-component fb 0 15)))
      (is (= 3.75 (get-component fb 7 15)))))
  (testing "filling"
    (let [fb (create-framebuffer 2 3 4)]
      (framebuffer-fill! fb 1.25)
      (is (= (repeat (* 2 3 4) 1.25) (seq (.pixels fb))))))
  (testing "Z buffer creation"
    (let [zb (create-z-buffer 16 32)]
      (is (= 16 (framebuffer-width zb)))
      (is (= 32 (framebuffer-height zb)))
      (is (= 1 (framebuffer-components zb)))))
  (testing "Z buffer filled with 0"
    (let [zb (create-z-buffer 4 4)]
      (is (= 0.0 (get-component zb 0 0)))
      (is (= 0.0 (get-component zb 1 0)))
      (is (= 0.0 (get-component zb 0 3)))
      (is (= 0.0 (get-component zb 3 3)))))
  (testing "downsampling"
    (let [fb (doto (create-framebuffer 2 2 1)
                   (set-component! 0 0 1)
                   (set-component! 1 0 2)
                   (set-component! 0 1 4)
                   (set-component! 1 1 8))
          sfb (downsample-framebuffer 2 fb)]
      (is (= 1 (framebuffer-width sfb)))
      (is (= 1 (framebuffer-height sfb)))
      (is (= 1 (framebuffer-components sfb)))
      (is (= 3.75 (get-component sfb 0 0))))
    (let [fb (doto (create-framebuffer 2 2 3)
                   (set-component! 0 0 0 1)
                   (set-component! 0 0 1 2)
                   (set-component! 0 0 2 4)
                   (set-component! 1 0 0 8)
                   (set-component! 1 0 1 16)
                   (set-component! 1 0 2 32)
                   (set-component! 0 1 0 64)
                   (set-component! 0 1 1 128)
                   (set-component! 0 1 2 256)
                   (set-component! 1 1 0 512)
                   (set-component! 1 1 1 1024)
                   (set-component! 1 1 2 2048))
          sfb (downsample-framebuffer 2 fb)]
      (is (= 1 (framebuffer-width sfb)))
      (is (= 1 (framebuffer-height sfb)))
      (is (= 3 (framebuffer-components sfb)))
      (is (= (/ (+ 1 8 64 512) 4.0) (get-component sfb 0 0 0)))
      (is (= (/ (+ 2 16 128 1024) 4.0) (get-component sfb 0 0 1)))
      (is (= (/ (+ 4 32 256 2048) 4.0) (get-component sfb 0 0 2))))
    (let [fb (doto (create-framebuffer 4 6 1)
                   (set-component! 0 0 1)
                   (set-component! 1 0 2)
                   (set-component! 2 0 4)
                   (set-component! 3 0 8)
                   (set-component! 0 1 16)
                   (set-component! 1 1 32)
                   (set-component! 2 1 64)
                   (set-component! 3 1 128)
                   (set-component! 0 2 256)
                   (set-component! 1 2 512)
                   (set-component! 2 2 1024)
                   (set-component! 3 2 2048)
                   (set-component! 0 3 4096)
                   (set-component! 1 3 8192)
                   (set-component! 2 3 16384)
                   (set-component! 3 3 32768)
                   (set-component! 0 4 65536)
                   (set-component! 1 4 131072)
                   (set-component! 2 4 262144)
                   (set-component! 3 4 524288)
                   (set-component! 0 5 1048576)
                   (set-component! 1 5 2097152)
                   (set-component! 2 5 4194304)
                   (set-component! 3 5 8388608))
          sfb (downsample-framebuffer 2 fb)]
      (is (= 2 (framebuffer-width sfb)))
      (is (= 3 (framebuffer-height sfb)))
      (is (= 1 (framebuffer-components sfb)))
      (is (= (/ (+ 1 2 16 32) 4.0) (get-component sfb 0 0)))
      (is (= (/ (+ 4 8 64 128) 4.0) (get-component sfb 1 0)))
      (is (= (/ (+ 256 512 4096 8192) 4.0) (get-component sfb 0 1)))
      (is (= (/ (+ 1024 2048 16384 32768) 4.0) (get-component sfb 1 1)))
      (is (= (/ (+ 65536 131072 1048576 2097152) 4.0) (get-component sfb 0 2)))
      (is (= (/ (+ 262144 524288 4194304 8388608) 4.0) (get-component sfb 1 2))))))


(defn- check-rasterize-triangle [[[x1 y1] [x2 y2] [x3 y3]] [w h] expected]
  (let [fb (create-framebuffer w h 1)
        zb (create-z-buffer w h)
        shader (fn [l1 l2 l3] [(+ l1 l2 l3)])]
    (rasterize-triangle! fb zb shader x1 y1 1 x2 y2 1 x3 y3 1)
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
      (rasterize-triangle! fb zb (fn [l1 l2 l3] [l1]) 4 4 1 0 4 1 4 0 1)
      (is (= [0.00 0.00 0.00 0.00
              0.00 0.00 0.00 0.25
              0.00 0.00 0.25 0.50
              0.00 0.25 0.50 0.75]
             (seq (.pixels fb))))
      (rasterize-triangle! fb zb (fn [l1 l2 l3] [l2]) 4 4 1 0 4 1 4 0 1)
      (is (= [0.000 0.000 0.000 0.125
              0.000 0.000 0.375 0.125
              0.000 0.625 0.375 0.125
              0.875 0.625 0.375 0.125]
             (seq (.pixels fb))))
      (rasterize-triangle! fb zb (fn [l1 l2 l3] [l3]) 4 4 1 0 4 1 4 0 1)
      (is (= [0.000 0.000 0.000 0.875
              0.000 0.000 0.625 0.625
              0.000 0.375 0.375 0.375
              0.125 0.125 0.125 0.125]
             (seq (.pixels fb))))))
  (testing "perspective-correct barycentric coordinates"
    (let [fb (create-framebuffer 4 4 1)
          zb (create-z-buffer 4 4)]
      (rasterize-triangle! fb zb (fn [l1 l2 l3] [l1]) 4 4 0.5 0 4 1 4 0 0.25)
      (is (= (map float [0.0 0.0          0.0         0.0
                         0.0 0.0          0.0         (/ 16 52.0)
                         0.0 0.0          (/ 16 76.0) (/ 32 60.0)
                         0.0 (/ 16 100.0) (/ 32 84.0) (/ 48 68.0)])
             (seq (.pixels fb))))
      (rasterize-triangle! fb zb (fn [l1 l2 l3] [l2]) 4 4 0.5 0 4 1 4 0 0.25)
      (is (= (map float [0.0           0.0          0.0         (/ 16 44.0)
                         0.0           0.0          (/ 48 68.0) (/ 16 52.0)
                         0.0           (/ 80 92.0)  (/ 48 76.0) (/ 16 60.0)
                         (/ 112 116.0) (/ 80 100.0) (/ 48 84.0) (/ 16 68.0)])
             (seq (.pixels fb))))
      (rasterize-triangle! fb zb (fn [l1 l2 l3] [l3]) 4 4 0.5 0 4 1 4 0 0.25)
      (is (= (map float [0.0         0.0         0.0         (/ 28 44.0)
                         0.0         0.0         (/ 20 68.0) (/ 20 52.0)
                         0.0         (/ 12 92.0) (/ 12 76.0) (/ 12 60.0)
                         (/ 4 116.0) (/ 4 100.0) (/ 4 84.0)  (/ 4 68.0)])
             (seq (.pixels fb)))))))


(deftest rasterising-multiple-components
  (testing "3 components"
    (let [fb (create-framebuffer 4 4 3)
          zb (create-z-buffer 4 4)]
      (rasterize-triangle! fb zb (fn [l1 l2 l3] [l1 l2 l3]) 4 4 1 0 4 1 4 0 1)
      (is (= [0.000 0.000 0.000, 0.000 0.000 0.000, 0.000 0.000 0.000, 0.000 0.125 0.875
              0.000 0.000 0.000, 0.000 0.000 0.000, 0.000 0.375 0.625, 0.250 0.125 0.625
              0.000 0.000 0.000, 0.000 0.625 0.375, 0.250 0.375 0.375, 0.500 0.125 0.375
              0.000 0.875 0.125, 0.250 0.625 0.125, 0.500 0.375 0.125, 0.750 0.125 0.125]
             (seq (.pixels fb)))))))


(deftest depth-test
  (testing "rasterising perspective-correct depth reciprocal"
    (let [fb (create-framebuffer 4 4 1)
          zb (create-z-buffer 4 4)]
      (rasterize-triangle! fb zb (constantly [1]) 0 4 1, 4 0 0.25, 4 4 0.5)
      (is (= [0.0           0.0           0.0          (/ 44.0 128)
              0.0           0.0           (/ 68.0 128) (/ 52.0 128)
              0.0           (/ 92.0 128)  (/ 76.0 128) (/ 60.0 128)
              (/ 116.0 128) (/ 100.0 128) (/ 84.0 128) (/ 68.0 128)]
             (seq (.pixels zb))))))
  (let [fb (create-framebuffer 4 4 1)
        zb (create-z-buffer 4 4)]
    (rasterize-triangle! fb zb (constantly [8]) 0 4 0.25, 4 0 1, 4 4 0.25)
    (is (= [0.0          0.0          0.0          (/ 116.0 128)
            0.0          0.0          (/ 92.0 128) (/ 92.0 128)
            0.0          (/ 68.0 128) (/ 68.0 128) (/ 68.0 128)
            (/ 44.0 128) (/ 44.0 128) (/ 44.0 128) (/ 44.0 128)]
           (seq (.pixels zb))))
    (rasterize-triangle! fb zb (constantly [1]) 0 4 1, 4 0 0.25, 4 4 0.5)
    (testing "only rasterising samples with depth reciprocal greater or equal to the depth reciprocal in the Z buffer"
      (is (= [0.0 0.0 0.0 8.0
              0.0 0.0 8.0 8.0
              0.0 1.0 1.0 8.0
              1.0 1.0 1.0 1.0]
             (seq (.pixels fb)))))
    (testing "putting passing depth samples in the Z buffer"
      (is (= [0.0           0.0           0.0          (/ 116.0 128)
              0.0           0.0           (/ 92.0 128) (/ 92.0 128)
              0.0           (/ 92.0 128)  (/ 76.0 128) (/ 68.0 128)
              (/ 116.0 128) (/ 100.0 128) (/ 84.0 128) (/ 68.0 128)]
             (seq (.pixels zb)))))))


(deftest rasterize-triangles-test
  (testing "rasterising coloured triangles"
    (let [fb (create-framebuffer 4 4 1)
          zb (create-z-buffer 4 4)
          triangles [{:a (m/vec3 0 0 0.5) :b (m/vec3 4 4 0.5) :c (m/vec3 0 4 0.5) :color [1.0]}
                     {:a (m/vec3 0 0 0.25) :b (m/vec3 4 0 0.25) :c (m/vec3 0 4 0.25) :color [2.0]}
                     {:a (m/vec3 0 0 0.125) :b (m/vec3 4 0 0.125) :c (m/vec3 4 4 0.125) :color [3.0]}]]
      (rasterize-triangles! fb zb triangles)
      (is (= [2.0 2.0 2.0 3.0
              1.0 2.0 3.0 3.0
              1.0 1.0 3.0 3.0
              1.0 1.0 1.0 3.0]
             (seq (.pixels fb))))
      (is (= [(/ 4.0) (/ 4.0) (/ 4.0) (/ 8.0)
              (/ 2.0) (/ 4.0) (/ 8.0) (/ 8.0)
              (/ 2.0) (/ 2.0) (/ 8.0) (/ 8.0)
              (/ 2.0) (/ 2.0) (/ 2.0) (/ 8.0)]
             (seq (.pixels zb)))))))

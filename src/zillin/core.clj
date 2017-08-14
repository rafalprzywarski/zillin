(ns zillin.core
  (:require [zillin.graphics :as g]
            [zillin.math :as m])
  (:import (java.io File)
           (java.awt.image BufferedImage)
           (javax.imageio ImageIO))
  (:gen-class))

(defn save-framebuffer [^String filename fb]
  (let [width (g/framebuffer-width fb)
        height (g/framebuffer-height fb)
        img ^BufferedImage (BufferedImage. width height BufferedImage/TYPE_INT_RGB)]
    (doseq [^int y (range height)]
      (doseq [^int x (range width)]
        (let [r ^int (* 255 (g/get-component fb x y 0))
              g ^int (* 255 (g/get-component fb x y 1))
              b ^int (* 255 (g/get-component fb x y 2))
              rgb (bit-or b (bit-shift-left g 8) (bit-shift-left r 16))]
          (.setRGB img x (- height y 1) rgb))))
    (ImageIO/write img "png" (File. filename))))

(defn transform-triangles [transform triangles]
  (map #(assoc %
               :a (m/mvmult transform (:a %))
               :b (m/mvmult transform (:b %))
               :c (m/mvmult transform (:c %)))
       triangles))

(defn simple-triangles []
  (let [fb (g/create-framebuffer 128 128 3)
        zb (g/create-z-buffer 128 128)
        transform (m/mmult
                   (m/perspective-projection 1.0 128 128)
                   (m/translation (m/vec3 0 0 40)))
        triangles [{:a (m/vec3 -10 -10 0) :b (m/vec3 10 -10 0) :c (m/vec3 10 10 0) :color [1 0 0]}
                   {:a (m/vec3 -10 -10 0) :b (m/vec3 10 10 0) :c (m/vec3 -10 10 0) :color [0 1 1]}
                   {:a (m/vec3 -30 -10 0) :b (m/vec3 -10 -10 0) :c (m/vec3 -10 10 0) :color [0 1 0]}
                   {:a (m/vec3 -30 -10 0) :b (m/vec3 -10 10 0) :c (m/vec3 -30 10 0) :color [1 0 1]}
                   {:a (m/vec3 10 -10 0) :b (m/vec3 30 -10 0) :c (m/vec3 30 10 0) :color [0 0 1]}
                   {:a (m/vec3 10 -10 0) :b (m/vec3 30 10 0) :c (m/vec3 10 10 0) :color [1 1 0]}]
        triangles (transform-triangles transform triangles)]
    (g/rasterize-triangles! fb zb triangles)
    (save-framebuffer "simple-triangle.png" fb)))

(defn checker-pattern
  ([] (checker-pattern 512 384))
  ([width height] (checker-pattern 1 width height))
  ([aa width height]
   (let [fwidth (* aa width) fheight (* aa height)
         fb (g/create-framebuffer fwidth fheight 3)
         zb (g/create-z-buffer fwidth fheight)
         side 1024
         n 128
         triangles (apply concat (map (fn [[x y]]
                                        (let [color (if (= (even? x) (even? y)) [1 0 0] [0 0 1])
                                              bx (* x side)
                                              by (* y side)]
                                          [{:a (m/vec3 bx by 0) :b (m/vec3 (+ bx side) by 0) :c (m/vec3 (+ bx side) (+ by side) 0) :color color}
                                           {:a (m/vec3 bx by 0) :b (m/vec3 (+ bx side) (+ by side) 0) :c (m/vec3 bx (+ by side) 0) :color color}]))
                                      (apply concat (map (fn [y] (map (fn [x] [x y]) (range n))) (range n)))))
         transform (m/mmult
                    (m/perspective-projection 0.5 fwidth fheight)
                    (m/translation (m/vec3 0 -200 (* 0.5 (+ 1 n) side)))
                    (m/rotation (m/vec3 1 0 0) (* 0.45 Math/PI))
                    (m/translation (m/vec3 (* -0.5 n side) (* -0.5 n side) 0)))
         triangles (transform-triangles transform triangles)]
     (g/rasterize-triangles! fb zb triangles)
     (let [sfb (g/downsample-framebuffer aa fb)]
       (save-framebuffer (str "checker-pattern-" aa "-" width "-" height ".png") sfb)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

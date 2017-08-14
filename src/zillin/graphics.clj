(ns zillin.graphics
  (require [zillin.math :as m]))


(defprotocol Framebuffer
  (framebuffer-width [this])
  (framebuffer-height [this])
  (framebuffer-components [this])
  (get-component [this x y] [this x y i])
  (set-component! [this x y val] [this x y i val])
  (framebuffer-fill! [this val]))


(deftype ArrayFramebuffer
  [width height components pixels]
  Framebuffer
  (framebuffer-width [this] (.width this))
  (framebuffer-height [this] (.height this))
  (framebuffer-components [this] (.components this))
  (get-component [this x y]
    (aget ^floats (.pixels this) (+ x (* y (.width this)))))
  (get-component [this x y i]
    (aget ^floats (.pixels this) (+ i (* (+ x (* y (.width this))) (.components this)))))
  (set-component! [this x y val]
    (let [x ^int x
          y ^int y]
      (aset-float (.pixels this) (+ x (* y (.width this))) val)))
  (set-component! [this x y i val]
    (let [x ^int x
          y ^int y
          i ^int i]
      (aset-float (.pixels this) (+ i (* (+ x (* y (.width this))) (.components this))) val)))
  (framebuffer-fill! [this val]
    (java.util.Arrays/fill ^floats (.pixels this) ^float val)))


(defn ^zillin.graphics.ArrayFramebuffer create-framebuffer [width height components]
  (ArrayFramebuffer. width height components (float-array (* width height components))))


(defn ^zillin.graphics.ArrayFramebuffer create-z-buffer [width height]
  (create-framebuffer width height 1))

(defn ^zillin.graphics.ArrayFramebuffer downsample-framebuffer [n fb]
  (let [width (/ (framebuffer-width fb) n)
        height (/ (framebuffer-height fb) n)
        nc (framebuffer-components fb)
        sfb (create-framebuffer width height nc)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (let [bx (* x n)
              by (* y n)
              s (double-array nc)]
          (doseq [sy (range n)]
            (doseq [sx (range n)]
              (doseq [c (range nc)]
                (aset-double s c (+ (aget s c) (get-component fb (+ bx sx) (+ by sy) c))))))
          (doseq [c (range nc)]
            (set-component! sfb x y c (/ (aget s c) (* n n)))))))
    sfb))


(defmacro double-area [x1 y1 x2 y2 xp yp]
  `(- (* (- ~x2 ~x1) (- ~yp ~y1)) (* (- ~y2 ~y1) (- ~xp ~x1))))


(defmacro edge-w [x1 y1 x2 y2]
  `(if (or (> ~y2 ~y1) (and (= ~y2 ~y1) (< ~x2 ~x1)))
     0.0
     (Math/nextDown 0.0)))


(defn rasterize-triangle!
  "Rasterises a triangle.
  X, Y coordinates are expected to be in screen space.
  Z coordinates' reciprocals are expected to be in view space.
  Colour is calculated by shader given barycentric coordinates [l1 l2 l3]."
  [fb zb shader x1 y1 rz1 x2 y2 rz2 x3 y3 rz3]
  (let [x1 ^double x1
        y1 ^double y1
        x2 ^double x2
        y2 ^double y2
        x3 ^double x3
        y3 ^double y3
        area (double-area x1 y1 x2 y2 x3 y3)]
    (when (> area 0.0)
      (let [lx (int (Math/floor (min x1 x2 x3)))
            lx (max 0 lx)
            ly (int (Math/floor (min y1 y2 y3)))
            ly (max 0 ly)
            ux (int (Math/ceil (max x1 x2 x3)))
            ux (min (framebuffer-width fb) ux)
            uy (int (Math/ceil (max y1 y2 y3)))
            uy (min (framebuffer-height fb) uy)
            ew1 ^double (edge-w x2 y2 x3 y3)
            ew2 ^double (edge-w x3 y3 x1 y1)
            ew3 ^double (edge-w x1 y1 x2 y2)
            components ^long (framebuffer-components fb)
            rz1 ^double rz1
            rz2 ^double rz2
            rz3 ^double rz3]
        (doseq [^double y (range ly uy)]
          (doseq [^double x (range lx ux)]
            (let [xc (+ 0.5 x)
                  yc (+ 0.5 y)
                  w1 (double-area x2 y2 x3 y3 xc yc)
                  w2 (double-area x3 y3 x1 y1 xc yc)
                  w3 (- area w1 w2)]
              (when (and (> w1 ew1) (> w2 ew2) (> w3 ew3))
                (let [rz1w1 (* rz1 w1)
                      rz2w2 (* rz2 w2)
                      rz3w3 (* rz3 w3)
                      rzarea (+ rz1w1 rz2w2 rz3w3)
                      rz (/ rzarea area)]
                  (when (>= rz (get-component zb x y))
                    (let [l1 (/ rz1w1 rzarea)
                          l2 (/ rz2w2 rzarea)
                          l3 (- 1.0 l1 l2)
                          vals (shader l1 l2 l3)]
                      (set-component! zb x y rz)
                      (dotimes [i components]
                        (set-component! fb x y i (vals i))))))))))))))


(defn rasterize-triangles! [fb zb triangles]
  (doseq [t triangles]
    (let [^zillin.math.Vec3 a (:a t)
          ^zillin.math.Vec3 b (:b t)
          ^zillin.math.Vec3 c (:c t)
          color (:color t)]
      (rasterize-triangle!
       fb zb (constantly color)
       (.x a) (.y a) (.z a)
       (.x b) (.y b) (.z b)
       (.x c) (.y c) (.z c)))))

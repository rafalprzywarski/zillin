(ns zillin.graphics)

(defprotocol Framebuffer
    (framebuffer-width [this])
    (framebuffer-height [this])
    (framebuffer-components [this])
    (get-component [this x y i])
    (set-component! [this x y i val]))

(deftype ArrayFramebuffer
    [width height components pixels]
    Framebuffer
    (framebuffer-width [this] (.width this))
    (framebuffer-height [this] (.height this))
    (framebuffer-components [this] (.components this))
    (get-component [this x y i]
        (aget ^floats (.pixels this) (+ i (* (+ x (* y (.width this))) (.components this)))))
    (set-component! [this x y i val]
        (aset-float (.pixels this) (+ i (* (+ x (* y (.width this))) (.components this))) val)))

(defn ^zillin.graphics.ArrayFramebuffer create-framebuffer [width height components]
    (ArrayFramebuffer. width height components (float-array (* width height components))))

(defn rasterize-triangle! [fb shader x1 y1 x2 y2 x3 y3]
    (let [double-area (fn [x1 y1 x2 y2 xp yp]
                          (- (* (- x2 x1) (- yp y1)) (* (- y2 y1) (- xp x1))))
          area (double-area x1 y1 x2 y2 x3 y3)]
         (when (> area 0.0)
            (let [edge-w (fn [x1 y1 x2 y2]
                            (if (or (> y2 y1) (and (= y2 y1) (< x2 x1)))
                                0.0
                                (Math/nextDown 0.0)))
                  lx (int (Math/floor (min x1 x2 x3)))
                  lx (max 0 lx)
                  ly (int (Math/floor (min y1 y2 y3)))
                  ly (max 0 ly)
                  ux (int (Math/ceil (max x1 x2 x3)))
                  ux (min (framebuffer-width fb) ux)
                  uy (int (Math/ceil (max y1 y2 y3)))
                  uy (min (framebuffer-height fb) uy)
                  ew1 (edge-w x2 y2 x3 y3)
                  ew2 (edge-w x3 y3 x1 y1)
                  ew3 (edge-w x1 y1 x2 y2)
                  components (framebuffer-components fb)]
                 (doseq [y (range ly uy)]
                    (doseq [x (range lx ux)]
                        (let [xc (+ 0.5 x)
                              yc (+ 0.5 y)
                              w1 (double-area x2 y2 x3 y3 xc yc)
                              w2 (double-area x3 y3 x1 y1 xc yc)
                              w3 (double-area x1 y1 x2 y2 xc yc)]
                            (when (and (> w1 ew1) (> w2 ew2) (> w3 ew3))
                                (let [vals (shader (/ w1 area) (/ w2 area) (/ w3 area))]
                                    (dotimes [i components]
                                        (set-component! fb x y i (vals i))))))))))))

(ns zillin.graphics)

(defprotocol Framebuffer
    (get-component [this x y i])
    (set-component! [this x y i val]))

(defn- component-index [f x y i]
    (+ i (* (+ x (* y (.width f))) (.components f))))

(deftype ArrayFramebuffer
    [width height components pixels]
    Framebuffer
    (get-component [this x y i]
        (aget (.pixels this) (component-index this x y i)))
    (set-component! [this x y i val]
        (aset-float (.pixels this) (component-index this x y i) val)))

(defn create-framebuffer [width height components]
    (ArrayFramebuffer. width height components (float-array (* width height components))))

(defn rasterize-triangle! [fb x1 y1 x2 y2 x3 y3]
    (let [edge-w (fn [x1 y1 x2 y2]
                    (let [dx (- x2 x1)
                          dy (- y2 y1)]
                         (if (or (> dy 0.0) (and (= dy 0.0) (< dx 0.0)))
                            0.0
                            (Math/nextDown 0.0))))
          orient (fn [x1 y1 x2 y2 xp yp]
                    (- (* (- x2 x1) (- yp y1)) (* (- y2 y1) (- xp x1))))
          lx (int (Math/floor (min x1 x2 x3)))
          ly (int (Math/floor (min y1 y2 y3)))
          ux (int (Math/ceil (max x1 x2 x3)))
          uy (int (Math/ceil (max y1 y2 y3)))
          ew1 (edge-w x2 y2 x3 y3)
          ew2 (edge-w x3 y3 x1 y1)
          ew3 (edge-w x1 y1 x2 y2)]
         (doseq [y (range ly uy)]
            (doseq [x (range lx ux)]
                (let [xc (+ 0.5 x)
                      yc (+ 0.5 y)
                      w1 (orient x2 y2 x3 y3 xc yc)
                      w2 (orient x3 y3 x1 y1 xc yc)
                      w3 (orient x1 y1 x2 y2 xc yc)]
                    (when (and (> w1 ew1) (> w2 ew2) (> w3 ew3))
                        (set-component! fb x y 0 1)))))))

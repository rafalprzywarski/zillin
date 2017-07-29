(ns zillin.graphics)

(defprotocol Framebuffer
    (get-component [this x y i])
    (set-component [this x y i val]))

(defn- component-index [f x y i]
    (+ i (* (+ x (* y (.width f))) (.components f))))

(deftype ArrayFramebuffer
    [width height components pixels]
    Framebuffer
    (get-component [this x y i]
        (aget (.pixels this) (component-index this x y i)))
    (set-component [this x y i val]
        (aset-float (.pixels this) (component-index this x y i) val)))

(defn create-framebuffer [width height components]
    (ArrayFramebuffer. width height components (float-array (* width height components))))

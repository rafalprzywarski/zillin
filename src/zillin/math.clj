(ns zillin.math)

(defrecord Vec3 [^float x ^float y ^float z])

(defn vec3 [x y z] (Vec3. x y z))

(defn add [^Vec3 l ^Vec3 r]
    (Vec3.
        (+ (.x l) (.x r))
        (+ (.y l) (.y r))
        (+ (.z l) (.z r))))

(defn sub [^Vec3 l ^Vec3 r]
    (Vec3.
        (- (.x l) (.x r))
        (- (.y l) (.y r))
        (- (.z l) (.z r))))

(defn dot [^Vec3 l ^Vec3 r]
    (+
        (* (.x l) (.x r))
        (* (.y l) (.y r))
        (* (.z l) (.z r))))

(defn det [a b c d] (- (* a d) (* b c)))

(defn cross [^Vec3 l ^Vec3 r]
    (let [xl (.x l) xr (.x r)
          yl (.y l) yr (.y r)
          zl (.z l) zr (.z r)]
         (Vec3.
             (det yl yr zl zr)
             (det zl zr xl xr)
             (det xl xr yl yr))))

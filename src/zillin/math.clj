(ns zillin.math)

(defrecord Vec3 [^double x ^double y ^double z])

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


(defn length [^Vec3 v]
  (Math/sqrt (dot v v)))


(defn ^Vec3 normalize [^Vec3 v]
  (let [len (length v)]
    (Vec3. (/ (.x v) len) (/ (.y v) len) (/ (.z v) len))))


(defn det [a b c d] (- (* a d) (* b c)))

(defn cross [^Vec3 l ^Vec3 r]
  (let [xl (.x l) xr (.x r)
        yl (.y l) yr (.y r)
        zl (.z l) zr (.z r)]
    (Vec3.
     (det yl yr zl zr)
     (det zl zr xl xr)
     (det xl xr yl yr))))

(defrecord Mat4 [^double m00 ^double m01 ^double m02 ^double m03
                 ^double m10 ^double m11 ^double m12 ^double m13
                 ^double m20 ^double m21 ^double m22 ^double m23
                 ^double m30 ^double m31 ^double m32 ^double m33])

(defn mat4 [m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33]
  (Mat4. m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33))

(defn mmult [^Mat4 a ^Mat4 b]
  (mat4
   (+ (* (.m00 a) (.m00 b)) (* (.m01 a) (.m10 b)) (* (.m02 a) (.m20 b)) (* (.m03 a) (.m30 b)))
   (+ (* (.m00 a) (.m01 b)) (* (.m01 a) (.m11 b)) (* (.m02 a) (.m21 b)) (* (.m03 a) (.m31 b)))
   (+ (* (.m00 a) (.m02 b)) (* (.m01 a) (.m12 b)) (* (.m02 a) (.m22 b)) (* (.m03 a) (.m32 b)))
   (+ (* (.m00 a) (.m03 b)) (* (.m01 a) (.m13 b)) (* (.m02 a) (.m23 b)) (* (.m03 a) (.m33 b)))
   (+ (* (.m10 a) (.m00 b)) (* (.m11 a) (.m10 b)) (* (.m12 a) (.m20 b)) (* (.m13 a) (.m30 b)))
   (+ (* (.m10 a) (.m01 b)) (* (.m11 a) (.m11 b)) (* (.m12 a) (.m21 b)) (* (.m13 a) (.m31 b)))
   (+ (* (.m10 a) (.m02 b)) (* (.m11 a) (.m12 b)) (* (.m12 a) (.m22 b)) (* (.m13 a) (.m32 b)))
   (+ (* (.m10 a) (.m03 b)) (* (.m11 a) (.m13 b)) (* (.m12 a) (.m23 b)) (* (.m13 a) (.m33 b)))
   (+ (* (.m20 a) (.m00 b)) (* (.m21 a) (.m10 b)) (* (.m22 a) (.m20 b)) (* (.m23 a) (.m30 b)))
   (+ (* (.m20 a) (.m01 b)) (* (.m21 a) (.m11 b)) (* (.m22 a) (.m21 b)) (* (.m23 a) (.m31 b)))
   (+ (* (.m20 a) (.m02 b)) (* (.m21 a) (.m12 b)) (* (.m22 a) (.m22 b)) (* (.m23 a) (.m32 b)))
   (+ (* (.m20 a) (.m03 b)) (* (.m21 a) (.m13 b)) (* (.m22 a) (.m23 b)) (* (.m23 a) (.m33 b)))
   (+ (* (.m30 a) (.m00 b)) (* (.m31 a) (.m10 b)) (* (.m32 a) (.m20 b)) (* (.m33 a) (.m30 b)))
   (+ (* (.m30 a) (.m01 b)) (* (.m31 a) (.m11 b)) (* (.m32 a) (.m21 b)) (* (.m33 a) (.m31 b)))
   (+ (* (.m30 a) (.m02 b)) (* (.m31 a) (.m12 b)) (* (.m32 a) (.m22 b)) (* (.m33 a) (.m32 b)))
   (+ (* (.m30 a) (.m03 b)) (* (.m31 a) (.m13 b)) (* (.m32 a) (.m23 b)) (* (.m33 a) (.m33 b)))))

(defn mvmult [^Mat4 m ^Vec3 v]
  (let [w (+ (* (.m30 m) (.x v)) (* (.m31 m) (.y v)) (* (.m32 m) (.z v)) (.m33 m))]
    (vec3
     (/ (+ (* (.m00 m) (.x v)) (* (.m01 m) (.y v)) (* (.m02 m) (.z v)) (.m03 m)) w)
     (/ (+ (* (.m10 m) (.x v)) (* (.m11 m) (.y v)) (* (.m12 m) (.z v)) (.m13 m)) w)
     (/ (+ (* (.m20 m) (.x v)) (* (.m21 m) (.y v)) (* (.m22 m) (.z v)) (.m23 m)) w))))

(defn translation [^Vec3 v]
  (mat4 1 0 0 (.x v)
        0 1 0 (.y v)
        0 0 1 (.z v)
        0 0 0 1))


(defn rotation [^Vec3 axis ^double angle]
  (let [cosa (Math/cos angle)
        one-cosa (- 1 cosa)
        sina (Math/sin angle)
        axis (normalize axis)
        x (.x axis)
        y (.y axis)
        z (.z axis)
        xycosa (* x y one-cosa)
        xzcosa (* x z one-cosa)
        yzcosa (* y z one-cosa)
        xsina (* x sina)
        ysina (* y sina)
        zsina (* z sina)]
    (mat4 (+ cosa (* x x one-cosa)) (- xycosa zsina)          (+ xzcosa ysina)          0
          (+ xycosa zsina)          (+ cosa (* y y one-cosa)) (- yzcosa xsina)          0
          (- xzcosa ysina)          (+ yzcosa xsina)          (+ cosa (* z z one-cosa)) 0
          0                         0                         0                         1)))

(defn perspective-projection [^double tan-angle ^double width ^double height]
  (let [d (/ height (* 2 tan-angle))]
    (mat4 d 0 (/ width 2) 0
          0 d (/ height 2) 0
          0 0 0 1
          0 0 1 0)))

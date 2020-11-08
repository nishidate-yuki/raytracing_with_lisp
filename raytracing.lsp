(setq size 512)

(defun add (v1 v2) 
    (list 
        (+ (first v1) (first v2)) 
        (+ (second v1) (second v2)) 
        (+ (third v1) (third v2))
    ) 
)

(defun minus (v1 v2) 
    (list 
        (- (first v1) (first v2)) 
        (- (second v1) (second v2)) 
        (- (third v1) (third v2))
    ) 
)

(defun mul (vec val) 
    (list 
        (* (first vec)  val) 
        (* (second vec) val) 
        (* (third vec)  val)
    ) 
)

(defun dot (v1 v2) 
    (+ 
        (* (first v1) (first v2)) 
        (* (second v1) (second v2)) 
        (* (third v1) (third v2))
    ) 
)

(defun len (v) 
    (sqrt
        (+
            (* (first v) (first v))
            (* (second v) (second v))
            (* (third v) (third v))
        )
    )
)

(defun norm (v) 
    (progn
        (setq l (len v))
        (list 
            (/ (first v) l)
            (/ (second v) l)
            (/ (third v) l)
        ) 
    )
)

(defun gen_ray (w h)
    (list 
        (list 0 0 -10)
        (norm
            (minus 
                (list 
                    (- (* (float (/ w size)) 2) 1) 
                    (- (- (* (float (/ h size)) 2) 1))
                    -5
                )
                (list 0 0 -10)
            )
        )
    )
)

(defun intersect (ray sphere)
    (progn
        (setq rayorg (first ray))
        (setq raydir (second ray))
        (setq radius (first sphere))
        (setq center (second sphere))

        (setq a (dot raydir raydir))
        (setq b (dot (minus rayorg center) raydir))
        (setq c (- (dot (minus rayorg center) (minus rayorg center))
                (* radius radius)))
        (setq d (- (* b b) (* a c)))

        (if (> d 0)
            (progn
                (setq dist (- (- b) (sqrt d)))
                (setq pos (add rayorg (mul raydir dist)))
                (setq normal (norm (minus pos center)))
                (list t normal pos)
            )
            (list nil)
        )
    )
)

(with-open-file 
    (img "img.ppm" :direction :output :if-exists :supersede :if-does-not-exist :create)

    (setq sphere (list 1 (list 0 0 0)))
    (setq lightpos (list 5 5 -5))
    (format img "P3~%~d ~d~%255~%" size size)
    (loop for h from 0 to (- size 1)
        do(loop for w from 0 to (- size 1)
            do (setq ray (gen_ray w h))
            do (setq res (intersect ray sphere))
            do (if (first res)
                (progn
                    (setq normal (second res))
                    (setq hitpos (third res))
                    (setq ldir (norm (minus lightpos hitpos)))
                    (setq lighting (max (dot normal ldir) 0))
                    (format img "~d ~d ~d~%" (round (* 255 lighting)) (round (* 255 lighting)) (round (* 255 lighting)))
                )
                (format img "~d ~d ~d~%" (round (* 255 (/ w size))) (round (* 255 (/ h size))) 128)
            )
        )
    )
)

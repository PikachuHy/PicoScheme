(define *world* '())
(define (make-point x y z) (vector x y z))
(define (point-x p)        (vector-ref p 0))
(define (point-y p)        (vector-ref p 1))
(define (point-z p)        (vector-ref p 2))
(define (sq x)             (* x x))
(define (mag x y z)        (sqrt (+ (sq x) (sq y) (sq z))))
(define eye                (make-point 0.0 0.0 200.0))

(define (unit-vector x y z)
  (let ((d (mag x y z)))
    (make-point (/ x d)(/ y d)(/ z d))))

(define (distance p1 p2)
  (mag (- (point-x p1) (point-x p2))
       (- (point-y p1) (point-y p2))
       (- (point-z p1) (point-z p2))))

(define (minroot a b c)
  (if (zero? a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4.0 a c))))
        (if (negative? disc)
            #f
            (let ((discrt (sqrt disc))
                  (minus-b (- b))
                  (two-a (* 2.0 a)))
              (min (/ (+ minus-b discrt) two-a)
                        (/ (- minus-b discrt) two-a)))))))

(define (tracer pathname res)
  (call-with-output-file pathname
    (lambda (p)
      (let ((extent (* res 100)))

        (display "P2 " p)
        (write extent p)
        (display " " p)
        (write extent p)
        (display " 255" p)
        (newline p)

        (do ((y 0 (+ y 1)))
            ((= y extent))
          (do ((x 0 (+ x 1)))
              ((= x extent))
            (write (color-at
                     (+ -50.0 (/ x res))
                     (+ -50.0 (/ y res)))
;                    (+ -50.0 (/ (exact->inexact x) (exact->inexact res)))
;                    (+ -50.0 (/ (exact->inexact y) (exact->inexact res))))
                   p)
            (newline p)))))))

(define (color-at x y)
  (let ((ray (unit-vector (- x (point-x eye))
                          (- y (point-y eye))
                          (-   (point-z eye)))))
    (round (* (sendray eye ray) 255.0))))

(define (sendray pt ray)
  (let* ((x   (first-hit pt ray))
         (s   (vector-ref x 0))
         (int (vector-ref x 1)))
    (if s (* (lambert s int ray)
             (surface-color s))
        0.0)))

(define (first-hit pt ray)
  (let loop ((lst *world*) (surface #f) (hit #f) (dist 1e308))
    (if (null? lst)
        (vector surface hit)
        (let ((s (car lst)))
          (let ((h (intersect s pt ray)))
            (if h
                (let ((d (distance h pt)))
                  (if (< d dist)
                      (loop (cdr lst) s h d)
                      (loop (cdr lst) surface hit dist)))
                (loop (cdr lst) surface hit dist)))))))

(define (lambert s int ray)
  (let ((n (normal s int)))
    (max 0.0 (+ (* (point-x ray) (point-x n))
                (* (point-y ray) (point-y n))
                (* (point-z ray) (point-z n))))))

(define (make-sphere color radius center)(vector color radius center))
(define (sphere-color  s) (vector-ref s 0))
(define (sphere-radius s) (vector-ref s 1))
(define (sphere-center s) (vector-ref s 2))

(define (defsphere x y z r c)
  (let ((s (make-sphere c r (make-point x y z))))
    (set! *world* (cons s *world*))
    s))

(define (surface-color s)
  (sphere-color s))

(define (intersect s pt ray)
  (sphere-intersect s pt ray))

(define (sphere-intersect s pt ray)
  (let* ((xr (point-x ray))
         (yr (point-y ray))
         (zr (point-z ray))
         (c  (sphere-center s))
         (n  (minroot
             (+ (sq xr) (sq yr) (sq zr))
             (* 2.0 (+ (* (- (point-x pt) (point-x c)) xr)
                       (* (- (point-y pt) (point-y c)) yr)
                       (* (- (point-z pt) (point-z c)) zr)))
             (+ (sq (- (point-x pt) (point-x c)))
                (sq (- (point-y pt) (point-y c)))
                (sq (- (point-z pt) (point-z c)))
                (- (sq (sphere-radius s)))))))
    (if n
        (make-point (+ (point-x pt) (* n xr))
                    (+ (point-y pt) (* n yr))
                    (+ (point-z pt) (* n zr)))
        #f)))

(define (normal s pt)
  (sphere-normal s pt))

(define (sphere-normal s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (point-x c) (point-x pt))
                 (- (point-y c) (point-y pt))
                 (- (point-z c) (point-z pt)))))

(define (ray-test opt)
  (set! *world* '())
  (defsphere   0.0 -300.0 -1200.0 200.0 0.8)
  (defsphere -80.0 -150.0 -1200.0 200.0 0.7)
  (defsphere  70.0 -100.0 -1200.0 200.0 0.9)
  (do ((x -2 (+ x 1)))
      ((> x 2))
    (do ((z 2 (+ z 1)))
        ((> z 7))
      (defsphere (* x 200.0) 300.0 (* z -400.0) 40.0 0.75)))
  (tracer "spheres.pgm" opt))

(ray-test 1.0)

(define (make-world)
  (set! *world* '())
  (defsphere   0.0 -300.0 -1200.0 200.0 0.8)
  (defsphere -80.0 -150.0 -1200.0 200.0 0.7)
  (defsphere  70.0 -100.0 -1200.0 200.0 0.9)
  (do ((x -2 (+ x 1)))
      ((> x 2) #t)
    (do ((z 2 (+ z 1)))
        ((> z 7) #t)
      (defsphere
        (*  x 200.0)
        300.0
        (* z -400.0)
        40.0
        0.75))))

(define (trace res)
  (let ((extent (* res 100))
        (points '())
        (color  '())
        (pixel (lambda(x)(+ -50 (/ x res)))) )

        (do ((y 0 (+ y 1)))
            ((= y extent)(list->vector points))

          (do ((x 0 (+ x 1)))
              ((= x extent))

            (set! color (color-at (pixel x) (pixel y)))

            (unless (zero? color)
              (set! points (cons (list (cons x y) color) points))) ))))


(define cntr (make-point 20 30 -1200))
(define sphr (make-sphere 0.8 200 cntr))

(define (make-ray p1 p2)
  (unit-vector (- (point-x p1) (point-x p2))
               (- (point-y p1) (point-y p2))
               (- (point-z p1) (point-z p2))))

(define ray (make-ray cntr eye))
(sphere-center sphr)
(sphere-intersect sphr eye ray)
(set! *world* (cons sphr '()))
(first-hit eye ray)
(sendray eye ray)

(trace 1)

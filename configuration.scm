#!/usr/bin/env gosh
(use file.util)
(use srfi-1)
(use srfi-13)
(use srfi-42)
(use math.const)
(use slib)
(require 'format)

;;(define make-vector (with-module gauche make-vector))

(define (make-2dv x y)
  (let ((v1 (make-vector y)))
    (dotimes (n y)
      (vector-set! v1 n (make-vector x 0.0)))
    v1))

(define (2dvref v x y)
  (vector-ref (vector-ref v x) y))

(define (2dset! v x y val)
  (vector-set! (vector-ref v x) y val)
  v)

(define (2dvproduct v1 v2)
  (let ((xlen  (vector-length v1))
        (ylen  (vector-length (vector-ref v2 0))))
    ;; (when (not (= xlen ylen)) (error "mismatch length"))
    (let ((v3 (make-2dv ylen xlen)))
      (do-ec (: i xlen)
             (: j ylen)
             (let ((val (+ (* (2dvref v1 i 0) (2dvref v2 0 j))
                           (* (2dvref v1 i 1) (2dvref v2 1 j))
                           (* (2dvref v1 i 2) (2dvref v2 2 j)))))
               (2dset! v3 i j val)))
      v3)))

(define (%2dcal proc v1 v2)
  (let ((xlen (vector-length v1)))
    (let ((v3 (make-vector xlen)))
      (do-ec (: i xlen)
             (vector-set! v3 i (proc (vector-ref v1 i) (vector-ref v2 i))))
      v3)))
(define (2dcal proc v1 v2)
  (let ((xlen (vector-length v1)))
    (let ((v3 (make-vector xlen)))
      (do-ec (: i xlen)
             (vector-set! v3 i (%2dcal proc (vector-ref v1 i) v2)))
      v3)))
(define (add2dvec v1 v2)
  (2dcal + v1 v2))
(define (prod2dvec v1 v2)
  (2dcal * v1 v2))


(define (2dvector v1 v2)
  (vector (- (vector-ref v1 0) (vector-ref v2 0))
          (- (vector-ref v1 1) (vector-ref v2 1))
          (- (vector-ref v1 2) (vector-ref v2 2))))

(define (normal-verctor v1 v2)
  (vector (- (* (ref v1 2) (ref v2 1))
             (* (ref v1 1) (ref v2 2)))
          (- (* (ref v1 0) (ref v2 2))
             (* (ref v1 2) (ref v2 0)))
          (- (* (ref v1 1) (ref v2 0))
             (* (ref v1 0) (ref v2 1)))))

(define (magnitudev v)
  ;; (magnitudev (vector (/ 3 7.071) (/ 4 7.071) (/ 5 7.071)))
  (let ((x (vector-ref v 0))
        (y (vector-ref v 1))
        (z (vector-ref v 2)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

(define (unitv v)
  ;; (magnitudev (unitv #(3 4 5)))
  (let ((mag (magnitudev v)))
    (vector (/ (vector-ref v 0) mag)
            (/ (vector-ref v 1) mag)
            (/ (vector-ref v 2) mag))))


(define (rotate theta v0 v :optional (translation #(0.0 0.0 0.0)))
  ;; (rotate pi/2 (unitv #(3 4 5)) #(#(1 2 3) #(3 4 5) #(6 7 8)))
  (let ((vx (ref v 0))
        (vy (ref v 1))
        (vz (ref v 2))
        (nx (ref v0 0))
        (ny (ref v0 1))
        (nz (ref v0 2))
        (costh (cos theta))
        (sinth (sin theta)))
    (let1 rv 
      (vector
       (vector (+ (* nx nx (- 1.0 costh)) costh)
               (- (* nx ny (- 1.0 costh)) (* nz sinth))
               (+ (* nz nx (- 1.0 costh)) (* ny sinth)))
       (vector (+ (* nx ny (- 1.0 costh)) (* nz sinth))
               (+ (* ny ny (- 1.0 costh)) costh)
               (- (* ny nz (- 1.0 costh)) (* nx sinth)))
       (vector (- (* nz nx (- 1.0 costh)) (* ny sinth))
               (+ (* ny nz (- 1.0 costh)) (* nx sinth))
               (+ (* nz nz (- 1.0 costh)) costh)))
      (add2dvec (2dvproduct v rv) translation))))


(define (main args)
  (let ((file (second args)))
    (let ((data (file->list read file)))
      (let ((unit-normal-vec
             (unitv
              (normal-verctor
               (2dvector (list->vector (ref data 1)) (list->vector (ref data 0)))
               (2dvector (list->vector (ref data 1)) (list->vector (ref data 2)))))))
        (format #t "~{ ~{ ~@{~14,9f ~} ~} ~%~}"
                (map vector->list
                     (vector->list
                      (add2dvec
                       (rotate (* pi/180 30.0)
                               unit-normal-vec
                               (list->vector (map list->vector (drop data 3)))
                               #(-2 0  0)
                               )
                       (vector-ref (prod2dvec (vector unit-normal-vec) #(-1.5 -1.5 -1.5)) 0))
                      )))))))


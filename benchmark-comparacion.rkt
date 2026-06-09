#lang racket
(require 2htdp/image)
(require racket/future)
(require "filtros-comunes.rkt")
(require "filtros-mixtos.rkt")

; =====================================================
; benchmark-comparacion.rkt
;
; Compara 4 estrategias para cada filtro:
;
;   seq-func  : funcional pura secuencial  (baseline)
;   seq-mixed : mixta secuencial           (filtros-mixtos)
;   par-func  : funcional pura paralela    (baseline)
;   par-mixed : mixta paralela             (filtros-mixtos)
;
; Las implementaciones baseline (seq-func, par-func) se
; definen aquí inline para no depender de los módulos de
; fase 1/2 (que tienen código de top-level con efectos).
; =====================================================


; ----- Baselines secuenciales (funcionales puras) -------
; Réplica exacta de filtros-secuenciales.rkt

(define (sf-grayscale px) (map pixel-grayscale px))
(define (sf-sepia     px) (map pixel-sepia     px))
(define (sf-negative  px) (map pixel-negative  px))

(define (sf-gaussian px w h)
  (convolve px w h kernel-gauss kernel-gauss kernel-gauss))

(define (sf-edges px w h)
  (define gray-px (sf-grayscale px))
  (define vec (vector->immutable-vector (list->vector gray-px)))
  (define get (make-getter vec w h))
  (map (lambda (i)
         (define x   (modulo   i w))
         (define y   (quotient i w))
         (define p   (vector-ref vec i))
         (define gx  (apply-kernel get x y kernel-sobel-x color-red))
         (define gy  (apply-kernel get x y kernel-sobel-y color-red))
         (define mag (clamp (->int (sqrt (+ (* gx gx) (* gy gy)))) 0 255))
         (make-color mag mag mag (color-alpha p)))
       (build-list (* w h) values)))


; ----- Baselines paralelos (funcionales puros) ----------
; Réplica exacta de filtros-paralelos.rkt

(define (pf-split-into-n lst n)
  (define len (length lst))
  (define sz  (max 1 (inexact->exact (ceiling (/ len n)))))
  (let loop ([rem lst] [acc '()] [rem-len len])
    (if (null? rem)
        (reverse acc)
        (let ([take-n (min sz rem-len)])
          (define-values [chunk rest] (split-at rem take-n))
          (loop rest (cons chunk acc) (- rem-len take-n))))))

(define (pf-map-filter pixel-fn px n)
  (define futs (map (lambda (chunk)
                      (future (lambda () (map pixel-fn chunk))))
                    (pf-split-into-n px n)))
  (apply append (map touch futs)))

(define (pf-convolve px w h kr kg kb n)
  (define vec      (vector->immutable-vector (list->vector px)))
  (define get      (make-getter vec w h))
  (define rows/t   (max 1 (inexact->exact (ceiling (/ h n)))))
  (define n-actual (min n h))
  (define futs
    (map (lambda (t)
           (define y0 (* t rows/t))
           (define y1 (min h (+ y0 rows/t)))
           (future (lambda ()
             (map (lambda (i)
                    (define x (modulo   i w))
                    (define y (+ y0 (quotient i w)))
                    (define p (vector-ref vec (+ (* y w) x)))
                    (make-color (clamp (->int (apply-kernel get x y kr color-red))   0 255)
                                (clamp (->int (apply-kernel get x y kg color-green)) 0 255)
                                (clamp (->int (apply-kernel get x y kb color-blue))  0 255)
                                (color-alpha p)))
                  (build-list (* (- y1 y0) w) values)))))
         (build-list n-actual values)))
  (apply append (map touch futs)))

(define (pf-grayscale px n)   (pf-map-filter pixel-grayscale px n))
(define (pf-sepia     px n)   (pf-map-filter pixel-sepia     px n))
(define (pf-negative  px n)   (pf-map-filter pixel-negative  px n))
(define (pf-gaussian  px w h n)
  (pf-convolve px w h kernel-gauss kernel-gauss kernel-gauss n))

(define (pf-edges px w h n)
  (define gray-px  (pf-map-filter pixel-grayscale px n))
  (define vec      (vector->immutable-vector (list->vector gray-px)))
  (define get      (make-getter vec w h))
  (define rows/t   (max 1 (inexact->exact (ceiling (/ h n)))))
  (define n-actual (min n h))
  (define futs
    (map (lambda (t)
           (define y0 (* t rows/t))
           (define y1 (min h (+ y0 rows/t)))
           (future (lambda ()
             (map (lambda (i)
                    (define x   (modulo   i w))
                    (define y   (+ y0 (quotient i w)))
                    (define p   (vector-ref vec (+ (* y w) x)))
                    (define gx  (apply-kernel get x y kernel-sobel-x color-red))
                    (define gy  (apply-kernel get x y kernel-sobel-y color-red))
                    (define mag (clamp (->int (sqrt (+ (* gx gx) (* gy gy)))) 0 255))
                    (make-color mag mag mag (color-alpha p)))
                  (build-list (* (- y1 y0) w) values)))))
         (build-list n-actual values)))
  (apply append (map touch futs)))


; ----- Utilidad de formato ------------------------------

(define (fmt-ratio a b)
  (if (> b 0.001)
      (format "~ax" (real->decimal-string (/ a b) 1))
      "N/A"))

(define (hline w) (displayln (make-string w #\-)))


; ----- Comparación secuencial ---------------------------

(define (report-sequential px w h)
  (displayln "\n  SECUENCIAL")
  (displayln (format "  ~a ~a ~a ~a"
                     (~a "filtro"    #:min-width 14)
                     (~a "func(ms)"  #:min-width 10)
                     (~a "mixto(ms)" #:min-width 10)
                     "speedup"))
  (hline 50)
  (for ([row (list
    (list "grayscale"   (lambda () (sf-grayscale px))       (lambda () (filter-grayscale-fast px)))
    (list "sepia"       (lambda () (sf-sepia     px))       (lambda () (filter-sepia-fast     px)))
    (list "negative"    (lambda () (sf-negative  px))       (lambda () (filter-negative-fast  px)))
    (list "gaussian"    (lambda () (sf-gaussian  px w h))   (lambda () (filter-gaussian-fast  px w h)))
    (list "edge detect" (lambda () (sf-edges     px w h))   (lambda () (filter-edges-fast     px w h))))])
    (define name (first row))
    (define-values [_f tf] (timer (second row)))
    (define-values [_m tm] (timer (third  row)))
    (displayln (format "  ~a ~a ~a ~a"
                       (~a name       #:min-width 14)
                       (~a (round tf) #:min-width 10)
                       (~a (round tm) #:min-width 10)
                       (fmt-ratio tf tm)))))


; ----- Comparación paralela (por conteo de hilos) ------

(define (report-parallel px w h)
  (for ([n '(1 2 4 8 16)])
    (displayln (format "\n  PARALELO — ~a hilo(s)" n))
    (displayln (format "  ~a ~a ~a ~a ~a ~a"
                       (~a "filtro"      #:min-width 14)
                       (~a "seq-f(ms)"  #:min-width 10)
                       (~a "seq-m(ms)"  #:min-width 10)
                       (~a "par-f(ms)"  #:min-width 10)
                       (~a "par-m(ms)"  #:min-width 10)
                       "par-f/par-m"))
    (hline 72)
    (for ([row (list
      (list "grayscale"
            (lambda () (sf-grayscale px))
            (lambda () (filter-grayscale-fast px))
            (lambda () (pf-grayscale px n))
            (lambda () (par-filter-grayscale-fast px n)))
      (list "sepia"
            (lambda () (sf-sepia px))
            (lambda () (filter-sepia-fast px))
            (lambda () (pf-sepia px n))
            (lambda () (par-filter-sepia-fast px n)))
      (list "negative"
            (lambda () (sf-negative px))
            (lambda () (filter-negative-fast px))
            (lambda () (pf-negative px n))
            (lambda () (par-filter-negative-fast px n)))
      (list "gaussian"
            (lambda () (sf-gaussian px w h))
            (lambda () (filter-gaussian-fast px w h))
            (lambda () (pf-gaussian px w h n))
            (lambda () (par-filter-gaussian-fast px w h n)))
      (list "edge detect"
            (lambda () (sf-edges px w h))
            (lambda () (filter-edges-fast px w h))
            (lambda () (pf-edges px w h n))
            (lambda () (par-filter-edges-fast px w h n))))])
      (define name (first row))
      (define-values [_sf t-sf] (timer (second row)))
      (define-values [_sm t-sm] (timer (third  row)))
      (define-values [_pf t-pf] (timer (fourth row)))
      (define-values [_pm t-pm] (timer (fifth  row)))
      (displayln (format "  ~a ~a ~a ~a ~a ~a"
                         (~a name          #:min-width 14)
                         (~a (round t-sf)  #:min-width 10)
                         (~a (round t-sm)  #:min-width 10)
                         (~a (round t-pf)  #:min-width 10)
                         (~a (round t-pm)  #:min-width 10)
                         (fmt-ratio t-pf t-pm))))))


; ----- Runner principal ---------------------------------

(define (run-comparison label img)
  (define w  (image-width  img))
  (define h  (image-height img))
  (define px (image->pixels img))
  (displayln (format "\n╔══════════════════════════════════════════════════════╗"))
  (displayln (format "  ~a  ~ax~a = ~a px" label w h (* w h)))
  (displayln (format "╚══════════════════════════════════════════════════════╝"))
  (report-sequential px w h)
  (report-parallel   px w h))

(displayln "\n=== BENCHMARK: func vs mixto — secuencial y paralelo ===")
;(run-comparison "cat.png    (small) " img-cat)
; Descomentar para imágenes más grandes:
;(run-comparison "cats2.png  (medium)" img-cats2)
(run-comparison "new-york   (large) " img-ny)

#lang racket
(require 2htdp/image)
(require "filtros-comunes.rkt")

; =====================================================
; FASE 1 — Filtros Secuenciales de Imagen
;
; Image bank:
;   cat.png            151 ×  227 =    34 277 px  (small)
;   cats2.png         1264 ×  845 = 1 068 080 px  (medium)
;   new-york-large    4000 × 1421 = 5 684 000 px  (large)
;
; Filters applied (over a 1D list of RGBA color pixels):
;   - grayscale     (map, puramente funcional)
;   - sepia         (map, puramente funcional)
;   - negativo      (map, puramente funcional)
;   - gaussian blur (3×3 convolution, requires vector)
;   - edge detect   (Sobel 3×3,      requires vector)
; =====================================================


; ----- Sequential filters (map over pixel list) ----

(define (filter-grayscale px) (map pixel-grayscale px))
(define (filter-sepia     px) (map pixel-sepia     px))
(define (filter-negative  px) (map pixel-negative  px))

(define (filter-gaussian px w h)
  (convolve px w h kernel-gauss kernel-gauss kernel-gauss))

; Sobel edge detection on grayscale channel.
; Gx detects horizontal changes, Gy vertical ones.
; Magnitude = sqrt(Gx² + Gy²), normalized to [0,255].
(define (filter-edges px w h)
  (define gray-px (filter-grayscale px))
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


; ----- Benchmark: list-ref vs vector-ref ------------

(define demo-n   100000)
(define demo-ops 3000)
(define demo-idx (build-list demo-ops (lambda (_) (random demo-n))))
(define demo-L   (build-list demo-n values))
(define demo-V   (list->vector demo-L))

(define-values [_r1 t-list-ref]
  (timer (lambda () (for-each (lambda (i) (list-ref demo-L i)) demo-idx))))

(define-values [_r2 t-vector-ref]
  (timer (lambda () (for-each (lambda (i) (vector-ref demo-V i)) demo-idx))))

(displayln "\n--- Benchmark: acceso aleatorio (list-ref vs vector-ref) ---")
(displayln (format "  lista  (~a ops, N=~a): ~a ms" demo-ops demo-n (round t-list-ref)))
(displayln (format "  vector (~a ops, N=~a): ~a ms" demo-ops demo-n (round t-vector-ref)))
(displayln (format "  speedup: ~ax" (round (/ t-list-ref (max t-vector-ref 0.001)))))
(displayln   "  => convolution on a list would be O(N^2); with vector it stays O(N).")


; ----- Filter timings per image ---------------------

(define (report-image label img)
  (define w  (image-width  img))
  (define h  (image-height img))
  (define px (image->pixels img))
  (displayln (format "\n[~a]  ~a x ~a = ~a px" label w h (* w h)))
  (measure "grayscale"   (lambda () (filter-grayscale px)))
  (measure "sepia"       (lambda () (filter-sepia     px)))
  (measure "negative"    (lambda () (filter-negative  px)))
  (measure "gaussian"    (lambda () (filter-gaussian  px w h)))
  (measure "edge detect" (lambda () (filter-edges     px w h))))

(displayln "\n=== Tiempos secuenciales (ms) ===")
(report-image "cat.png    (small)  " img-cat)
;(report-image "cats2.png  (medium) " img-cats2)
;(report-image "new-york   (large)  " img-ny)


; ----- Visual output --------------------------------

(pixels->image (filter-grayscale (image->pixels img-cat))
               (image-width img-cat) (image-height img-cat))

;(pixels->image (filter-sepia (image->pixels img-cat))
;               (image-width img-cat) (image-height img-cat))

;(pixels->image (filter-negative (image->pixels img-cat))
;               (image-width img-cat) (image-height img-cat))

;(pixels->image (filter-gaussian (image->pixels img-cat) (image-width img-cat) (image-height img-cat))
;               (image-width img-cat) (image-height img-cat))

;(pixels->image (filter-edges (image->pixels img-cat) (image-width img-cat) (image-height img-cat))
;               (image-width img-cat) (image-height img-cat))

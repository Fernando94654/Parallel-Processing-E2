#lang racket
(require 2htdp/image)
(require racket/future)
(require "filtros-comunes.rkt")

; =====================================================
; FASE 2 — Filtros Paralelos con futures + touch
;
; Estrategia: particionamiento por franjas de filas
;
;   imagen completa
;   ┌────────────────┐
;   │  fila 0..k-1   │  → future 0
;   ├────────────────┤
;   │  fila k..2k-1  │  → future 1
;   ├────────────────┤
;   │      ...       │
;   └────────────────┘
;
; Filtros map (grayscale, sepia, negative):
;   La lista de píxeles se divide en N trozos iguales.
;   Cada future aplica (map pixel-fn chunk).
;   Resultado: (apply append (map touch futs)).
;
; Filtros de convolución (gaussian, edges):
;   El vector de entrada es compartido (solo lectura,
;   thread-safe en Racket). Cada future genera únicamente
;   las filas que le corresponden; no se necesitan locks.
;   Resultado: se concatenan las sublistas de cada future.
;
; En ambos casos la unificación es un simple append;
; no se requiere reducción adicional porque las franjas
; son disjuntas y están ordenadas.
; =====================================================


; ----- Particionamiento: lista → N trozos -----------

(define (split-into-n lst n)
  (define len (length lst))
  (define sz  (max 1 (inexact->exact (ceiling (/ len n)))))
  (let loop ([rem lst] [acc '()] [rem-len len])
    (if (null? rem)
        (reverse acc)
        (let ([take-n (min sz rem-len)])
          (define-values [chunk rest] (split-at rem take-n))
          (loop rest (cons chunk acc) (- rem-len take-n))))))


; ----- Filtros map en paralelo ----------------------
; Divide la lista en N chunks → un future por chunk →
; toca todos con touch → concatena con append.

(define (par-map-filter pixel-fn px n)
  (define futs (map (lambda (chunk)
                      (future (lambda () (map pixel-fn chunk))))
                    (split-into-n px n)))
  (apply append (map touch futs)))


; ----- Convolución en paralelo ----------------------
; El vector completo se comparte entre futures (read-only).
; Cada future procesa su rango de filas [y0, y1) de forma
; independiente — no hay escrituras compartidas.

(define (par-convolve px w h kr kg kb n)
  (define vec      (list->vector px))
  (define get      (make-getter vec w h))
  (define rows/t   (max 1 (inexact->exact (ceiling (/ h n)))))
  (define n-actual (min n h))
  (define futs
    (for/list ([t (in-range n-actual)])
      (define y0 (* t rows/t))
      (define y1 (min h (+ y0 rows/t)))
      (future (lambda ()
        (for*/list ([y (in-range y0 y1)] [x (in-range w)])
          (define p (vector-ref vec (+ (* y w) x)))
          (make-color (clamp (->int (apply-kernel get x y kr color-red))   0 255)
                      (clamp (->int (apply-kernel get x y kg color-green)) 0 255)
                      (clamp (->int (apply-kernel get x y kb color-blue))  0 255)
                      (color-alpha p)))))))
  (apply append (map touch futs)))


; ----- Wrappers paralelos (misma firma que fase 1 + n)

(define (par-filter-grayscale px n)   (par-map-filter pixel-grayscale px n))
(define (par-filter-sepia     px n)   (par-map-filter pixel-sepia     px n))
(define (par-filter-negative  px n)   (par-map-filter pixel-negative  px n))

(define (par-filter-gaussian px w h n)
  (par-convolve px w h kernel-gauss kernel-gauss kernel-gauss n))

(define (par-filter-edges px w h n)
  (define gray-px  (par-map-filter pixel-grayscale px n))
  (define vec      (list->vector gray-px))
  (define get      (make-getter vec w h))
  (define rows/t   (max 1 (inexact->exact (ceiling (/ h n)))))
  (define n-actual (min n h))
  (define futs
    (for/list ([t (in-range n-actual)])
      (define y0 (* t rows/t))
      (define y1 (min h (+ y0 rows/t)))
      (future (lambda ()
        (for*/list ([y (in-range y0 y1)] [x (in-range w)])
          (define p   (vector-ref vec (+ (* y w) x)))
          (define gx  (apply-kernel get x y kernel-sobel-x color-red))
          (define gy  (apply-kernel get x y kernel-sobel-y color-red))
          (define mag (clamp (->int (sqrt (+ (* gx gx) (* gy gy)))) 0 255))
          (make-color mag mag mag (color-alpha p)))))))
  (apply append (map touch futs)))


; ----- Benchmark paralelo: 1 2 4 8 16 hilos --------

(define thread-counts '(1 2 4 8 16))

(define (report-image-parallel label img)
  (define w  (image-width  img))
  (define h  (image-height img))
  (define px (image->pixels img))
  (displayln (format "\n[~a]  ~a x ~a = ~a px" label w h (* w h)))
  (for-each (lambda (n)
    (displayln (format "\n  -- ~a hilo(s) --" n))
    (measure "grayscale"   (lambda () (par-filter-grayscale px n)))
    (measure "sepia"       (lambda () (par-filter-sepia     px n)))
    (measure "negative"    (lambda () (par-filter-negative  px n)))
    (measure "gaussian"    (lambda () (par-filter-gaussian  px w h n)))
    (measure "edge detect" (lambda () (par-filter-edges     px w h n))))
  thread-counts))

(displayln "\n=== Tiempos paralelos (ms) ===")
(report-image-parallel "cat.png    (small)  " img-cat)
;(report-image-parallel "cats2.png  (medium) " img-cats2)
;(report-image-parallel "new-york   (large)  " img-ny)

#lang racket
(require 2htdp/image)
(require racket/future)
(require "filtros-comunes.rkt")

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

; ----- Wrappers paralelos (misma firma que fase 1 + n)

(define (par-filter-grayscale px n)   (par-map-filter pixel-grayscale px n))
(define (par-filter-sepia     px n)   (par-map-filter pixel-sepia     px n))
(define (par-filter-negative  px n)   (par-map-filter pixel-negative  px n))

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
    )
  thread-counts))

(displayln "\n=== Tiempos paralelos (ms) ===")
(report-image-parallel "cat.png    (small)  " img-cat)
;(report-image-parallel "cats2.png  (medium) " img-cats2)
;(report-image-parallel "new-york   (large)  " img-ny)

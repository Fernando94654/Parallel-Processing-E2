#lang racket
(require 2htdp/image)
(require racket/future)
(require "filtros-comunes.rkt")

(provide (all-defined-out))


(define (filter-grayscale-fast px) (for/list ([p (in-list px)]) (pixel-grayscale p)))
(define (filter-sepia-fast     px) (for/list ([p (in-list px)]) (pixel-sepia     p)))
(define (filter-negative-fast  px) (for/list ([p (in-list px)]) (pixel-negative  p)))


; Converts input to immutable vector once (O(N)) so every future gets O(1) reads.
; Each future returns its own local list — no shared mutation, fully future-safe.
(define (par-map-filter-fast pixel-fn px n)
  (define N   (length px))
  (define vec (vector->immutable-vector (list->vector px)))
  (define sz  (max 1 (inexact->exact (ceiling (/ N n)))))
  (define futs
    (for/list ([t (in-range n)])
      (define start (* t sz))
      (define end   (min N (+ start sz)))
      (if (>= start N)
          #f
          (future (lambda ()
            (for/list ([i (in-range start end)])
              (pixel-fn (vector-ref vec i))))))))
  (apply append (map (lambda (f) (if f (touch f) '())) futs)))


(define (par-filter-grayscale-fast px n) (par-map-filter-fast pixel-grayscale px n))
(define (par-filter-sepia-fast     px n) (par-map-filter-fast pixel-sepia     px n))
(define (par-filter-negative-fast  px n) (par-map-filter-fast pixel-negative  px n))


(define (report-image-mixed label img)
  (define px (image->pixels img))
  (displayln (format "\n~a" label))
  (for-each (lambda (n)
    (displayln (format "  ~a hilo(s):" n))
    (measure "grayscale" (lambda () (par-filter-grayscale-fast px n)))
    (measure "sepia"     (lambda () (par-filter-sepia-fast     px n)))
    (measure "negative"  (lambda () (par-filter-negative-fast  px n))))
  '(1 2 4 8 16)))

(module+ main
  (displayln "=== mixto ===")
  (report-image-mixed "cat.png" img-cat)
  ;(report-image-mixed "cats2.png" img-cats2)
  ;(report-image-mixed "new-york" img-ny)
  )

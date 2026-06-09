#lang racket
(require 2htdp/image)
(require racket/future)
(require "filtros-comunes.rkt")


(define (split-into-n lst n)
  (define len (length lst))
  (define sz  (max 1 (inexact->exact (ceiling (/ len n)))))
  (let loop ([rem lst] [acc '()] [rem-len len])
    (if (null? rem)
        (reverse acc)
        (let ([take-n (min sz rem-len)])
          (define-values [chunk rest] (split-at rem take-n))
          (loop rest (cons chunk acc) (- rem-len take-n))))))


(define (par-map-filter pixel-fn px n)
  (define futs (map (lambda (chunk)
                      (future (lambda () (map pixel-fn chunk))))
                    (split-into-n px n)))
  (apply append (map touch futs)))


(define (par-filter-grayscale px n) (par-map-filter pixel-grayscale px n))
(define (par-filter-sepia     px n) (par-map-filter pixel-sepia     px n))
(define (par-filter-negative  px n) (par-map-filter pixel-negative  px n))


(define (report-threads px ns)
  (if (null? ns) (void)
      (begin
        (displayln (format "  ~a hilo(s):" (car ns)))
        (measure "grayscale" (lambda () (par-filter-grayscale px (car ns))))
        (measure "sepia"     (lambda () (par-filter-sepia     px (car ns))))
        (measure "negative"  (lambda () (par-filter-negative  px (car ns))))
        (report-threads px (cdr ns)))))

(define (report-image-parallel label img)
  (define px (image->pixels img))
  (displayln (format "\n~a" label))
  (report-threads px '(1 2 4 8 16)))

(displayln "=== paralelo ===")
(report-image-parallel "cat.png" img-cat)
;(report-image-parallel "cats2.png" img-cats2)
;(report-image-parallel "new-york" img-ny)

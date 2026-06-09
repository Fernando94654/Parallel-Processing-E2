#lang racket
(require 2htdp/image)
(require "filtros-comunes.rkt")


(define (filter-grayscale px) (map pixel-grayscale px))
(define (filter-sepia     px) (map pixel-sepia     px))
(define (filter-negative  px) (map pixel-negative  px))


; ----- recursión pura ----------------------------------

(define (filter-grayscale-rec px)
  (if (null? px) '()
      (cons (pixel-grayscale (car px))
            (filter-grayscale-rec (cdr px)))))

(define (filter-sepia-rec px)
  (if (null? px) '()
      (cons (pixel-sepia (car px))
            (filter-sepia-rec (cdr px)))))

(define (filter-negative-rec px)
  (if (null? px) '()
      (cons (pixel-negative (car px))
            (filter-negative-rec (cdr px)))))


; ----- benchmark: list-ref vs vector-ref ---------------

(define demo-n   100000)
(define demo-ops 3000)
(define demo-idx (build-list demo-ops (lambda (_) (random demo-n))))
(define demo-L   (build-list demo-n values))
(define demo-V   (list->vector demo-L))

(define-values [_r1 t-list]   (timer (lambda () (for-each (lambda (i) (list-ref   demo-L i)) demo-idx))))
(define-values [_r2 t-vector] (timer (lambda () (for-each (lambda (i) (vector-ref demo-V i)) demo-idx))))

(displayln (format "list-ref=~ams  vector-ref=~ams  speedup=~ax"
                   (round t-list) (round t-vector)
                   (round (/ t-list (max t-vector 0.001)))))


; ----- reporte por imagen ------------------------------

(define (report-image label img)
  (define px (image->pixels img))
  (displayln (format "\n~a" label))
  (displayln "  map:")
  (measure "grayscale" (lambda () (filter-grayscale px)))
  (measure "sepia"     (lambda () (filter-sepia     px)))
  (measure "negative"  (lambda () (filter-negative  px)))
  (displayln "  recursion:")
  (measure "grayscale" (lambda () (filter-grayscale-rec px)))
  (measure "sepia"     (lambda () (filter-sepia-rec     px)))
  (measure "negative"  (lambda () (filter-negative-rec  px))))

(displayln "=== secuencial ===")
(report-image "cat.png" img-cat)
;(report-image "cats2.png" img-cats2)
;(report-image "new-york" img-ny)


; ----- salida visual -----------------------------------

(pixels->image (filter-grayscale (image->pixels img-cat))
               (image-width img-cat) (image-height img-cat))

;(pixels->image (filter-sepia (image->pixels img-cat))
;               (image-width img-cat) (image-height img-cat))

;(pixels->image (filter-negative (image->pixels img-cat))
;               (image-width img-cat) (image-height img-cat))

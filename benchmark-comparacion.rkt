#lang racket
(require 2htdp/image)
(require racket/future)
(require "filtros-comunes.rkt")
(require "filtros-mixtos.rkt")


; Baselines funcionales secuenciales
(define (sf-grayscale px) (map pixel-grayscale px))
(define (sf-sepia     px) (map pixel-sepia     px))
(define (sf-negative  px) (map pixel-negative  px))

; Baselines funcionales paralelos
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

(define (pf-grayscale px n) (pf-map-filter pixel-grayscale px n))
(define (pf-sepia     px n) (pf-map-filter pixel-sepia     px n))
(define (pf-negative  px n) (pf-map-filter pixel-negative  px n))


(define (fmt-speedup a b)
  (format "x~a" (real->decimal-string (/ a (max b 0.001)) 1)))

(define (report-row name f-func f-mixed)
  (define-values [_f tf] (timer f-func))
  (define-values [_m tm] (timer f-mixed))
  (displayln (format "    ~a  func=~a  mixto=~a  ~a"
                     (~a name #:min-width 10)
                     (~a (round tf) #:min-width 6)
                     (~a (round tm) #:min-width 6)
                     (fmt-speedup tf tm))))

(define (report-par-row name f-sf f-sm f-pf f-pm)
  (define-values [_sf t-sf] (timer f-sf))
  (define-values [_sm t-sm] (timer f-sm))
  (define-values [_pf t-pf] (timer f-pf))
  (define-values [_pm t-pm] (timer f-pm))
  (displayln (format "    ~a  sf=~a  sm=~a  pf=~a  pm=~a  ~a"
                     (~a name #:min-width 10)
                     (~a (round t-sf) #:min-width 6)
                     (~a (round t-sm) #:min-width 6)
                     (~a (round t-pf) #:min-width 6)
                     (~a (round t-pm) #:min-width 6)
                     (fmt-speedup t-pf t-pm))))


(define (run-comparison label img)
  (define px (image->pixels img))
  (displayln (format "\n~a" label))

  (displayln "  secuencial:")
  (report-row "grayscale" (lambda () (sf-grayscale px)) (lambda () (filter-grayscale-fast px)))
  (report-row "sepia"     (lambda () (sf-sepia     px)) (lambda () (filter-sepia-fast     px)))
  (report-row "negative"  (lambda () (sf-negative  px)) (lambda () (filter-negative-fast  px)))

  (for-each (lambda (n)
    (displayln (format "  ~a hilo(s):" n))
    (report-par-row "grayscale"
      (lambda () (sf-grayscale px)) (lambda () (filter-grayscale-fast px))
      (lambda () (pf-grayscale px n)) (lambda () (par-filter-grayscale-fast px n)))
    (report-par-row "sepia"
      (lambda () (sf-sepia px)) (lambda () (filter-sepia-fast px))
      (lambda () (pf-sepia px n)) (lambda () (par-filter-sepia-fast px n)))
    (report-par-row "negative"
      (lambda () (sf-negative px)) (lambda () (filter-negative-fast px))
      (lambda () (pf-negative px n)) (lambda () (par-filter-negative-fast px n))))
  '(1 2 4 8 16)))


(displayln "=== comparacion func vs mixto ===")
(run-comparison "cat.png" img-cat)
;(run-comparison "cats2.png" img-cats2)
;(run-comparison "new-york" img-ny)

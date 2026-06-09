#lang racket
(require 2htdp/image)
(require racket/future)
(require "filtros-comunes.rkt")
(require "filtros-mixtos.rkt")

; parallel functional baseline (split-into-n approach)
(define (pf-split lst n)
  (define len (length lst))
  (define sz  (max 1 (inexact->exact (ceiling (/ len n)))))
  (let loop ([rem lst] [acc '()] [rem-len len])
    (if (null? rem) (reverse acc)
        (let ([take-n (min sz rem-len)])
          (define-values [chunk rest] (split-at rem take-n))
          (loop rest (cons chunk acc) (- rem-len take-n))))))

(define (pf pixel-fn px n)
  (apply append
    (map touch
      (map (lambda (chunk) (future (lambda () (map pixel-fn chunk))))
           (pf-split px n)))))

(define filters
  (list (list "grayscale" pixel-grayscale filter-grayscale-fast par-filter-grayscale-fast)
        (list "sepia"     pixel-sepia     filter-sepia-fast     par-filter-sepia-fast)
        (list "negative"  pixel-negative  filter-negative-fast  par-filter-negative-fast)))

(define (collect-image img-label img)
  (define px (image->pixels img))
  (for-each
    (lambda (f)
      (define fname    (first  f))
      (define pixel-fn (second f))
      (define seq-fast (third  f))
      (define par-fast (fourth f))
      (define-values [_sf t-sf] (timer (lambda () (map pixel-fn px))))
      (define-values [_sm t-sm] (timer (lambda () (seq-fast px))))
      (printf "~a,~a,sf,0,~a\n" img-label fname (round t-sf))
      (printf "~a,~a,sm,0,~a\n" img-label fname (round t-sm))
      (for-each (lambda (n)
        (define-values [_pf t-pf] (timer (lambda () (pf pixel-fn px n))))
        (define-values [_pm t-pm] (timer (lambda () (par-fast px n))))
        (printf "~a,~a,pf,~a,~a\n" img-label fname n (round t-pf))
        (printf "~a,~a,pm,~a,~a\n" img-label fname n (round t-pm)))
      '(1 2 4 8 16)))
  filters))

(printf "image,filter,approach,threads,time_ms\n")
(collect-image "cat"      img-cat)
(collect-image "cats2"    img-cats2)
(collect-image "new-york" img-ny)

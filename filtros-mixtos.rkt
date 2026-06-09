#lang racket
(require 2htdp/image)
(require racket/future)
(require "filtros-comunes.rkt")

(provide (all-defined-out))

; =====================================================
; FASE 3 — Filtros Mixtos (Funcional + Imperativo)
;
;  filter-*-fast      : for/list con in-list (iterador tipado)
;                       → evita dispatch genérico de map
;
;  par-map-filter-fast: vector compartido de salida; futures
;                       escriben en rangos disjuntos [i0,i1)
;                       → elimina split-into-n, apply append
;
; =====================================================

;  Filtros map secuenciales con for/list 

(define (filter-grayscale-fast px) (for/list ([p (in-list px)]) (pixel-grayscale p)))
(define (filter-sepia-fast     px) (for/list ([p (in-list px)]) (pixel-sepia     p)))
(define (filter-negative-fast  px) (for/list ([p (in-list px)]) (pixel-negative  p)))

; Filtro map paralelo con vector compartido
;
; Reemplaza: split-into-n + (apply append (map touch futs))
;
; El vector de entrada se convierte una sola vez (O(N)).
; Cada future recibe el rango [start, end) y escribe en
; posiciones disjuntas de `out` — sin locks necesarios.

(define (par-map-filter-fast pixel-fn px n)
  (define N   (length px))
  (define vec (list->vector px))
  (define out (make-vector N))
  (define sz  (max 1 (inexact->exact (ceiling (/ N n)))))
  (define futs
    (for/list ([t (in-range n)])
      (define start (* t sz))
      (define end   (min N (+ start sz)))
      (if (>= start N)
          #f
          (future (lambda ()
            (for ([i (in-range start end)])
              (vector-set! out i (pixel-fn (vector-ref vec i)))))))))
  (for ([f (in-list futs)] #:when f) (touch f))
  (vector->list out))


;  Wrappers paralelos 

(define (par-filter-grayscale-fast px n)   (par-map-filter-fast pixel-grayscale px n))
(define (par-filter-sepia-fast     px n)   (par-map-filter-fast pixel-sepia     px n))
(define (par-filter-negative-fast  px n)   (par-map-filter-fast pixel-negative  px n))


; Benchmark mixto: 1 2 4 8 16 hilos 

(define (report-image-mixed label img)
  (define w  (image-width  img))
  (define h  (image-height img))
  (define px (image->pixels img))
  (displayln (format "\n[~a]  ~a x ~a = ~a px" label w h (* w h)))
  (for-each (lambda (n)
    (displayln (format "\n  -- ~a hilo(s) --" n))
    (measure "grayscale"   (lambda () (par-filter-grayscale-fast px n)))
    (measure "sepia"       (lambda () (par-filter-sepia-fast     px n)))
    (measure "negative"    (lambda () (par-filter-negative-fast  px n)))
    )
  '(1 2 4 8 16)))

; Corre solo cuando se ejecuta este archivo directamente
(module+ main
  (displayln "\n=== Tiempos mixtos (ms) ===")
  (report-image-mixed "cat.png    (small)  " img-cat)
  ;(report-image-mixed "cats2.png  (medium) " img-cats2)
  ;(report-image-mixed "new-york   (large)  " img-ny)
  )

#lang racket
(require 2htdp/image)
(require racket/future)
(require "filtros-comunes.rkt")

(provide (all-defined-out))

; =====================================================
; FASE 3 — Filtros Mixtos (Funcional + Imperativo)
;
; Optimizaciones respecto a las versiones funcionales puras:
;
;  apply-kernel-fast  : for/sum en lugar de (apply + (map ...))
;                       → elimina ~9 cons-cells por píxel por canal
;
;  convolve-fast      : make-vector + for + vector-set!
;                       → elimina build-list (N índices) y
;                         map (N cons-cells de salida)
;
;  filter-*-fast      : for/list con in-list (iterador tipado)
;                       → evita dispatch genérico de map
;
;  par-map-filter-fast: vector compartido de salida; futures
;                       escriben en rangos disjuntos [i0,i1)
;                       → elimina split-into-n, apply append
;
;  par-convolve-fast  : for* sobre (y,x) en lugar de
;                       build-list + map dentro de cada future
;                       → mismo vector compartido de salida
; =====================================================


; ----- 1. Kernel application sin listas intermedias -----
;
; Reemplaza: (apply + (map (lambda (off k) (* k (extractor ...)))
;                          offsets3x3 kernel))
;
; for/sum con dos in-list paralelos itera sin alocar la lista
; de productos. Firma idéntica a apply-kernel.

(define (apply-kernel-fast get x y kernel extractor)
  (for/sum ([off (in-list offsets3x3)]
            [k   (in-list kernel)])
    (* k (extractor (get (+ x (car off))
                         (+ y (cadr off)))))))


; ----- 2. Convolución secuencial con vector de salida ----
;
; Reemplaza: (map (lambda (i) ...) (build-list (* w h) values))
;
; make-vector pre-aloca N slots; for+in-range no crea lista de
; índices; vector->list al final satisface pixels->image.

(define (convolve-fast px w h kr kg kb)
  (define N   (* w h))
  (define vec (vector->immutable-vector (list->vector px)))
  (define get (make-getter vec w h))
  (define out (make-vector N))
  (for ([i (in-range N)])
    (define x (modulo   i w))
    (define y (quotient i w))
    (define p (vector-ref vec i))
    (vector-set! out i
      (make-color (clamp (->int (apply-kernel-fast get x y kr color-red  )) 0 255)
                  (clamp (->int (apply-kernel-fast get x y kg color-green)) 0 255)
                  (clamp (->int (apply-kernel-fast get x y kb color-blue )) 0 255)
                  (color-alpha p))))
  (vector->list out))


; ----- 3. Filtros map secuenciales con for/list ----------

(define (filter-grayscale-fast px) (for/list ([p (in-list px)]) (pixel-grayscale p)))
(define (filter-sepia-fast     px) (for/list ([p (in-list px)]) (pixel-sepia     p)))
(define (filter-negative-fast  px) (for/list ([p (in-list px)]) (pixel-negative  p)))

(define (filter-gaussian-fast px w h)
  (convolve-fast px w h kernel-gauss kernel-gauss kernel-gauss))


; ----- 4. Detección de bordes secuencial -----------------

(define (filter-edges-fast px w h)
  (define N       (* w h))
  (define gray-px (filter-grayscale-fast px))
  (define vec     (vector->immutable-vector (list->vector gray-px)))
  (define get     (make-getter vec w h))
  (define out     (make-vector N))
  (for ([i (in-range N)])
    (define x   (modulo   i w))
    (define y   (quotient i w))
    (define p   (vector-ref vec i))
    (define gx  (apply-kernel-fast get x y kernel-sobel-x color-red))
    (define gy  (apply-kernel-fast get x y kernel-sobel-y color-red))
    (define mag (clamp (->int (sqrt (+ (* gx gx) (* gy gy)))) 0 255))
    (vector-set! out i (make-color mag mag mag (color-alpha p))))
  (vector->list out))


; ----- 5. Filtro map paralelo con vector compartido ------
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


; ----- 6. Convolución paralela con vector compartido -----
;
; Reemplaza: build-list + map dentro de cada future,
;            más (apply append (map touch futs)).
;
; for* itera sobre (y,x) directamente — sin lista de índices.
; Los futures escriben en rangos de filas [y0,y1) disjuntos.

(define (par-convolve-fast px w h kr kg kb n)
  (define N        (* w h))
  (define vec      (vector->immutable-vector (list->vector px)))
  (define get      (make-getter vec w h))
  (define out      (make-vector N))
  (define rows/t   (max 1 (inexact->exact (ceiling (/ h n)))))
  (define n-actual (min n h))
  (define futs
    (for/list ([t (in-range n-actual)])
      (define y0 (* t rows/t))
      (define y1 (min h (+ y0 rows/t)))
      (future (lambda ()
        (for* ([y (in-range y0 y1)]
               [x (in-range w)])
          (define i (+ (* y w) x))
          (define p (vector-ref vec i))
          (vector-set! out i
            (make-color (clamp (->int (apply-kernel-fast get x y kr color-red  )) 0 255)
                        (clamp (->int (apply-kernel-fast get x y kg color-green)) 0 255)
                        (clamp (->int (apply-kernel-fast get x y kb color-blue )) 0 255)
                        (color-alpha p))))))))
  (for-each touch futs)
  (vector->list out))


; ----- 7. Wrappers paralelos (misma firma que fase 2 + n) -

(define (par-filter-grayscale-fast px n)   (par-map-filter-fast pixel-grayscale px n))
(define (par-filter-sepia-fast     px n)   (par-map-filter-fast pixel-sepia     px n))
(define (par-filter-negative-fast  px n)   (par-map-filter-fast pixel-negative  px n))

(define (par-filter-gaussian-fast px w h n)
  (par-convolve-fast px w h kernel-gauss kernel-gauss kernel-gauss n))


; ----- 8. Detección de bordes paralela -------------------

(define (par-filter-edges-fast px w h n)
  (define N        (* w h))
  (define gray-px  (par-map-filter-fast pixel-grayscale px n))
  (define vec      (vector->immutable-vector (list->vector gray-px)))
  (define get      (make-getter vec w h))
  (define out      (make-vector N))
  (define rows/t   (max 1 (inexact->exact (ceiling (/ h n)))))
  (define n-actual (min n h))
  (define futs
    (for/list ([t (in-range n-actual)])
      (define y0 (* t rows/t))
      (define y1 (min h (+ y0 rows/t)))
      (future (lambda ()
        (for* ([y (in-range y0 y1)]
               [x (in-range w)])
          (define i   (+ (* y w) x))
          (define p   (vector-ref vec i))
          (define gx  (apply-kernel-fast get x y kernel-sobel-x color-red))
          (define gy  (apply-kernel-fast get x y kernel-sobel-y color-red))
          (define mag (clamp (->int (sqrt (+ (* gx gx) (* gy gy)))) 0 255))
          (vector-set! out i (make-color mag mag mag (color-alpha p))))))))
  (for-each touch futs)
  (vector->list out))


; ----- Benchmark mixto: 1 2 4 8 16 hilos ----------------

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
    (measure "gaussian"    (lambda () (par-filter-gaussian-fast  px w h n)))
    (measure "edge detect" (lambda () (par-filter-edges-fast     px w h n))))
  '(1 2 4 8 16)))

; Corre solo cuando se ejecuta este archivo directamente
(module+ main
  (displayln "\n=== Tiempos mixtos (ms) ===")
  (report-image-mixed "cat.png    (small)  " img-cat)
  ;(report-image-mixed "cats2.png  (medium) " img-cats2)
  ;(report-image-mixed "new-york   (large)  " img-ny)
  )

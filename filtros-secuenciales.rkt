#lang racket
(require 2htdp/image)

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


; ----- Utils ----------------------------------------

(define (timer f)
  (collect-garbage)
  (define t0 (current-inexact-milliseconds))
  (define r  (f))
  (define t1 (current-inexact-milliseconds))
  (values r (- t1 t0)))

(define (clamp v lo hi) (max lo (min hi v)))
(define (->int x)       (inexact->exact (round x)))


; -----  Images bank --------------------------------

(define img-cat   (bitmap/file "images/cat.png"))
(define img-cats2 (bitmap/file "images/cats2.png"))
(define img-ny    (bitmap/file "images/new-york-large.jpg"))


; ----- Image conversion <-> RGBA list-----------------

(define (image->pixels img)    (image->color-list  img))
(define (pixels->image px w h) (color-list->bitmap px w h))


; ----- Functional filters per pixel (map)  ---

; grayscale: ITU-R BT.601 weighted luminance
(define (pixel-grayscale p)
  (define g (->int (+ (* 0.299 (color-red   p))
                      (* 0.587 (color-green p))
                      (* 0.114 (color-blue  p)))))
  (make-color g g g (color-alpha p)))

; sepia
(define (pixel-sepia p)
  (define r (color-red p)) (define g (color-green p)) (define b (color-blue p))
  (make-color (clamp (->int (+ (* 0.393 r) (* 0.769 g) (* 0.189 b))) 0 255)
              (clamp (->int (+ (* 0.349 r) (* 0.686 g) (* 0.168 b))) 0 255)
              (clamp (->int (+ (* 0.272 r) (* 0.534 g) (* 0.131 b))) 0 255)
              (color-alpha p)))

; negative: inversion of each panel
(define (pixel-negative p)
  (make-color (- 255 (color-red   p))
              (- 255 (color-green p))
              (- 255 (color-blue  p))
              (color-alpha p)))

(define (filter-grayscale px) (map pixel-grayscale px))
(define (filter-sepia     px) (map pixel-sepia     px))
(define (filter-negative  px) (map pixel-negative  px))


; ----- fConvolution filters 3×3 -----------------------
; -------------------------------------------------------

; Kernel offsets 3*3 (row-major: top-left → bottom-right)
(define offsets3x3
  '((-1 -1) (0 -1) (1 -1)
    (-1  0) (0  0) (1  0)
    (-1  1) (0  1) (1  1)))

; getter with border clamp (replicates edge pixel)
(define (make-getter vec w h)
  (lambda (x y)
    (vector-ref vec (+ (* (clamp y 0 (- h 1)) w)
                       (clamp x 0 (- w 1))))))

; weighted sum of the 3x3 neighborhood in a scalar channel
(define (apply-kernel get x y kernel extractor)
  (apply + (map (lambda (off k)
                  (* k (extractor (get (+ x (car off))
                                       (+ y (cadr off))))))
               offsets3x3 kernel)))

; general convolution: same kernel for R, G, B
(define (convolve px w h kr kg kb)
  (define vec (list->vector px))             ; O(N) once, then O(1) reads
  (define get (make-getter vec w h))
  (for*/list ([y (in-range h)] [x (in-range w)])
    (define p (vector-ref vec (+ (* y w) x)))
    (make-color (clamp (->int (apply-kernel get x y kr color-red  )) 0 255)
                (clamp (->int (apply-kernel get x y kg color-green)) 0 255)
                (clamp (->int (apply-kernel get x y kb color-blue )) 0 255)
                (color-alpha p))))


; --- gaussian blur 3×3 (normalized kernel, sum = 1) ---
(define kernel-gauss
  '(0.0625 0.125 0.0625
    0.125  0.25  0.125
    0.0625 0.125 0.0625))

(define (filter-gaussian px w h)
  (convolve px w h kernel-gauss kernel-gauss kernel-gauss))


;Border detection: Sobel operator on grayscale channel.
; Gx detects horizontal changes, Gy vertical ones.
; Magnitude = sqrt(Gx² + Gy²), normalized to [0,255].
(define kernel-sobel-x '(-1  0  1  -2  0  2  -1  0  1))
(define kernel-sobel-y '(-1 -2 -1   0  0  0   1  2  1))

(define (filter-edges px w h)
  (define gray-pixels (filter-grayscale px))   ; Sobel operates on grayscale
  (define vec (list->vector gray-pixels))
  (define get (make-getter vec w h))
  (for*/list ([y (in-range h)] [x (in-range w)])
    (define p   (vector-ref vec (+ (* y w) x)))
    (define gx  (apply-kernel get x y kernel-sobel-x color-red))
    (define gy  (apply-kernel get x y kernel-sobel-y color-red))
    (define mag (clamp (->int (sqrt (+ (* gx gx) (* gy gy)))) 0 255))
    (make-color mag mag mag (color-alpha p))))


 
; ----- benchmark: list-ref vs vector-ref ----------------


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


; -----  Filters meditions per image -------------------

(define (measure name thunk)
  (define-values [_ t] (timer thunk))
  (displayln (format "  ~a ~a ms" (~a name #:min-width 14) (round t))))

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


; Visual output

(pixels->image (filter-grayscale (image->pixels img-cat))
                 (image-width img-cat) (image-height img-cat))

;(pixels->image (filter-sepia (image->pixels img-cat))
;                 (image-width img-cat) (image-height img-cat))

;(pixels->image (filter-negative (image->pixels img-cat))
;                 (image-width img-cat) (image-height img-cat))

;(pixels->image (filter-gaussian (image->pixels img-cat) (image-width img-cat) (image-height img-cat))
;                 (image-width img-cat) (image-height img-cat))

;(pixels->image (filter-edges (image->pixels img-cat) (image-width img-cat) (image-height img-cat))
;                 (image-width img-cat) (image-height img-cat))

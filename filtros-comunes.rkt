#lang racket
(require 2htdp/image)

(provide
  ; utilities
  timer clamp ->int
  ; image bank
  img-cat img-cats2 img-ny
  ; image conversion
  image->pixels pixels->image
  ; per-pixel transformations
  pixel-grayscale pixel-sepia pixel-negative
  ; convolution infrastructure
  offsets3x3 make-getter apply-kernel convolve
  ; kernels
  kernel-gauss kernel-sobel-x kernel-sobel-y
  ; benchmark utility
  measure)


; ----- Utils ----------------------------------------

(define (timer f)
  (collect-garbage)
  (define t0 (current-inexact-milliseconds))
  (define r  (f))
  (define t1 (current-inexact-milliseconds))
  (values r (- t1 t0)))

(define (clamp v lo hi) (max lo (min hi v)))
(define (->int x)       (inexact->exact (round x)))


; ----- Image bank -----------------------------------

(define img-cat   (bitmap/file "images/cat.png"))
(define img-cats2 (bitmap/file "images/cats2.png"))
(define img-ny    (bitmap/file "images/new-york-large.jpg"))


; ----- Image conversion <-> RGBA list ---------------

(define (image->pixels img)    (image->color-list  img))
(define (pixels->image px w h) (color-list->bitmap px w h))


; ----- Per-pixel transformations (pure functions) ---

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

; negative: channel inversion
(define (pixel-negative p)
  (make-color (- 255 (color-red   p))
              (- 255 (color-green p))
              (- 255 (color-blue  p))
              (color-alpha p)))


; ----- Convolution infrastructure -------------------

; 3×3 kernel offsets (row-major: top-left → bottom-right)
(define offsets3x3
  '((-1 -1) (0 -1) (1 -1)
    (-1  0) (0  0) (1  0)
    (-1  1) (0  1) (1  1)))

; pixel getter with border clamp (replicates edge pixel)
(define (make-getter vec w h)
  (lambda (x y)
    (vector-ref vec (+ (* (clamp y 0 (- h 1)) w)
                       (clamp x 0 (- w 1))))))

; weighted sum of the 3×3 neighborhood for one scalar channel
(define (apply-kernel get x y kernel extractor)
  (apply + (map (lambda (off k)
                  (* k (extractor (get (+ x (car off))
                                       (+ y (cadr off))))))
               offsets3x3 kernel)))

; general convolution: same kernel applied to R, G, B
(define (convolve px w h kr kg kb)
  (define vec (vector->immutable-vector (list->vector px)))   ; O(N) once, then O(1) reads
  (define get (make-getter vec w h))
  (map (lambda (i)
         (define x (modulo   i w))
         (define y (quotient i w))
         (define p (vector-ref vec i))
         (make-color (clamp (->int (apply-kernel get x y kr color-red  )) 0 255)
                     (clamp (->int (apply-kernel get x y kg color-green)) 0 255)
                     (clamp (->int (apply-kernel get x y kb color-blue )) 0 255)
                     (color-alpha p)))
       (build-list (* w h) values)))


; ----- Kernels --------------------------------------

; gaussian blur 3×3 (normalized, sum = 1)
(define kernel-gauss
  '(0.0625 0.125 0.0625
    0.125  0.25  0.125
    0.0625 0.125 0.0625))

; Sobel edge detection
(define kernel-sobel-x '(-1  0  1  -2  0  2  -1  0  1))
(define kernel-sobel-y '(-1 -2 -1   0  0  0   1  2  1))


; ----- Timing utility --------------------------------

(define (measure name thunk)
  (define-values [_ t] (timer thunk))
  (displayln (format "  ~a ~a ms" (~a name #:min-width 14) (round t))))

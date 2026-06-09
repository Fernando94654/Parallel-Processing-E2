#lang racket
(require 2htdp/image)

(provide
  timer clamp ->int
  img-cat img-cats2 img-ny
  image->pixels pixels->image
  pixel-grayscale pixel-sepia pixel-negative
  measure)


(define (timer f)
  (collect-garbage)
  (define t0 (current-inexact-milliseconds))
  (define r  (f))
  (define t1 (current-inexact-milliseconds))
  (values r (- t1 t0)))

(define (clamp v lo hi) (max lo (min hi v)))
(define (->int x)       (inexact->exact (round x)))


(define img-cat   (bitmap/file "images/cat.png"))
(define img-cats2 (bitmap/file "images/cats2.png"))
(define img-ny    (bitmap/file "images/new-york-large.jpg"))


(define (image->pixels img)    (image->color-list  img))
(define (pixels->image px w h) (color-list->bitmap px w h))


; ITU-R BT.601 weighted luminance
(define (pixel-grayscale p)
  (define g (->int (+ (* 0.299 (color-red   p))
                      (* 0.587 (color-green p))
                      (* 0.114 (color-blue  p)))))
  (make-color g g g (color-alpha p)))

(define (pixel-sepia p)
  (define r (color-red p)) (define g (color-green p)) (define b (color-blue p))
  (make-color (clamp (->int (+ (* 0.393 r) (* 0.769 g) (* 0.189 b))) 0 255)
              (clamp (->int (+ (* 0.349 r) (* 0.686 g) (* 0.168 b))) 0 255)
              (clamp (->int (+ (* 0.272 r) (* 0.534 g) (* 0.131 b))) 0 255)
              (color-alpha p)))

(define (pixel-negative p)
  (make-color (- 255 (color-red   p))
              (- 255 (color-green p))
              (- 255 (color-blue  p))
              (color-alpha p)))


(define (measure name thunk)
  (define-values [_ t] (timer thunk))
  (displayln (format "  ~a ~a ms" (~a name #:min-width 12) (round t))))

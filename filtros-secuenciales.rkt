#lang racket
(require 2htdp/image)
(require racket/future)

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


; =====================================================
; FASE 2 — Filtros Paralelos con futures + touch
;
; Estrategia: particionamiento por franjas de filas
;
;   imagen completa
;   ┌────────────────┐
;   │  fila 0..k-1   │  → future 0
;   ├────────────────┤
;   │  fila k..2k-1  │  → future 1
;   ├────────────────┤
;   │      ...       │
;   └────────────────┘
;
; Filtros map (grayscale, sepia, negative):
;   La lista de píxeles se divide en N trozos iguales.
;   Cada future aplica (map pixel-fn chunk).
;   Resultado: (apply append (map touch futs)).
;
; Filtros de convolución (gaussian, edges):
;   El vector de entrada es compartido (solo lectura,
;   thread-safe en Racket). Cada future genera únicamente
;   las filas que le corresponden; no se necesitan locks.
;   Resultado: se concatenan las sublistas de cada future.
;
; En ambos casos la unificación es un simple append;
; no se requiere reducción adicional porque las franjas
; son disjuntas y están ordenadas.
; =====================================================


; --- Particionamiento: lista → N trozos de tamaño ≈ igual ---

(define (split-into-n lst n)
  (define len (length lst))
  (define sz  (max 1 (inexact->exact (ceiling (/ len n)))))
  (let loop ([rem lst] [acc '()] [rem-len len])
    (if (null? rem)
        (reverse acc)
        (let ([take-n (min sz rem-len)])
          (define-values [chunk rest] (split-at rem take-n))
          (loop rest (cons chunk acc) (- rem-len take-n))))))


; --- Filtros map en paralelo -------------------------
; Divide la lista en N chunks → un future por chunk →
; toca todos con touch → concatena con append.

(define (par-map-filter pixel-fn px n)
  (define futs (map (lambda (chunk)
                      (future (lambda () (map pixel-fn chunk))))
                    (split-into-n px n)))
  (apply append (map touch futs)))


; --- Convolución en paralelo -------------------------
; El vector completo se comparte entre futures (read-only).
; Cada future procesa su rango de filas [y0, y1) de forma
; independiente — no hay escrituras compartidas.

(define (par-convolve px w h kr kg kb n)
  (define vec     (list->vector px))
  (define get     (make-getter vec w h))
  (define rows/t  (max 1 (inexact->exact (ceiling (/ h n)))))
  (define n-actual (min n h))
  (define futs
    (for/list ([t (in-range n-actual)])
      (define y0 (* t rows/t))
      (define y1 (min h (+ y0 rows/t)))
      (future (lambda ()
        (for*/list ([y (in-range y0 y1)] [x (in-range w)])
          (define p (vector-ref vec (+ (* y w) x)))
          (make-color (clamp (->int (apply-kernel get x y kr color-red))   0 255)
                      (clamp (->int (apply-kernel get x y kg color-green)) 0 255)
                      (clamp (->int (apply-kernel get x y kb color-blue))  0 255)
                      (color-alpha p)))))))
  (apply append (map touch futs)))


; --- Wrappers paralelos (misma firma que fase 1 + n) -

(define (par-filter-grayscale px n) (par-map-filter pixel-grayscale px n))
(define (par-filter-sepia     px n) (par-map-filter pixel-sepia     px n))
(define (par-filter-negative  px n) (par-map-filter pixel-negative  px n))

(define (par-filter-gaussian px w h n)
  (par-convolve px w h kernel-gauss kernel-gauss kernel-gauss n))

(define (par-filter-edges px w h n)
  (define gray-px  (par-map-filter pixel-grayscale px n))   ; paso 1: escala de grises paralela
  (define vec      (list->vector gray-px))
  (define get      (make-getter vec w h))
  (define rows/t   (max 1 (inexact->exact (ceiling (/ h n)))))
  (define n-actual  (min n h))
  (define futs
    (for/list ([t (in-range n-actual)])
      (define y0 (* t rows/t))
      (define y1 (min h (+ y0 rows/t)))
      (future (lambda ()
        (for*/list ([y (in-range y0 y1)] [x (in-range w)])
          (define p   (vector-ref vec (+ (* y w) x)))
          (define gx  (apply-kernel get x y kernel-sobel-x color-red))
          (define gy  (apply-kernel get x y kernel-sobel-y color-red))
          (define mag (clamp (->int (sqrt (+ (* gx gx) (* gy gy)))) 0 255))
          (make-color mag mag mag (color-alpha p)))))))
  (apply append (map touch futs)))


; --- Benchmark paralelo: 1 2 4 8 16 hilos -----------

(define thread-counts '(1 2 4 8 16))

(define (report-image-parallel label img)
  (define w  (image-width  img))
  (define h  (image-height img))
  (define px (image->pixels img))
  (displayln (format "\n[~a]  ~a x ~a = ~a px" label w h (* w h)))
  (for-each (lambda (n)
    (displayln (format "\n  -- ~a hilo(s) --" n))
    (measure "grayscale"   (lambda () (par-filter-grayscale px n)))
    (measure "sepia"       (lambda () (par-filter-sepia     px n)))
    (measure "negative"    (lambda () (par-filter-negative  px n)))
    (measure "gaussian"    (lambda () (par-filter-gaussian  px w h n)))
    (measure "edge detect" (lambda () (par-filter-edges     px w h n))))
  thread-counts))

(displayln "\n=== Tiempos paralelos (ms) ===")
(report-image-parallel "cat.png    (small)  " img-cat)
;(report-image-parallel "cats2.png  (medium) " img-cats2)
;(report-image-parallel "new-york   (large)  " img-ny)

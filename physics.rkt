#lang racket/gui

(require racket/math)

(define WINDOW-WIDTH 640)
(define WINDOW-HEIGHT 480)
(define SCALE 3)

(define COLOR-BACKGROUND (make-object color% 39 40 34))
(define COLOR-ORANGE (make-object color% 253 135 36))
(define BORDER-WIDTH 0.5)

(define frame (new frame%
                   [label "physics"]
                   [width WINDOW-WIDTH]
                   [height WINDOW-HEIGHT]))

(define EDGES 3)
(define RADIUS 20)

(define (add-edge) (set! EDGES (+ EDGES 1)))
(define (remove-edge) (set! EDGES (max (- EDGES 1) 3)))

(define (make-points edges radius)
  (build-list edges (lambda (i)
                      (make-object point%
                                   (+ (* radius (cos (* 2 pi (/ i edges)))) (/ WINDOW-WIDTH (* SCALE 2)))
                                   (+ (* radius (sin (* 2 pi (/ i edges)))) (/ WINDOW-HEIGHT (* SCALE 2)))))))

(define my-canvas%
  (class canvas%
    (define/override (on-char event)
      (define key (send event get-key-code))
      (cond
        ([equal? key 'left] '())
        ([equal? key 'right] '())
        ([equal? key 'up] (add-edge))
        ([equal? key 'down] (remove-edge)))
      (send this refresh))
    (super-new)))

(define (draw-stuff dc)
  (send dc set-brush COLOR-ORANGE 'solid)
  (send dc set-pen COLOR-ORANGE BORDER-WIDTH 'hilite)
  (send dc set-text-foreground "white")
  (let ([points (make-points EDGES RADIUS)])
    (send dc draw-polygon points)))


(new my-canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (send canvas set-canvas-background COLOR-BACKGROUND)
        (send dc set-scale SCALE SCALE)
        (draw-stuff dc))])

(send frame show #t)

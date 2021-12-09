(define-module (days day09)
  #:use-module (advent-utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-158)
  #:use-module (srfi srfi-171))

(define-public (solve1 grid)
  (generator-transduce
   (compose
    (tfilter-low-points grid)
    (tmap (lambda (coords) (apply point-score grid coords))))
   +
   (make-2d-array-index-generator grid)))

(define-public (solve2 grid)
  (apply *
   (list-head
    (sort
     (generator-transduce
      (compose
       (tfilter-low-points grid)
       (tmap (lambda (coords) (apply get-bassin-size grid coords))))
      rcons
      (make-2d-array-index-generator grid))
     >)
    3)))

(define (tfilter-low-points grid)
  (tfilter (lambda (coords) (apply is-low-point? grid coords))))

(define (make-2d-array-index-generator array)
  "Yield (x y) coordinates"
  (let ((height (car (array-dimensions array)))
        (width (cadr (array-dimensions array))))
    (make-coroutine-generator
     (lambda (yield)
       (do ((y 0 (1+ y))) ((= y height))
         (do ((x 0 (1+ x))) ((= x width))
           (yield (list x y))))))))

(define (point-score grid x y)
  (1+ (array-ref grid y x)))

(define (is-low-point? grid x y)
  (let ((adjacents (adjacent-points grid x y))
        (this-height (array-ref grid y x)))
    (every
     (lambda (height) (> height this-height))
     adjacents)))

(define (adjacent-points grid x y)
  (filter-map
   (lambda (offset)
     (let*-values (((x-offset y-offset) (car+cdr offset))
                   ((new-x) (+ x x-offset))
                   ((new-y) (+ y y-offset)))
       (and
        (array-in-bounds? grid new-y new-x)
        (array-ref grid new-y new-x))))
   %cardinals))

(define (get-bassin-size grid x y)
  (define (is-top? grid x y)
    (= 9 (array-ref grid y x)))

  (length (visit-grid grid x y is-top? '())))

(define (visit-grid grid x y stop-proc visited)
  ;(format #t "visit ~a,~a ~a\n" x y visited)
  (if (or
       (not (array-in-bounds? grid y x))
       (stop-proc grid x y)
       (member `(,x . ,y) visited))

      visited

      (fold
       (lambda (offset visited)
         ;(format #t "Will visit ~d,~d\n" (+ x (car offset)) (+ y (cdr offset)))
         (visit-grid grid (+ x (car offset)) (+ y (cdr offset)) stop-proc visited))
       (cons `(,x . ,y) visited)
       %cardinals)))

(define %cardinals
  ;; (x . y) offsets
  '((0 . -1)
    (1 . 0)
    (0 . 1)
    (-1 . 0)))

(define-public (parse port)
  (list->typed-array 'u8 2 (parse-grid port)))

(define (parse-grid port)
  (map
   (lambda (line)
     (map
      (lambda (c)
        (- (char->integer c) (char->integer #\0)))
      (string->list line)))
   (parse-lines-file port)))

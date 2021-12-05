(define-module (days day05)
  #:use-module (advent-utils)
  #:use-module (ggspec lib)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-41))

(define-public (parse-and-solve1 input)
  (solve1 (parse input)))

(define-public (parse-and-solve2 input)
  (solve2 (parse input)))

(define-public (solve1 lines)
  (count-overlaps (filter is-not-diagonal? lines)))

(define-public (solve2 lines)
  (count-overlaps lines))

(define (count-overlaps lines)
  (let* ((bounds (lines-bounds lines))
         (grid (make-typed-array 'u8 0 `(,(line-start-y bounds) ,(line-end-y bounds)) `(,(line-start-x bounds) ,(line-end-x bounds)))))

    (fold
     (lambda (line n-intersections)
       (fold-line-points
        (lambda (x y n-intersections)
          (+ n-intersections
             (match (array-ref grid y x)
               (0 (array-set! grid 1 y x) 0)
               (1 (array-set! grid 2 y x) 1)
               (_ 0))))
        n-intersections
        line))
     0 lines)))

(define (fold-line-points proc acc line)
  "call (proc x y) over the points of the line"

  (let* ((start-x (line-start-x line))
         (start-y (line-start-y line))
         (end-x (line-end-x line))
         (end-y (line-end-y line))
         (min-x (min start-x end-x))
         (max-x (max start-x end-x))
         (min-y (min start-y end-y))
         (max-y (max start-y end-y))
         (length-x (1+ (- max-x min-x)))
         (length-y (1+ (- max-y min-y))))

    (cond
     ((eq? start-x end-x) (fold
                           (lambda (y acc)
                             (proc start-x y acc))
                           acc
                           (iota length-y min-y)))
     ((eq? start-y end-y) (fold
                           (lambda (x acc)
                             (proc x start-y acc))
                           acc
                           (iota length-x min-x)))
     ((eq? length-x length-y)
      (fold
       (lambda (coords acc)
         (proc (car coords) (cadr coords) acc))
       acc
       (zip
        (iota length-x start-x
              (if (< start-x end-x) 1 -1))
        (iota length-y start-y
              (if (< start-y end-y) 1 -1)))))

     (#t (error "Invalid line ~a" line)))))

(define (lines-bounds lines)
  "Return the bouds of the lines in the form ((min-x . min-y) . (max-x . max-y))"
  (fold
   (lambda (line bounds)
     `((,(min (line-start-x bounds) (line-start-x line) (line-end-x line))
        . ,(min (line-start-y bounds) (line-start-y line) (line-end-y line)))
       .
       (,(max (line-end-x bounds) (line-start-x line) (line-end-x line))
        . ,(max (line-end-y bounds) (line-start-y line) (line-end-y line)))))

   `((,(line-start-x (car lines)) . ,(line-start-y (car lines))) . (,(line-start-x (car lines)) . ,(line-start-y (car lines))))
   lines))

(define (is-not-diagonal? line)
  (or (eq? (line-start-x line) (line-end-x line))
      (eq? (line-start-y line) (line-end-y line))))

(define is-diagonal? (compose not is-not-diagonal?))

(define line-start car)
(define line-end cdr)
(define line-start-x caar)
(define line-start-y cdar)
(define line-end-x cadr)
(define line-end-y cddr)

(define-public (parse port)
  (stream->list (stream-map parse-line (parse-lines-file-stream port))))

(define (parse-line line)
  (let ((coords (map
                 (lambda (point-str)

                   (let ((x+y (map string->number (string-split point-str #\,))))
                     `(,(car x+y) . ,(cadr x+y))))
                 (string-tokenize line (->char-set "0123456789,")))))

    `(,(car coords) . ,(cadr coords))))

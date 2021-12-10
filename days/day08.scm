(define-module (days day08)
  #:use-module (advent-utils)
  #:use-module (ggspec lib)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-171))

(define-public (solve1 tests)
  (list-transduce
   (compose (tmap cdr) tconcatenate (tfilter is-simple-digit?))
   rcount
   tests))

(define-public (solve2 tests)
  (list-transduce
   (tmap (lambda (input+output) (solve-and-yield-output (car input+output) (cdr input+output))))
   +
   tests))

(define (solve-and-yield-output input output)
  (compute-output output (solve input)))

(define (compute-output output solution)
  (list-transduce
   (compose
    (tmap (lambda (display) (correct-wiring display solution)))
    (tmap display->integer))
   (lambda args
     (match args
       ((acc n) (+ n (* 10 acc)))
       ((acc) acc)))
   0
   output))

(define (correct-wiring display solution)
  (sort
   (map
    (lambda (incorrect-segment)
      (list-index (lambda (i) (eq? i incorrect-segment)) solution))
    display)
   <))

(define (display->integer display)
  (list-index
   (lambda (num-display)
     (equal? display num-display))
   %nums-to-display))

(define (solve displays)
  (let* ((displays (sort displays (lambda (a b) (< (length a) (length b)))))
         (displays-grouped
          (list-transduce
           (tpartition
            (let ((last-return #f)
                  (last-length 0))
              (lambda (display)
                (if (eq? (length display) last-length)
                    last-return
                    (begin
                      (set! last-length (length display))
                      (set! last-return (not last-return))
                      last-return)))))
           rcons
           displays)))

    (apply
     append
     (fold
      eliminate-possibilities
      (create-possibilities)
      displays-grouped))))

(define (eliminate-possibilities displays possibilities)
  "Display: a list of segment as (integer)
   Possibilities: a list of sets of possible output segments for each input segment"
  (let* ((display-length (length (car displays)))
         (displays-intersection (apply lset-intersection eq? displays))
         (possible-correct-segments (list-ref %digits-possible-per-length display-length))
         (possible-correct-segments-set (apply lset-intersection eq? possible-correct-segments)))

    (map
     (lambda (segment segment-possibilities)
       (if (memq segment possible-correct-segments-set)
           (lset-intersection eq? segment-possibilities displays-intersection)
           (lset-difference eq? segment-possibilities displays-intersection)))
     (iota 7)
     possibilities)))

(define (cmp-easy-display a b)
  "Return true if a is more easy than b, that is: length of a has less possible digits than length of b"
  (let ((a-possibilities (n-digit-possible a))
        (b-possibilities (n-digit-possible b)))
    (or (< a-possibilities b-possibilities)
        (and (eq? a-possibilities b-possibilities) (< (length a) (length b))))))

(define (create-possibilities)
  (map
   (lambda i (iota 7))
   (iota 7)))

;; list of sets of segments, ordered per digit
(define %nums-to-display
  '((0 1 2 4 5 6)
    (2 5)
    (0 2 3 4 6)
    (0 2 3 5 6)
    (1 2 3 5)
    (0 1 3 5 6)
    (0 1 3 4 5 6)
    (0 2 5)
    (0 1 2 3 4 5 6)
    (0 1 2 3 5 6)))

(define %digits-possible-per-length
  (map
   (lambda (len)
     (list-transduce
      (tfilter (lambda (d)
                 (eq? len (length d))))
      rcons
      %nums-to-display))
   (iota 8)))

(define %n-digits-possible-per-length
  (map
   (lambda (len)
     (list-transduce
      (tfilter (lambda (d)
                 (eq? len (length d))))
      rcount
      %nums-to-display))
   (iota 8)))

(define (n-digit-possible display)
  "Return the number of possible digits for display based on its lenght"
  (list-ref %n-digits-possible-per-length (length display)))

(define (is-simple-digit? display)
  (memq (length display) '(2 3 4 7)))

(define-public (parse port)
  (map parse-line (parse-lines-file port)))

(define (parse-line line)
  (let* ((splitted (string-split line #\|))
         (input (car splitted))
         (output (cadr splitted)))

    `(,(map parse-display (string-tokenize input))
      .
      ,(map parse-display (string-tokenize output)))))

(define (parse-display display)
  (map segment->integer (string->list display)))

(define (segment->integer segment)
  (- (char->integer segment) (char->integer #\a)))

(define (integer->segment i)
  (integer->char (+ i (char->integer #\a))))

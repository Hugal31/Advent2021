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
  (let* ((displays (sort (append input output) cmp-easy-display))
         (possibilities (create-possibilities)))

    ((@ (ice-9 pretty-print) pretty-print)
     (fold
      eliminate-possibilities
      possibilities
      displays))
    0))

(define (eliminate-possibilities display possibilities)
  "Display: a list of segment as (integer)
   Possibilities: a list of sets of possible output segments for each input segment"
  (let ((possible-correct-segments (list-ref %digits-possible-per-length (length display))))
    ;; TODO Find a way to remove this if
    (if (eq? 1 (length possible-correct-segments))
        (let ((correct-segments (car possible-correct-segments)))
                                        ;(format #t "Testing ~a\nPossibilities are ~a\n" display possibilities)
          (map
           (lambda (segment segment-possibilities)
             (if (memq segment correct-segments)
                 (lset-intersection eq? segment-possibilities display)
                 (lset-difference eq? segment-possibilities display)))
           (iota 7)
           possibilities))
        possibilities)))

;; (define (remove-possibilities-if-found possibilities)
;;   (define (get-found-numbers pos)
;;     (list-transduce
;;      (compose (tfilter (lambda (p) (= 1 (length p)))) tflatten)
;;      rcons
;;      pos))

;;   (do ((prev-possibilities '() possibilities)
;;        (found-numbers (get-found-numbers possibilities)
;;                       (get-found-numbers possibilities)))
;;       ((eq? prev-possibilities possibilities) possibilities)

;;     (set! possibilities
;;           (map
;;            (lambda (segment-possibilities)
;;              (if (= 1 (length segment-possibilities))
;;                  segment-possibilities
;;                  (lset-difference eq? segment-possibilities found-numbers)))
;;            possibilities))))

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

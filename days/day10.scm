(define-module (days day10)
  #:use-module (advent-utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-171))

(define-public (solve1 lines)
  (list-transduce
   (tmap corrupt-line-score)
   +
   lines))

(define-public (solve2 lines)
  (take-middle
   (sort
    (list-transduce
     (compose
      (tfilter is-line-valid?)
      (tmap auto-complete-line-score))
     rcons
     lines)
    <)))

(define (take-middle list)
  (list-ref list (/ (1- (length list)) 2)))

(define (auto-complete-line-score line)
  (fold
   (lambda (opening-char acc)
     (+ (* 5 acc)
        (assq-ref %autocomplete-char-score opening-char)))
   0
   (parse-line line)))

(define (parse-line line)
  "Return the list of the opening chars waiting for a closing char, in the reverse order,
OR the offending corrupted char if any"
  (fold
     (lambda (next-char stack)
       (if (list? stack)
           (if (is-opening-char? next-char)
               (cons next-char stack)
               (if (and (not (null? stack))
                        (eq? next-char (assq-ref %associated-closing-char (car stack))))
                   (cdr stack)
                   next-char))
           stack))
     '()
     (string->list line)))

(define (corrupt-line-score line)
  (or
   (assq-ref %corrupt-char-score (parse-line line))
   0))

(define is-line-valid? (compose list? parse-line))

(define (is-opening-char? c)
  (memq c %opening-chars))

(define %opening-chars
  '(#\( #\[ #\{ #\<))

(define %associated-closing-char
  '((#\( . #\))
    (#\[ . #\])
    (#\{ . #\})
    (#\< . #\>)))

(define %corrupt-char-score
  '((#\). 3)
    (#\] . 57)
    (#\} . 1197)
    (#\> . 25137)))

(define %autocomplete-char-score
  '((#\( . 1)
    (#\[  . 2)
    (#\{ . 3)
    (#\< . 4)))

(define-public parse parse-lines-file)

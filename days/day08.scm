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

(define-public (solve2 numbers)
  0)

(define (is-simple-digit? display)
  (match (length display)
    ((or 2 3 4 7) #t)
    (_ #f)))

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
  (map (lambda (c) (- (char->integer c) (char->integer #\a))) (string->list display)))

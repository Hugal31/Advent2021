(define-module (days day01)
  #:use-module (advent-utils)
  #:use-module (ggspec lib)
  #:use-module (srfi srfi-1))

(define-public (parse-and-solve1 input)
  (solve1 (parse-ints-file input)))

(define-public (parse-and-solve2 input)
  (solve2 (parse-ints-file input)))

(define-public (solve1 numbers)
  (count >
         (cdr numbers) numbers))

(define-public (solve2 numbers)
  (count >
         (cdddr numbers) numbers))

(define-public parse parse-ints-file)

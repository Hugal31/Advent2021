(define-module (spec utils)
  #:use-module (ggspec lib)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match))

(define-public (simple-example-suites
                day-number
                cases-part1
                cases-part2)

  "Define two suites, on per part, with N tests.

  Arguments:
    day-number: integer: the number of the day

   cases-part1: list of lists. Each item is a test case for part 1,
   a tuple with the input in the car and the output in the cdr.

   cases-part2: same as cases-part1, but for part2."

  (let* ((module (resolve-module `(days ,(string->symbol (format #f "day~2,'0d" day-number)))))
         (solve1 (module-ref module 'solve1))
         (solve2 (module-ref module 'solve2)))

    (unless (and solve1 solve2)
      (error "Could not resolve solve1 or solve2 in module" (list module solve1 solve2)))

    (simple-example-suite day-number 1 solve1 cases-part1)
    (simple-example-suite day-number 2 solve2 cases-part2)))

(define (simple-example-suite day-number part-number solve-proc test-cases)
  (suite (format #f "Day ~d part ~d" day-number part-number)
         (apply tests
                (map (lambda (test-case)
                       (let ((input (car test-case))
                             (output (cdr test-case)))
                         (test
                          (test-case-description part-number input output)
                          e
                          (assert-equal output (solve-proc input)))))
                     test-cases))))

(define (test-case-description part-number input output)
  (with-output-to-string
    (lambda ()
      (format #t "solve~d should return ~a on input\n" part-number output)
      ((@ (ice-9 pretty-print) pretty-print) input))))

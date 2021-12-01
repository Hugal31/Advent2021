(define-module (days day01)
  #:use-module (advent-utils)
  #:use-module (ggspec lib)
  #:use-module (srfi srfi-1))

(define-public (parse-and-solve1 input)
  (solve1 (parse-ints-file input)))

(define-public (parse-and-solve2 input)
  (solve2 (parse-ints-file input)))

(define-public (solve1 numbers)
  (fold (lambda (n-1 n acc)
          (if (> n n-1) (+ 1 acc) acc))
        0 numbers (cdr numbers)))

(define-public (solve2 numbers)
  (fold (lambda (n-3 n acc)
          (if (> n n-3)
              (+ 1 acc)
              acc))
        0 numbers (cdddr numbers)))

(define-public (unit-tests)
  (let ((example-list
          '(199
            200
            208
            210
            200
            207
            240
            269
            260
            263)))

    (suite "solve1"
           (tests
            (test "solve1 should return 7 on example"
                  e
                  (assert-equal 7 (solve1 example-list)))))

    (suite "solve2"
           (tests
            (test "solve2 should return 5 on example"
                  e
                  (assert-equal 5 (solve2 example-list)))))))

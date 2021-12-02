(define-module (days day02)
  #:use-module (advent-utils)
  #:use-module (ggspec lib)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-41))

(define-public (parse-and-solve1 input)
  (solve1 (parse-instructions input)))

(define-public (parse-and-solve2 input)
  (solve2 (parse-instructions input)))

(define-public (solve1 instructions)
  (let ((horizontal 0)
        (depth 0))

    (for-each (lambda (instruction)
                (match instruction
                  (('forward . amount)
                   (set! horizontal (+ horizontal amount)))
                  (('down . amount)
                   (set! depth (+ depth amount)))
                  (('up . amount)
                   (set! depth (- depth amount)))))
              instructions)

    (* horizontal depth)))

(define-public (solve2 instructions)
  (let ((aim 0)
        (horizontal 0)
        (depth 0))

    (for-each (lambda (instruction)
                (match instruction
                  (('forward . amount)
                   (set! horizontal (+ horizontal amount))
                   (set! depth (+ depth (* amount aim))))
                  (('down . amount)
                   (set! aim (+ aim amount)))
                  (('up . amount)
                   (set! aim (- aim amount)))))
              instructions)

    (* horizontal depth)))

(define (parse-instructions input)
  (stream->list
   (stream-map (lambda (line)
                 (match (string-split line #\space)
                   (("forward" amount)
                    `(forward . ,(string->number amount)))
                   (("up" amount)
                    `(up . ,(string->number amount)))
                   (("down" amount)
                    `(down . ,(string->number amount)))))
               (parse-lines-file-stream input))))

(define-public (unit-tests)
  (let ((example-list
         '((forward . 5)
           (down . 5)
           (forward . 8)
           (up . 3)
           (down . 8)
           (forward . 2))))

    (suite "solve1"
           (tests
            (test "solve1 should return 150 on example"
                  e
                  (assert-equal 150 (solve1 example-list)))))

    (suite "solve2"
           (tests
            (test "solve2 should return 900 on example"
                  e
                  (assert-equal 900 (solve2 example-list)))))))

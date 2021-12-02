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
                   ((not (_ (? (lambda (txt) (integer? (string->number txt 10))))))
                    (error "Each line must be a string and a number separated by a line" line))

                   (("forward" amount)
                    `(forward . ,(string->number amount 10)))
                   (("up" amount)
                    `(up . ,(string->number amount 10)))
                   (("down" amount)
                    `(down . ,(string->number amount 10)))))
               (parse-lines-file-stream input))))

(define-public (unit-tests)
  (let ((example-list
         '((forward . 5)
           (down . 5)
           (forward . 8)
           (up . 3)
           (down . 8)
           (forward . 2))))

    (suite "parse-instructions"
           (tests
            (test "should parse null list"
                  e
                  (assert-equal '() (call-with-input-string "" parse-instructions)))
            (test "should parse all instructions"
                  e
                  (assert-equal '((forward . 1)
                                  (up . 2)
                                  (down . 3))
                                (call-with-input-string "forward 1
up 2
down 3"
                                  parse-instructions)))
            (test "should throw error on invalid tet"
                  e
                  (assert-all
                   (assert-true (error? (call-with-input-string "up b" parse-instructions)))
                   (assert-true (error? (call-with-input-string "forward c" parse-instructions)))
                   (assert-true (error? (call-with-input-string "down 1.2" parse-instructions)))))))

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

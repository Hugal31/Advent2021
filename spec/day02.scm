(add-to-load-path (dirname (dirname (current-filename))))

(use-modules ((days day02) #:prefix day02::)
             (ggspec lib))

(define example-list
  '((forward . 5)
    (down . 5)
    (forward . 8)
    (up . 3)
    (down . 8)
    (forward . 2)))

(suite "Day02 parse-instructions"
       (tests
        (test "should parse null list"
              e
              (assert-equal '() (call-with-input-string "" day02::parse-instructions)))
        (test "should parse all instructions"
              e
              (assert-equal '((forward . 1)
                              (up . 2)
                              (down . 3))
                            (call-with-input-string "forward 1
up 2
down 3"
                              day02::parse-instructions)))
        (test "should throw error on invalid tet"
              e
              (assert-all
               (assert-true (error? (call-with-input-string "up b" day02::parse-instructions)))
               (assert-true (error? (call-with-input-string "forward c" day02::parse-instructions)))
               (assert-true (error? (call-with-input-string "down 1.2" day02::parse-instructions)))))))

(suite "Day02 part1"
       (tests
        (test "solve1 should return 150 on example"
              e
              (assert-equal 150 (day02::solve1 example-list)))))

(suite "Day02 part2"
       (tests
        (test "solve2 should return 900 on example"
              e
              (assert-equal 900 (day02::solve2 example-list)))))

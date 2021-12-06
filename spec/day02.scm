(add-to-load-path (dirname (dirname (current-filename))))

(use-modules ((days day02) #:prefix day02::)
             (ggspec lib)
             (spec utils))

(define example-list
  '((forward . 5)
    (down . 5)
    (forward . 8)
    (up . 3)
    (down . 8)
    (forward . 2)))

(simple-example-suites
 2
 `((,example-list . 150))
 `((,example-list . 900)))

(suite "Day 2 parse-instructions"
       (tests
        (test "should parse null list"
              e
              (assert-equal '() (call-with-input-string "" day02::parse)))
        (test "should parse all instructions"
              e
              (assert-equal '((forward . 1)
                              (up . 2)
                              (down . 3))
                            (call-with-input-string "forward 1
up 2
down 3"
                              day02::parse)))
        (test "should throw error on invalid tet"
              e
              (assert-all
               (assert-true (error? (call-with-input-string "up b" day02::parse)))
               (assert-true (error? (call-with-input-string "forward c" day02::parse)))
               (assert-true (error? (call-with-input-string "down 1.2" day02::parse)))))))

(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (advent-utils)
             (ggspec lib))

(suite "Test advent-utils parsing"
       (tests
        (test "parse-lines-file should return a list of lines"
              e
              (assert-equal
               '("1" "" "2 3" "foobar")
               (call-with-input-string "1

2 3
foobar"
                 parse-lines-file)))

        (test "parse-sections should return a list of list of string"
              e
              (assert-equal
               '(("1 2 3") ("a b c" "d e f"))
               (call-with-input-string "1 2 3

a b c
d e f

"
                 parse-sections)))

        (test "string-split-non-empty should return a list of non-empty strings"
              e
              (assert-equal
               '("1" "2" "3")
               (string-split-non-empty "  1 2   3" #\space)))))

#!
(suite "Test advent-utils misc"
       (tests
        (test "combination should return list combinations"
              e
              (assert-all
               (assert-equal '() (combinations '() 2))
               (assert-equal '() (combinations '(1) 2))
               (assert-equal '((1 2)) (combinations '(1 2) 2))
               (assert-equal '((1 2) (1 3) (2 3)) (combinations '(1 2 3) 2))))))
!#

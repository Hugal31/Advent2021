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

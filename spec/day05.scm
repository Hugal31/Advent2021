(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (ggspec lib)
             ((days day05) #:prefix day05::)
             (spec utils))

(define example
  "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(simple-example-suites
 5
 `((,example . 5))
 `((,example . 12))
 #:parse? #t)

(suite "Test day 5 utils"
       (tests
        (test "parse should return a list of pairs of pair"
              e
              (assert-all
               (assert-equal
                '(((0 . 9) . (5 . 9))
                  ((4 . 2) . (1 . 2)))
                (call-with-input-string "0,9 -> 5,9
4,2 -> 1,2"
                 day05::parse))))))

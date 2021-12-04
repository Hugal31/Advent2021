(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (ggspec lib)
             (spec utils))

(define example
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(simple-example-suites
 4
 `((,example . 4512))
 `((,example . 1924))
 #:parse #t)

(define parse (@@ (days day04) parse))
(define apply-bingo (@@ (days day04) apply-bingo))
(define grid-score (@@ (days day04) grid-score))
(define winning-grid (@@ (days day04) winning-grid))

;; TODO Use values?
(suite "Day 4 utils"
       (tests
        (test "parse should return a list of numbers and a list of array"
              e
              (assert-equal
               '((1 4 5 2 3)
                 (#2((4 2 3)
                     (1 2 3)
                     (5 6 2))
                    #2((1 2 3)
                       (4 5 6)
                       (7 8 9))))
               (call-with-input-string "1,4,5,2,3

4 2 3
1 2 3
5 6 2

1 2 3
4 5  6
7 8  9"
                 parse)
               ))

        (test "apply-bingo should return a modified validation array"
              e
              (assert-equal
               #2b((#f #t) (#f #t))
               (apply-bingo
                #2((1 2) (3 4)) #2b((#f #t) (#f #f)) 4)))

        (test "winning-grid should return #t if the grid contains a row or a column of #t"
              e
              (assert-all
               (assert-false (winning-grid #2b((#f #t) (#t #f))))
               (assert-true (winning-grid #2b((#t #f) (#t #f))))
               (assert-true (winning-grid #2b((#f #t) (#f #t))))
               (assert-true (winning-grid #2b((#t #t) (#f #f))))))

        (test "grid-score should return the sum of all unchecked numbers"
              e
              (assert-equal
               (+ 2 3)
               (grid-score #2((1 2) (3 4)) #2b((#f #t) (#t #f)))))))

(add-to-load-path (dirname (dirname (current-filename))))

(use-modules ((days day01) #:prefix day01::)
             (ggspec lib))

(define example-list
  '(199
    200
    208
    210
    200
    207
    240
    269
    260
    263))

(suite "Day01 solve1"
       (tests
        (test "solve1 should return 7 on example"
              e
              (assert-equal 7 (day01::solve1 example-list)))))

(suite "Day01 solve2"
       (tests
        (test "solve2 should return 5 on example"
              e
              (assert-equal 5 (day01::solve2 example-list)))))

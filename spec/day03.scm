(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (ggspec lib)
             (spec utils))

(define example-list
  '(#b00100
    #b11110
    #b10110
    #b10111
    #b10101
    #b01111
    #b00111
    #b11100
    #b10000
    #b11001
    #b00010
    #b01010))

(simple-example-suites
 3
 `((,example-list . 198))
 `((,example-list . 230)))

(define most-common-bit (@@ (days day03) most-common-bit))
(define max-numbers-bit (@@ (days day03) max-numbers-bit))

(suite "Day 3 utils"
       (tests
        (test "max-numbers-bit should return the maximum bit index"
              e
              (assert-all
               (assert-equal 0 (max-numbers-bit '()))
               (assert-equal 1 (max-numbers-bit '(1 2)))
               (assert-equal 2 (max-numbers-bit '(4 1 0)))))

        (test "most-common-bit should return the most commit bits for a given position"
              e
              (assert-all
               (assert-false (most-common-bit 0 '(#b10 #b01 #b10)))
               (assert-true (most-common-bit 1 '(#b10 #b01 #b10)))
               (assert-equal 0 (most-common-bit 1 '(#b10 #b01)))))))

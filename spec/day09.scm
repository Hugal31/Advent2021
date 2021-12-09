(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (ggspec lib)
             (spec utils))

(define example
  "2199943210
3987894921
9856789892
8767896789
9899965678")

(simple-example-suites
 9
 `((,example . 15))
 `((,example . 1134))
 #:parse? #t)

(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (ggspec lib)
             (spec utils))

(define example
  '(3 4 3 1 2))

(simple-example-suites
 6
 `((,example . 5934))
 `((,example . 26984457539)))

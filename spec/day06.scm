(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (ggspec lib)
             ((days day05) #:prefix day05::)
             (spec utils))

(define example
  '(3 4 3 1 2))

(simple-example-suites
 6
 `((,example . 5934))
 `())

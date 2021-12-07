(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (ggspec lib)
             (spec utils))

(define example-list
  '(16 1 2 0 4 2 7 1 2 14))

(simple-example-suites
 7
 `((,example-list . 37))
 `((,example-list . 168)))

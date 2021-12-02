(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (ggspec lib)
             (spec utils))

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

(simple-example-suites
 1
 `((,example-list . 7))
 `((,example-list . 5)))

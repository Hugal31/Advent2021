(define-module (days day04)
  #:use-module (advent-utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-41))

(define-public (parse-and-solve1 input)
  (solve1 (parse input)))

(define-public (parse-and-solve2 input)
  (solve2 (parse input)))

(define-public (solve1 numbers)
  (let* ((pool (car numbers))
         (grids (cadr numbers))
         (grid-validations (map make-validation-array grids))
         (last-drawn-number #f))

    (*
     (do ((drawn-number (car pool) (car pool))
          (pool (cdr pool) (cdr pool)))
         ((any winning-grid grid-validations) last-drawn-number)
       (set! grid-validations
               (map
                (lambda (grid validation)
                  (apply-bingo grid validation drawn-number))
                grids grid-validations))
       (set! last-drawn-number drawn-number))

     (apply grid-score (find
                        (lambda (grid+validation)
                          (winning-grid (cadr grid+validation)))
                        (zip grids grid-validations))))))

(define-public (solve2 numbers)
  (let* ((pool (car numbers))
         (grids (cadr numbers))
         (grid-validations (map make-validation-array grids))
         (last-drawn-number #f)
         (non-winning-grids (zip grids grid-validations)))

    (*
     (do ((drawn-number (car pool) (car pool))
          (pool (cdr pool) (cdr pool)))
         ((every winning-grid grid-validations) last-drawn-number)
       (set! non-winning-grids (filter
                                (lambda (grid+validation)
                                  (not (winning-grid (cadr grid+validation))))
                                (zip grids grid-validations)))
       (set! grid-validations
               (map
                (lambda (grid validation)
                  (apply-bingo grid validation drawn-number))
                grids grid-validations))
       (set! last-drawn-number drawn-number))

     (apply grid-score (car non-winning-grids)))))

(define (grid-score grid validation)
  (let* ((total 0)
         (dimensions (array-dimensions validation))
         (height (car dimensions))
         (width (cadr dimensions)))
    ;; Erk
    (fold
     (lambda (row acc)
       (+ acc
          (fold
           (lambda (column acc)
             (if (not (array-ref validation row column))
                 (+ acc (array-ref grid row column))
                 acc))
           0 (iota width))))
     0 (iota height))))

(define (make-validation-array grid)
  "Return an array with the same shape as grid, but filled with #f"
  ;; There must be a better solution
  (apply make-typed-array 'b #f (array-shape grid)))

(define (apply-bingo grid validation drawn-number)
  (array-map! validation
              (lambda (n b)
                (or b (eq? n drawn-number)))
              grid validation)
  validation)

(define (winning-grid validation-array)
  (let* ((dimensions (array-dimensions validation-array))
         (height (car dimensions))
         (width (cadr dimensions)))
    (or
     ;; Test rows
     (any
      (lambda (row)
        (fold
         (lambda (column acc)
           (and acc (array-ref validation-array row column)))
         #t (iota width)))
      (iota height))
     ;; Test columns
     (any
      (lambda (column)
        (fold
         (lambda (row acc)
           (and acc (array-ref validation-array row column)))
         #t (iota height)))
      (iota width))
     )))

(define-public (parse input)
  (let ((sections (parse-sections input))
        (string->number10 (lambda (s) (string->number s 10))))
    (list

     (map string->number10 (string-split (caar sections) #\,))

     (map
      (lambda (section)
        (list->array
         2
         (map
          (lambda (line)
            (map
             string->number10
             (string-split-non-empty line #\space)))
          section)))

      (cdr sections)))))

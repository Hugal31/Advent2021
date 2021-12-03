(define-module (days day03)
  #:use-module (advent-utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-60))

(define-public (parse-and-solve1 input)
  (solve1 (parse-ints-file input 2)))

(define-public (parse-and-solve2 input)
  (solve2 (parse-ints-file input 2)))

(define-public (solve1 numbers)
  (let* ((max-bit-numbers (max-numbers-bit numbers))
         (gamma-rate (get-gamma-rate numbers))
         (epsilon-rate (bit-extract (lognot gamma-rate) 0 (1+ max-bit-numbers))))
    (* gamma-rate epsilon-rate)))

(define-public (solve2 numbers)
  (* (get-rating numbers #t)
     (get-rating numbers #f)))

(define (get-gamma-rate numbers)
  (get-rate numbers #t))

(define (get-rate numbers bit-value)
  (define max-bit-length (1+ (max-numbers-bit numbers)))

  (fold
   (lambda (index acc)
     (if (eq? (most-common-bit index numbers) bit-value)
         (logior acc (integer-expt 2 index))
         acc))
   0 (iota max-bit-length)))

(define (get-rating numbers search-most-bits)
  (define max-bit-length (1+ (max-numbers-bit numbers)))

  (car
   (fold
    (lambda (index numbers)
      (if (eq? 1 (length numbers))
          numbers
          (let ((common-bit (and (most-common-bit index numbers) #t)))

            (filter
             (lambda (number)
               (eq? search-most-bits
                    (eq? common-bit (logbit? index number))))
             numbers))))
    numbers (iota max-bit-length (1- max-bit-length) -1))))

(define (most-common-bit pos numbers)
  "Returns #t if there is more 1 than 0 at pos in numbers"
  (match
      (fold
       (lambda (number acc)
         (+ acc (or (and (logbit? pos number) 1) -1)))
       0 numbers)
    (0 0)
    (a (positive? a))))

(define (bool->integer bool) (or (and bool 1) 0))

(define (max-numbers-bit numbers)
  (fold
   (lambda (number acc) (max acc (1- (integer-length number))))
   0 numbers))

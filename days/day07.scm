(define-module (days day07)
  #:use-module (advent-utils)
  #:use-module (ggspec lib)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-171))

(define-public (solve1 numbers)
  (compute-total-consumption distance numbers (median numbers)))

(define-public (solve2 numbers)
  (let ((goal (round (apply mean numbers))))
    ;; Test goal, goal+1 and goal-1
    (apply
     min
     (map
      (lambda (goal)
        (compute-total-consumption compute-fuel-consumption numbers goal))
      (list goal (1+ goal) (1- goal))))))

(define (compute-total-consumption consumption-proc poses goal)
  (list-transduce
   (tmap
    (lambda (pos)
      (consumption-proc pos goal)))
   +
   poses))

(define (distance from to)
  (abs (- from to)))

(define (compute-fuel-consumption from to)
  (let ((distance (abs (- from to))))
    (/ (* distance (1+ distance)) 2)))

(define (median numbers)
  (let* ((sorted-numbers (sort numbers <))
         (len (length numbers)))

    (if (odd? len)
        (list-ref sorted-numbers (/ (1- len) 2))
        (mean
         (list-ref sorted-numbers (/ len 2))
         (list-ref sorted-numbers (1- (/ len 2)))))))

(define mean
  (lambda numbers
    (/ (apply + numbers) (length numbers))))

(define-public (parse input)
  (parse-numbers-on-single-line input #\,))

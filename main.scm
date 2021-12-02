#!/usr/bin/env -S guile --no-auto-compile -e main -s
!#

;; Not sure if idiomatic
(define this-directory (dirname (current-filename)))
(define input-directory (string-append this-directory "/inputs"))

(add-to-load-path this-directory)

(use-modules (ice-9 format)
             (srfi srfi-1)
             )

(define (main args)
  (unless (eq? 3 (length args))
    (error "Not enough arguements in" args))

  (let* ((day-number (string->number (cadr args)))
         (module-name `(days ,(string->symbol (format #f "day~2,'0d" day-number))))
         (module (resolve-module module-name #:ensure #f))
         (challenge (caddr args)))

    (unless module
      (error "Could not resolve module" module-name))

    (if (equal? "test" challenge)
        (let* ((unit-tests (module-ref module 'unit-tests)))

          (unless unit-tests
            (error "Could not resolve unit-tests in" module))

          (unit-tests))

        (let* ((challenge-number (string->number challenge))
               (function-name (format #f "parse-and-solve~d" challenge-number))
               (solve-function (module-ref module (string->symbol function-name)))
               (input-file (format #f "~a/day~2,'0d.txt" input-directory day-number)))

          (unless solve-function
            (error "Could not find function" function-name))

          (display (call-with-input-file input-file solve-function))
          (newline)))))

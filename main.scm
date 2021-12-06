#!/usr/bin/env -S guile --no-auto-compile -e main -s
!#

;; Not sure if idiomatic
(define this-directory (dirname (current-filename)))
(define input-directory (string-append this-directory "/inputs"))

(add-to-load-path this-directory)

(use-modules (ice-9 format)
             (ice-9 getopt-long)
             (srfi srfi-1))

(define option-spec
  '((help (single-char #\h) (value #f))
    (bench (single-char #\b) (value #f))))

(define (main args)
  (let* ((options (getopt-long args option-spec))
         (args (cdar options))
         (help-required (option-ref options 'help #f))
         (bench-required (option-ref options 'bench #f)))

    (if help-required
        (format #t "~a DAY_NUMBER PART_NUMBER [--help]\n" (car args))
        ;; (unless (eq? 3 (length args))
        ;;   (error "Not enough arguements in" args))

        (begin
          (unless (eq? 2 (length args))
            (error "Not enough arguments"))

          (let* ((day-number (string->number (car args)))
                 (challenge-number (string->number (cadr args))))

            (unless (and day-number challenge-number)
              (error "Invalid numbers in args ~a" args))

            (resolve-and-run-part day-number challenge-number #:bench? bench-required))))))

(define* (resolve-and-run-part day-number challenge-number #:key (bench? #f))
  (let* ((module-name `(days ,(string->symbol (format #f "day~2,'0d" day-number))))
         (module (resolve-module module-name #:ensure #f)))

    (unless module
      (error "Could not resolve module" module-name))

    (let* ((solve-function-name (format #f "solve~d" challenge-number))
           (solve (module-ref module (string->symbol solve-function-name)))
           (parse (module-ref module 'parse))
           (input-file (format #f "~a/day~2,'0d.txt" input-directory day-number)))

      (unless solve
        (error "Could not find function" function-name))

      ((if bench? bench-part run-part) solve (call-with-input-file input-file parse)))))

(define (run-part solve input)
  (display (solve input))
  (newline))

(define* (bench-part solve input #:key (count 1000))
  (let ((start-time (get-internal-real-time))
        (start-second (current-time)))

    (do ((i 0 (1+ i))) ((= i count))
      (solve input))

    (let* ((end-time (get-internal-real-time))
           (end-second (current-time))
           (total-time (- end-time start-time))
           (total-time-in-sec (/ total-time internal-time-units-per-second))
           (total-time-per-iter-in-sec (/ (/ total-time count) internal-time-units-per-second))
           (total-seconds (- end-second start-second)))

      (format #t "~d iterations took ~f seconds (~f), so ~fms (~f) per iteration.\n"
              count
              total-seconds
              total-time-in-sec
              (* 1000 (/ total-seconds count))
              (* 1000 total-time-per-iter-in-sec)))))

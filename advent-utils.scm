(define-module (advent-utils)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-41)
  #:export (parse-lines-file-stream
            parse-ints-file))

(define-public (parse-lines-file port)
  (let* ((result '()))

    (do ((line (read-line port) (read-line port)))
        ((eof-object? line))
        (set! result (cons line result)))

    (reverse result)))

(define-stream (parse-lines-file-stream port)
  (let* ((line (read-line port)))
    (if (eof-object? line)
        stream-null
        (stream-cons line (parse-lines-file-stream port)))))

(define* (parse-ints-file port #:optional (base 10))
  (stream->list
   (stream-map
    (lambda (line) (string->number line base))
    (parse-lines-file-stream port))))

(define-public (parse-sections port)
  (let ((result '())
        (current-section '()))

    (stream-for-each
     (lambda (line)
       (if (string-null? line)
           (unless (null? current-section)
             (set! result (cons (reverse current-section) result))
             (set! current-section '()))

           (set! current-section (cons line current-section))))
     (parse-lines-file-stream port))

    (unless (null? current-section)
      (set! result (cons (reverse current-section) result)))

    (reverse result)))

(define-public (string-split-non-empty str char_pred)
  (filter (lambda (s) (not (string-null? s)))
          (string-split str char_pred)))

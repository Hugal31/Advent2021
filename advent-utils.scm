(define-module (advent-utils)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-41))

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

(define-public (parse-ints-file port)
  (stream->list (stream-map string->number (parse-lines-file-stream port))))

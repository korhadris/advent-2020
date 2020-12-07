#lang racket

(define (get-response line)
  (for/set ([response (in-string line)]) response))


(define (read-group input)
  (if (eof-object? (peek-char input))
      eof
      (for/list ([line (in-lines input)]
                 #:break (= 0 (string-length line)))
        (for/set ([response (in-string line)]) response))))

(define (get-data input)
  (for/list ([group (in-port read-group input)]) group))

(define (unique-responses group)
  ;; Why isn't there just set-length? (Besides the wording...)
  (length (set->list (apply set-union group))))

(define (common-responses group)
  (length (set->list (apply set-intersect group))))

(module+ main
  (define data (call-with-input-file "2020-06.data" get-data))
  (for/sum ([group (in-list data)]) (unique-responses group))
  (for/sum ([group (in-list data)]) (common-responses group)))

#lang racket

(define (parse-password line)
  (define pw-values (regexp-match #rx"([0-9]+)-([0-9]+) (.): (.*)" line))
  (define val1 (string->number (second pw-values)))
  (define val2 (string->number (third pw-values)))
  (define required-char (string-ref (fourth pw-values) 0))
  (define password (fifth pw-values))
  (list val1 val2 required-char password))

(define (valid-old-password? password-info)
  (define-values (min# max# required-char password)
    (apply values password-info))
  (define char# (for/sum ([test-char (in-string password)]
                          #:when (char=? required-char test-char))
                  1))
  (<= min# char# max#))

(define (valid-new-password? password-info)
  (define-values (pos1 pos2 required-char password)
    (apply values password-info))
  (define match-1 (char=? required-char (string-ref password (sub1 pos1))))
  (define match-2 (char=? required-char (string-ref password (sub1 pos2))))
  (xor match-1 match-2))

(module+ main
  (define passwords
    (call-with-input-file "2020-02.data"
      (lambda (in) (for/list ([line (in-lines in)])
                     (parse-password line)))))
  (define old-valid
    (for/sum ([password (in-list passwords)])
      (if (valid-old-password? password) 1 0)))
  (define new-valid
    (for/sum ([password (in-list passwords)])
      (if (valid-new-password? password) 1 0)))
  (display (format "~a valid old passwords\n~a valid new passwords\n"
                   old-valid new-valid)))

#lang racket

(define (read-passport input)
  (if (eof-object? (peek-char input))
      eof
      (for/list ([key-value (string-split
                             (string-join (for/list ([line (in-lines input)]
                                                     #:break (= 0 (string-length line)))
                                            line) " "))])
        (define-values (key value)
          (apply values (rest (regexp-match #rx"([^:]*):(.*)" key-value))))
        (list (string->symbol key) value)))) 

(define (parse-data input)
  (for/list ([section (in-port read-passport input)])
    section))

(define (simple-valid-passport? passport)
  (for/and ([key '(byr iyr eyr hgt hcl ecl pid)])
    (assoc key passport)))

(define (valid-height? height)
  (define value-units (regexp-match #rx"([0-9]+)(cm|in)" height))
  (cond [(not value-units) #f]
        [(string=? "cm" (third value-units))
         (<= 150 (string->number (second value-units)) 193)]
        [else
         (<= 59 (string->number (second value-units)) 76)]))

(define (strict-valid-passport? passport)
  (and (simple-valid-passport? passport)
       (<= 1920 (string->number (second (assoc 'byr passport))) 2002)
       (<= 2010 (string->number (second (assoc 'iyr passport))) 2020)
       (<= 2020 (string->number (second (assoc 'eyr passport))) 2030)
       (valid-height? (second (assoc 'hgt passport)))
       (regexp-match? #px"^#[0-9a-f]{6,6}$" (second (assoc 'hcl passport)))
       (memq (string->symbol (second (assoc 'ecl passport))) '(amb blu brn gry grn hzl oth))
       (regexp-match? #px"^[0-9]{9,9}$" (second (assoc 'pid passport)))))

(module+ main
  (define data
    (call-with-input-file "2020-04.data" parse-data))
  (define simple-valid-passports
    (for/sum ([passport (in-list data)])
      (if (simple-valid-passport? passport) 1 0)))
  (display (format "~a/~a valid passports (simple)\n" simple-valid-passports (length data)))
  (define strict-valid-passports
    (for/sum ([passport (in-list data)])
      (if (strict-valid-passport? passport) 1 0)))
  (display (format "~a/~a valid passports (strict)\n" strict-valid-passports (length data))))


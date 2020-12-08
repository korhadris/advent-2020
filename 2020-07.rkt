#lang racket

(define (parse-bag line)
  (define (bag->symbol bag)
    (string->symbol (regexp-replace #rx"(.*) (.*) bag.*" bag "\\1-\\2")))
  (define bag-match
    (regexp-match #rx"(.* bags) contain (.*)\\." line))
  (list (bag->symbol (second bag-match))
        (if (regexp-match? #rx"^[0-9]" (third bag-match))
            (for/list ([sub-bag (string-split (third bag-match) ", ")])
              (define sub-match (regexp-match #rx"([0-9*]) (.*)" sub-bag))
              (cons (bag->symbol (third sub-match)) (string->number (second sub-match))))
            empty)))

(define (get-data input)
  (for/list ([line (in-lines input)])
    (parse-bag line)))

(define (make-reverse-bag-hash bags)
  (define bag-hash (make-hasheq))
  (for* ([bag (in-list bags)]
         [sub-pair (in-list (second bag))])
    (define sub-hash (hash-ref! bag-hash (car sub-pair) (make-hasheq)))
    (hash-set! sub-hash (first bag) (cdr sub-pair)))
  bag-hash)

(define (find-containing-bags bag-hash bag-type)
  (apply append
   (for/list ([(container quantity) (in-hash (hash-ref bag-hash bag-type (make-hasheq)))])
     (append (list container) (find-containing-bags bag-hash container)))))

(define (make-bag-hash bags)
  (define bag-hash (make-hasheq))
  (for ([bag (in-list bags)])
    (hash-set! bag-hash (first bag) (second bag)))
  bag-hash)

(define (count-bags bag-hash bag-type)
  (for/sum ([sub-bag (in-list (hash-ref bag-hash bag-type empty))])
    (* (cdr sub-bag) (add1 (count-bags bag-hash (car sub-bag))))))

(module+ main
  (define data (call-with-input-file "2020-07.data" get-data))
  (define reverse-bag-hash (make-reverse-bag-hash data))
  (display (format "~a bags have at least one gold bag\n"
                   (for/sum ([bag (list->set
                                   (find-containing-bags reverse-bag-hash 'shiny-gold))])
                     1)))
  (define bag-hash (make-bag-hash data))
  (display (format "One gold bag has ~a other bags inside\n"
                   (count-bags bag-hash 'shiny-gold))))

#lang racket

(define (parse-line line)
  (define line-match
    (regexp-match #rx"(.*) ([-+][0-9]*)" line))
  (cons (string->symbol (second line-match))
        (string->number (third line-match))))

(define (get-data input)
  (for/vector ([line (in-lines input)])
    (parse-line line)))

(define (process-data data accumulator index processed swap)
  (cond [(hash-has-key? processed index)  ;; Infinite loop found
         (list accumulator index swap)]
        [(<= (vector-length data) index)  ;; End of program
         (list accumulator #f swap)]
        [else
         (define op (if (eq? index swap)
                        (match (car (vector-ref data index))
                          ['jmp 'nop]
                          ['nop 'jmp]
                          ['acc 'acc])
                        (car (vector-ref data index))))
         (define value (cdr (vector-ref data index)))
         (match op
           ['acc
            (process-data data (+ accumulator value) (add1 index) (hash-set processed index 1) swap)]
           ['jmp
            (process-data data accumulator (+ index value) (hash-set processed index 1) swap)]
           ['nop
            (process-data data accumulator (add1 index) (hash-set processed index 1) swap)])]))

(define (display-results results)
  (if (second results)
      (display (format "ACC:~a LOOP:~a\n" (first results) (second results)))
      (display (format "ACC:~a SWAP:~a\n" (first results) (third results)))))

(module+ main
  (define data (call-with-input-file "2020-08.data" get-data))
  (display-results (process-data data 0 0 (hasheq) #f))
  (for ([index (in-range (vector-length data))]
        #:when (not (second (process-data data 0 0 (hasheq) index))))
    (display-results (process-data data 0 0 (hasheq) index))))

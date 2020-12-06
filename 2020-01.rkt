#lang racket

(require net/url)

(define (get-data input)
  (sort (for/list ([line (in-lines input)])
          (string->number line))
        <))

(define (find-product data pair-sum current)
  (cond [(empty? data) #f]
        [(= pair-sum (+ current (first data)))
         (display (format "~a ~a\n" current (first data)))
         (* current (first data))]
        [(equal? 1 (length data)) #f]
        ;; [(< pair-sum (+ current (first data))) #f]  ;; This saves lots of time...
        [else
         (find-product (rest data) pair-sum current)]))

(define (find-pair data pair-sum)
  (if (empty? data)
      #f
      (or (find-product (rest data) pair-sum (first data))
          (find-pair (rest data) pair-sum))))

(define (find-triple data target)
  (cond [(< (length data) 3) #f]
        [else
         (define current (first data))
         (define pair-sum (- target current))
         (define pair-product (find-pair (rest data) pair-sum))
         (if pair-product
             (begin
               (displayln current)
               (* current pair-product))
             (find-triple (rest data) target))]))

(module+ main
  (define data 
    (call-with-input-file "2020-01.data" get-data))
  (display (format "Part 1 product: ~a\n" (find-pair data 2020)))
  (display (format "Part 2 product: ~a\n" (find-triple data 2020))))

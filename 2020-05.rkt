#lang racket

(define (seat->id seat)
  (string->number (regexp-replace* #rx"[FL]" (regexp-replace* #rx"[BR]" seat "1") "0") 2))

(define (get-data input)
  (sort
   (for/list ([line (in-lines input)])
     (seat->id line))
   <))

(define (find-empty-seat seats-taken)
  ;; Ignoring the case where they are all full
  (define prev-seat (first seats-taken))
  (define next-seat (second seats-taken))
  (if (= prev-seat (sub1 next-seat))
      (find-empty-seat (rest seats-taken))
      (add1 prev-seat)))

(module+ main
  (define data (call-with-input-file "2020-05.data" get-data))
  (display (format "Highest numbered seat: ~a\n" (last data)))
  (display (format "First empty seat: ~a\n" (find-empty-seat data))))

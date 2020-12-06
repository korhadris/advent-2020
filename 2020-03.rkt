#lang racket

(struct tree-map (data rows columns))

(define (parse-map input)
  (define data
    (for/vector ([line (in-lines input)])
      (for/vector ([map-char (in-string line)])
        (char=? #\# map-char))))
  (tree-map data (vector-length data) (vector-length (vector-ref data 0))))

(define (is-tree? tree-map row-in col-in)
  (define row (modulo row-in (tree-map-rows tree-map)))
  (define col (modulo col-in (tree-map-columns tree-map)))
  (vector-ref (vector-ref (tree-map-data tree-map) row) col))

(define (num-trees-hit tree-map right down)
  (define trees-hit
    (for/sum ([row (in-range down (tree-map-rows tree-map) down)]
              [col (in-naturals 1)])
      (if (is-tree? tree-map row (* col right)) 1 0)))
  (display (format "~a right, ~a down -> ~a trees hit\n"
                   right down trees-hit))
  trees-hit)

(module+ main
  (define tree-data
    (call-with-input-file "2020-03.data" parse-map))
  ;; (display (format "Tree map: ~a x ~a\n"
  ;;                  (tree-map-rows tree-data) (tree-map-columns tree-data)))
  (* (num-trees-hit tree-data 1 1)
     (num-trees-hit tree-data 3 1)
     (num-trees-hit tree-data 5 1)
     (num-trees-hit tree-data 7 1)
     (num-trees-hit tree-data 1 2)))

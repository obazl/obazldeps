;; stuff inspired by clojure

(define (dec n)
  (- n 1))

(define (nthrest coll n)
  "Returns the nth rest of coll, coll when n is 0."
  (if (> n 0)
      (nthrest (cdr coll) (dec n))
        coll))

;; partition-nth
;; partitions list in groups of n elements
;; like Clojure's partition; see also take-nth
;(define (partition-nth n lst)



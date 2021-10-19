;; https://srfi.schemers.org/srfi-152/srfi-152.html
;; String library (reduced)

;; see   ~/scheme/srfi-152

(load "srfi.scm")

(define (string-parse-start+end proc s args)
  (if (not (string? s)) (error "Non-string value" proc s))
  (let ((slen (string-length s)))
    (if (pair? args)
	(let ((start (car args))
	      (args (cdr args)))
	  (if (and (integer? start) (exact? start) (>= start 0))
	      (receive (end args)
		  (if (pair? args)
		      (let ((end (car args))
			    (args (cdr args)))
			(if (and (integer? end) (exact? end) (<= end slen))
			    (values end args)
			    (error "Illegal substring END spec" proc end s)))
		      (values slen args))
		(if (<= start end) (values args start end)
		    (error "Illegal substring START/END spec"
			   proc start end s)))
	      (error "Illegal substring START spec" proc start s)))
	(values '() 0 slen))))

(define (string-parse-final-start+end proc s args)
  (receive (rest start end) (string-parse-start+end proc s args)
    (if (pair? rest) (error "Extra arguments to procedure" proc rest)
	(values start end))))

;; (define-syntax let-string-start+end
;; (define-macro
;;   (let-string-start+end start end) proc s-exp args-exp body ...)
;;   `(let-values (((start end)
;;                  ,(string-parse-final-start+end proc s-exp args-exp)))
;;   (format #t "start ~A end ~A" start ent)))

;; srfi 152
(define (string-null? s) (zero? (string-length s)))

(define (string-split str ch)
  (let ((len (string-length str)))
    (letrec
      ((split
        (lambda (a b)
          (cond
            ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
              ((char=? ch (string-ref str b)) (if (= a b)
                (split (+ 1 a) (+ 1 b))
                  (cons (substring str a b) (split b b))))
                (else (split a (+ 1 b)))))))
                  (split 0 0))))

;; srfi 152
;;; Returns starting-position in STRING or #f if not true.
;;; This implementation is slow & simple. It is useful as a "spec" or for
;;; comparison testing with fancier implementations.
;;; See below for fast KMP version.

;; _very_ inefficient
(define (string-contains string subs) ;; . maybe-starts+ends)
 ;; (let-string-start+end2 (start1 end1 start2 end2)
 ;;                        string-contains string substring maybe-starts+ends
  (let* ((len1 (length string))
         (len2 (length subs))
	 (i-bound (- len1 len2)))
     (let lp ((i 0))
       (if (> i i-bound)
           #f
           (let ((ss (substring string i)))
	     (if (string-prefix? subs ss)
	         i
	         (lp (+ i 1))))))))

;; srfi 152
(define (%substring s start end)
  (if (and (zero? start) (= end (string-length s))) s
      (substring s start end)))

(define (string-take s n)
  (check-arg string? s string-take)
  (check-arg (lambda (val) (and (integer? n) (exact? n)
				(<= 0 n (string-length s))))
	     n string-take)
  (%substring s 0 n))

(define (string-take-right s n)
  (check-arg string? s string-take-right)
  (let ((len (string-length s)))
    (check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
	       n string-take-right)
    (%substring s (- len n) len)))

(define (string-drop s n)
  (check-arg string? s string-drop)
  (let ((len (string-length s)))
    (check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
	       n string-drop)
  (%substring s n len)))

(define (string-drop-right s n)
  (check-arg string? s string-drop-right)
  (let ((len (string-length s)))
    (check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
	       n string-drop-right)
    (%substring s 0 (- len n))))


;; srfi 152
(define (string-index str criterion) ;; . maybe-start+end)
  ;; (let-string-start+end (start end) string-index str maybe-start+end
  (let ((start 0) (end (length str)))
    (let lp ((i start))
      (and (< i end)
           (if (criterion (string-ref str i)) i
               (lp (+ i 1)))))))

(define (string-index-right str criterion) ;; . maybe-start+end)
  ;; (let-string-start+end (start end) string-index-right str maybe-start+end
  (let ((start 0) (end (length str)))
	   (let lp ((i (- end 1)))
	     (and (>= i start)
		  (if (criterion (string-ref str i)) i
		      (lp (- i 1)))))))

;; srfi 152
(define (%string-prefix-length s1 start1 end1 s2 start2 end2)
  (let* ((delta (min (- end1 start1) (- end2 start2)))
	 (end1 (+ start1 delta)))

    (if (and (eq? s1 s2) (= start1 start2))	; EQ fast path
	delta

	(let lp ((i start1) (j start2))		; Regular path
	  (if (or (>= i end1)
		  (not (char=? (string-ref s1 i)
			       (string-ref s2 j))))
	      (- i start1)
	      (lp (+ i 1) (+ j 1)))))))

;; (define (%string-prefix? s1 start1 end1 s2 start2 end2)
;;   (let ((len1 (- end1 start1)))
;;     (and (<= len1 (- end2 start2))	; Quick check
;; 	 (= (%string-prefix-length s1 start1 end1
;; 				   s2 start2 end2)
;; 	    len1))))

(define (string-prefix? s1 s2)
  (let* ((len1 (length s1))
        (len2 (length s2))
        (delta (- len2 len1)))
    (and (<= len1 len2)	; Quick check
	 (= (%string-prefix-length s1 0 len1
				   s2 0 len2)
	    len1))))

(define (string-suffix? s1 s2)
  (let* ((len1 (length s1))
        (len2 (length s2))
        (delta (- len2 len1)))
    (and (<= len1 len2)
         (eq? delta
              (string-position s1 s2 delta)))))

;; srfi 13
(define (%string-prefix-length-ci s1 start1 end1 s2 start2 end2)
  (let* ((delta (min (- end1 start1) (- end2 start2)))
	 (end1 (+ start1 delta)))

    (if (and (eq? s1 s2) (= start1 start2))	; EQ fast path
	delta

	(let lp ((i start1) (j start2))		; Regular path
	  (if (or (>= i end1)
		  (not (char-ci=? (string-ref s1 i)
				  (string-ref s2 j))))
	      (- i start1)
	      (lp (+ i 1) (+ j 1)))))))

(define (%string-prefix-ci? s1 start1 end1 s2 start2 end2)
  (let ((len1 (- end1 start1)))
    (and (<= len1 (- end2 start2))	; Quick check
	 (= len1 (%string-prefix-length-ci s1 start1 end1
					   s2 start2 end2)))))

(define (string-prefix-ci? s1 s2)
  (let ((len1 (string-length s1))
        (len2 (string-length s2)))
    (and (<= len1 len2)
	 (= len1 (%string-prefix-length-ci s1 0 len1
                                           s2 0 len2)))))

;; srfi 152
(define (string-drop-right s n)
  (check-arg string? s string-drop-right)
  (let ((len (string-length s)))
    (check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
	       n string-drop-right)
    (%substring s 0 (- len n))))

;; string-capitalize from s7test.scm
(define (cl-string x)
  (if (string? x) x
      (if (char? x)
	  (string x)
	  (if (symbol? x)
	      (symbol->string x)
	      (error 'wrong-type-arg "string ~A?" x)))))

(define* (nstring-capitalize str-1 (start 0) end)
  (define (alpha? c)
    (or (char-alphabetic? c)
	(char-numeric? c)))
  (let ((str (cl-string str-1)))
    (let ((nd (if (number? end) end (length str))))
      (do ((i start (+ i 1)))
	  ((= i nd) str)
	(if (alpha? (str i))
	    (if (or (= i 0)
		    (not (alpha? (str (- i 1)))))
		(set! (str i) (char-upcase (str i)))
		(set! (str i) (char-downcase (str i)))))))))

;; upcase char 0, downcase the rest
(define* (string-capitalize str-1 (start 0) end)
  (let ((str (cl-string str-1)))
    (nstring-capitalize (copy str) start end)))


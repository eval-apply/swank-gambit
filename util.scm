
(define (unfold p f g seed . maybe-tail-gen)
  (if (pair? maybe-tail-gen)

      (let ((tail-gen (car maybe-tail-gen)))
	(if (pair? (cdr maybe-tail-gen))
	    (apply error "Too many arguments" unfold p f g seed maybe-tail-gen)

	    (let recur ((seed seed))
	      (if (p seed) (tail-gen seed)
		  (cons (f seed) (recur (g seed)))))))

      (let recur ((seed seed))
	(if (p seed) '()
	    (cons (f seed) (recur (g seed)))))))

(define (take lis k)
  (let recur ((lis lis) (k k))
    (if (zero? k) '()
	(cons (car lis)
	      (recur (cdr lis) (- k 1))))))

(define (drop lis k)
  (let iter ((lis lis) (k k))
    (if (zero? k) lis (iter (cdr lis) (- k 1)))))

(define (drop-right lis k)
  (let recur ((lag lis) (lead (drop lis k)))
    (if (pair? lead)
	(cons (car lag) (recur (cdr lag) (cdr lead)))
	'())))

;; srfi-1
(define (null-list? l)
  (cond ((pair? l) #f)
	((null? l) #t)
	(else (error "null-list?: argument out of domain" l))))

(define (fold-right kons knil lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))		; N-ary case
	(let ((cdrs (%cdrs lists)))
	  (if (null? cdrs) knil
	      (apply kons (%cars+ lists (recur cdrs))))))

      (let recur ((lis lis1))				; Fast path
	(if (null-list? lis) knil
	    (let ((head (car lis)))
	      (kons head (recur (cdr lis))))))))

(library (match-expanders)
  (export push-box! pattern-expanders syntax-car syntax-cdr
	  length=?
	  join-syntax-symbols)
(import (chezscheme))

(define (push-box! b v)
  (set-box! b (cons v (unbox b))))

(define pattern-expanders (box '()))

(define syntax-car
  (lambda (ls)
    (syntax-case ls ()
      ((x . y) #'x))))

(define syntax-cdr
  (lambda (ls)
    (syntax-case ls ()
      ((x . y) #'y))))

(define (length=? l n)
  ;; tests if a list has a certain length
  ;; failing early if possible
  ;; failing on non-lists
  (let loop ((l l) (n n))
    (cond ((< n 0) #f)
	  ((null? l) (= n 0))
	  ((pair? l) (loop (cdr l) (- n 1)))
	  (else #f))))

(define (join-syntax-symbols s1 . rest)
  (datum->syntax s1 (string->symbol (apply string-append
					   (symbol->string (syntax->datum s1))
					   (map (lambda (s)
						  (symbol->string (syntax->datum s)))
						rest)))))

)

(library (match-core)
  (export match match-pats)
(import (chezscheme) (match-expanders))

(define-syntax match
  (syntax-rules ()
    ((match <exp> (<pat> <body>) ...)
     (let* ((t <exp>) (stack (list t)))
       (match-aux t stack (<pat> <body>) ...)))))

(define-syntax match-aux
  (syntax-rules ()
    ((match-aux t stack)
     (error 'match "failed to match" t))
    ((match-aux t stack (<pat-1> <body-1>) (<pat> <body>) ...)
     (let ((fk (lambda () (match-aux t stack (<pat> <body>) ...))))
       (match-pats (<pat-1>) stack <body-1> fk)))))

(define-syntax match-pats
  (lambda (stx)
    (syntax-case stx ()
      ((match-pats () stack sk fk)
       #'(if (null? stack) sk (fk)))
      ((match-pats (p . ps) stack sk fk)
       (let ((pat^ (syntax->datum #'p)))
	 (cond ((symbol? pat^)
		#`(if (null? stack)
		      (fk)
		      (let ((p (car stack)) (stack (cdr stack)))
			(match-pats ps stack sk fk))))
	       ((or (null? pat^) (number? pat^)
		    (boolean? pat^) (char? pat^))
		#`(match-pats ('p . ps) stack sk fk))
	       (else (let ((expander (assoc (car pat^) (unbox pattern-expanders))))
		       (unless expander
			 (error 'match-pats "no expander for pattern" pat^))
		       ((cdr expander) (syntax->list #'p) #'ps #'stack #'sk #'fk)))))))))

)

(library (match)
(export match pattern-expanders foo define-record-matcher)
(import (chezscheme) (match-expanders) (match-core))

(define-syntax make-record-matcher
  (syntax-rules ()
    ((make-record-matcher rec rec? num-fields (get ...))
     (push-box! pattern-expanders
		(cons 'rec
		      (lambda (p ps stack sk fk)
			(let ((pps (cdr p)))
			  (unless (= num-fields (length pps))
			    (error 'make-record-matcher "bad record pattern" (syntax->datum p)))
			  #`(if (null? #,stack)
				(error 'quote-matcher "failure during record match")
				(let ((top (car #,stack)) (stack (cdr #,stack)))
				  (if (rec? top)
				      (let ((stack (list* (get top) ... stack)))
					(match-pats #,(append pps ps)
						    stack #,sk #,fk))
				      (#,fk)))))))))))

(define-syntax define-record-matcher
  (lambda (stx)
    (syntax-case stx ()
      ((define-record-matcher rec (field ...))
       (let* ((getters (map (lambda (f)
			      (join-syntax-symbols #'rec #'- f))
			    (syntax->list #'(field ...))))
	      (rec? (join-syntax-symbols #'rec #'?)))
	 #`(make-record-matcher rec #,rec? #,(length getters) #,getters))))))

;; > (import (match))
;; > (define-record kons (kar kdr))
;; > (define-record-matcher kons (kar kdr))
;; > (match (make-kons 1 2) ((kons x y) (list y x)))
;; (2 1)

(define (foo) #t)

(push-box! pattern-expanders
	   (cons 'quote
		 (lambda (p ps stack sk fk)
		   #`(if (null? #,stack)
			 (error 'quote-matcher "failure during match")
			 (let ((top (car #,stack)) (stack (cdr #,stack)))
			   (if (equal? '#,(cadr p) top)
			       (match-pats #,ps
					   stack #,sk #,fk)
			       (#,fk)))))))

(push-box! pattern-expanders
	   (cons 'cons
		 (lambda (p ps stack sk fk)
		   (let ((p-1 (cadr p))
			 (p-2 (caddr p)))
		     #`(if (null? #,stack)
			   (error 'quote-matcher "failure during match")
			   (let ((top (car #,stack)) (stack (cdr #,stack)))
			     (if (pair? top)
				 (let ((stack (list* (car top) (cdr top) stack)))
				   (match-pats #,(list* p-1 p-2 ps)
					       stack #,sk #,fk))
				 (#,fk))))))))

(push-box! pattern-expanders
	   (cons 'list
		 (lambda (p ps stack sk fk)
		   (let ((pps (cdr p)))
		     #`(if (null? #,stack)
			   (error 'quote-matcher "failure during match")
			   (let ((top (car #,stack)) (stack (cdr #,stack)))
			     (if (length=? top #,(length pps))
				 (let ((stack (append top stack)))
				   (match-pats #,(append pps ps)
					       stack #,sk #,fk))
				 (#,fk))))))))

(push-box! pattern-expanders
	   (cons 'quasiquote
		 (lambda (p ps stack sk fk)
		   (let* ((q (cadr p)) (q^ (syntax->datum q)))
		     (cond ((or (null? q^)
				(symbol? q^) (number? q^)
				(boolean? q^) (char? q^))
			    #`(match-pats ('#,q . #,ps)
					  #,stack #,sk #,fk))
			   ((eq? 'unquote (car q^))
			    (let ((p (syntax-car (syntax-cdr q))))
			      #`(match-pats (#,p . #,ps)
					    #,stack #,sk #,fk)))
			   (else
			    (let ((q-1 (syntax-car q)) (q-2 (syntax-cdr q)))
			      #`(match-pats ((cons (quasiquote #,q-1) (quasiquote #,q-2)) . #,ps)
					    #,stack #,sk #,fk))))))))

)

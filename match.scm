(library (match-definitions)
(export trie-merge compile-pattern interpret-tree)
(import (chezscheme))

(define (push-box! b v)
  (set-box! b (cons v (unbox b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the code for merging a list of instructions into a trie

(define terminator (cons '() '()))

(define (trie-merge seqs)
  (cond ((null? seqs)
	 '())
	((member '() seqs)
	 (cons terminator
               (trie-merge (filter pair? seqs))))
	(else (let* ((p (partition-by-head (caar seqs) seqs))
		     (tails (car p))
		     (rest (cdr p)))
		(cons `(,(caar seqs) . ,(trie-merge tails))
		      (trie-merge rest))))))

(define (partition-by-head head seqs)
  (cond ((null? seqs)
         terminator)
	(else (let* ((p (partition-by-head head (cdr seqs)))
 		     (l (car p))
 		     (r (cdr p)))
		(if (equal? head (caar seqs))
		    (cons (cons (cdar seqs) l) r)
		    (cons l (cons (car seqs) r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the interpreter for trees of matching instructions.
;; matching instructions are as defined in compile-pattern,
;; and the trees terminate with an (execute <code>) command

;; The interpreter handles scope explicitly and manages a
;; fail continuation to do backtracking.

(define-syntax interpret-tree
  (syntax-rules (execute bind compare-equal? guard decons)

    ((interpret-tree scope () stack failure)
     failure)

    ((interpret-tree scope (((execute <body>) (())) <alternatives> ...) stack failure)
     (let* scope <body>))

    ((interpret-tree scope (((bind <var>) <then> ...) <alternatives> ...) stack failure)
     (let ((top (car stack))
           (new-stack (cdr stack)))
       (interpret-tree ((<var> top) . scope) (<then> ...) new-stack
		       (interpret-tree scope (<alternatives> ...) stack failure))))

    ((interpret-tree scope (((compare-equal? <s-expr>) <then> ...) <alternatives> ...)
                     stack failure)
     (let ((top (car stack))
           (new-stack (cdr stack)))
       (if (let* scope (equal? top '<s-expr>))
           (interpret-tree scope (<then> ...) new-stack failure)
           (interpret-tree scope (<alternatives> ...) stack failure))))

    ((interpret-tree scope (((guard <predicate>) <then> ...) <alternatives> ...)
                     stack failure)
     (if (let* scope <predicate>)
         (interpret-tree scope (<then> ...) stack failure)
         (interpret-tree scope (<alternatives> ...) stack failure)))
    
    ((interpret-tree scope (((decons) <then> ...) <alternatives> ...) stack failure)
     (let ((top (car stack))
           (new-stack (cdr stack))
	   (fail-thunk (lambda ()
			 (interpret-tree scope (<alternatives> ...) stack failure))))
       (if (pair? top)
           (let ((stack (cons (car top) (cons (cdr top) new-stack))))
             (interpret-tree scope (<then> ...) stack (fail-thunk)))
	   (fail-thunk))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This part compiles a pattern into a sequence of matching instructions
;; that performs the testing and binding operations
;;
;; Update this part to change or extend the pattern language.

;; The syntax of patterns used here is:
;;
;; <pat>  ::= <var> | (<atom> <pat> ...)
;;
;; these functions are used to turn a pattern into a sequence of matching instructions

(define (var? s) (symbol? s))
(define (functor? s) (and (pair? s) (symbol? (car s))))

(define (compile-pattern box pat rest)
  (cond ((var? pat) (compile-var box pat rest))
	((functor? pat) (compile-functor box (car pat) (cdr pat) rest))
	(else (error 'compile-pattern "Invalid pattern" pat))))

(define (compile-var box var rest)
  (if (member var (unbox box))
      (cons `(compare-equal? ,var) rest)
      (begin
	(push-box! box var)
	(cons `(bind ,var) rest))))

(define (compile-functor box name args rest)
  (list* '(decons)
	 `(compare-equal? ,name)
	 (compile-args box args rest)))

(define (compile-args box args rest)
  (if (null? args)
      (cons '(compare-equal? ()) rest)
      (cons '(decons)
	    (compile-pattern box (car args)
			     (compile-args box (cdr args)
					   rest)))))

)

(library (match)
(export match)
(import (chezscheme) (match-definitions))

(define-syntax (match stx)
  (define (compile-pattern^ entry)
    (compile-pattern (box '())
		     (car entry)
		     (list `(execute ,(cadr entry)))))
  (syntax-case stx (else)
    ((match <exp> (<pattern> <body> ...) ... (else <fail> ...))
     (let* ((rules #'((<pattern> (begin <body> ...)) ...))
	    (instructions (map compile-pattern^ (syntax->datum rules)))
	    (trie (trie-merge instructions)))
       #`(let ((stack (list <exp>)))
	   (interpret-tree ()
			   #,(datum->syntax #'interpret-trie trie)
			   stack
			   (begin <fail> ...)))))
    ((match <exp> (<pattern> <body> ...) ...)
     #'(match <exp> (<pattern> <body> ...) ... (else (error 'match "Failed to match"))))))

)

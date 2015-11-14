; CS 131 Homework 5
; Scheme Code Diff Analyzer

; (define ns (make-base-namespace)) ; for test-compare-expr on DrRacket, uncomment this line
									; and add ns as second arg. to each eval call

; standard/literal compare
(define compare-std-expr
	(lambda (x y)
		(if (equal? x y)
			x
			(cond
				[(and (equal? x #t) (equal? y #f)) 'TCP]
				[(and (equal? y #t) (equal? x #f)) '(not TCP)]
				[else `(if TCP ,x ,y)]
			)
		)
	)
)

; element by element recursive list compare
(define compare-list-expr
	(lambda (x y)
		(if( or (equal? x '() ) (equal? y '() ))
			'()
			(cons (compare-expr (car x) (car y)) (compare-list-expr (cdr x) (cdr y)))
		)
	)
)

; check if let bindings match
(define let-bindings-match? 
	(lambda (x y)
		(if(and (equal? (length x) (length y)) (equal? (car(car x)) (car(car y)))) ; length and names same
			(if(and (> (length x) 1) (> (length y) 1))
				(let-bindings-match? (cdr x) (cdr y)) ; more bindings to check
				#t ; no more bindings to check
			)
			#f 	; first if failed. False.
		)
	)
)

;;; Part 1
;;; main compare expression
; if x and y are lists of the same length and have the same first element,
; check for special cases. Compare accordingly
; if not a special case, just recursively compare list
; otherwise compare literally
(define (compare-expr x y)
	(if (and (list? x) (list? y))
		(if (equal? (length x) (length y))
			(if (equal? (car x) (car y))
				(cond ; for special cases
					[(equal? (car x) 'lambda) 		; only some lambdas should be list compared
						(if (equal? (cadr x) (cadr y)) ; formals equal?
								(compare-list-expr x y)
								(compare-std-expr x y) 
						)
					]
					[(equal? (car x) 'quote) 
						(compare-std-expr x y) 		; quote is a literal compare
					]
					[(equal? (car x) 'let) 
						(if (let-bindings-match? (cadr x) (cadr y))
							(compare-list-expr x y)	; fine-grained compare if bindings match
							(compare-std-expr x y)	; literal compare bindings dont match
						)
					]
					[(equal? (car x) 'if) (compare-list-expr x y)]	; rec list compare for if
					[else (compare-list-expr x y)] 	; do rec list compare
				)
				; do some eval for car x not equal car y (one of 2 is keyword)
				(if (or (equal? (car x) 'let) (equal? (car x) 'lambda) 
						(equal? (car x) 'if) (equal? (car x) 'quote)
						(equal? (car y) 'let) (equal? (car y) 'lambda) 
						(equal? (car y) 'if) (equal? (car y) 'quote))
					(compare-std-expr x y)  ; perform literal compare
					(compare-list-expr x y) ; perform fine-grained compare 
				)
			)
			(compare-std-expr x y) ; perform literal compare
		)
		(compare-std-expr x y) ; compare literal x and y
	)
)

; deal with eval scoping issue
(define scoped-eval-list
	(lambda (a b)
		(if b
			(cons 'let (cons '((TCP #t)) (cons a '())))
			(cons 'let (cons '((TCP #f)) (cons a '())))
		)
	)
)

; Part 2
; Test Compare Expression
(define (test-compare-expr x y)
	(and (equal? (eval x) (eval (scoped-eval-list (compare-expr x y) #t)))
		 (equal? (eval y) (eval (scoped-eval-list (compare-expr x y) #f)))
	)
)

; Part 3

(define test-x
	'( 
		; test lambda with matching formals
		(lambda (a b c)
			; test if and simple compare
			(if a
				; test let with matched binding name but unmatched binding val
				(let ((d b)) (+ d c))
				; test #t vs #f
				(if #t
					; test let with unmatched binding name
					(let ((l 2)) (- l 1))
					; test lambda with unmatched formals
					(lambda (e g)
						; this isn't modified due to prev. unmatched formals
                        (let ((h e)) (> h 1))
                    )
				) 
			)
		)
	)
)

(define test-y
	'(
		(lambda (a b c) 
			(if b
				(let ((d c)) (+ d c))
				(if #f
					(let ((m 2)) (- l 1))
					(lambda (e f)
                        (let ((h e)) (> h 2))
                    )
				)
			)
		)
	)
)

; to test test-x and test-y uncomment next line
; (compare-expr test-x test-y)
; result: ((lambda (a b c) (if (if TCP a b) (let ((d (if TCP b c))) (+ d c)) (if TCP (if TCP (let ((l 2)) (- l 1)) (let ((m 2)) (- l 1))) (if TCP (lambda (e g) (let ((h e)) (> h 1))) (lambda (e f) (let ((h e)) (> h 2))))))))

; test cases for compare-expr and test-compare-expr removed. 
; predicates were tested with given test cases + some expansion on given cases
; found on project description
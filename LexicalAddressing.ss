; Determines if a variable occurs free and if it occurs bound in an LcExpr
; <LcExpr> :: = <identifier> |
;				(lambda (<identifier*>) <LcExpr>)|
;				(<LcExpr> <LcExpr>)|
;				(if <LcExpr> <LcExpr> <LcExpr>)|
;				(let ((<identifier> <LcExpr>)+) <LcExpr>)|
;				(set! <indentifier> <lcExpr>)
				

(define occurs-free?
	(lambda (var expr)
		(cond
			; var must be a symbol, but not one of our reserved words
			((not (valid-var? var)) #f)
			; if the var is the expr, it is free
			((symbol? expr) (eqv? var expr))
			; if the expr is a lambda
			((eqv? (car expr) 'lambda)
				(occurs-free-in-lambda? var expr))
			; if the expr is a let
			((eqv? (car expr) 'let)
				; occurs on right hand side of def or not in defs but in body
				(let ((secondsls (seconds (cadr expr))))
					; checks if on right hand side of defs
					(or (ormap (lambda (exprl) (occurs-free? var exprl)) secondsls)
						; check if free in body when not defined
						(and (not (memq var (firsts (cadr expr)))) (occurs-free? var (caddr expr))))))
			; if the expr is a let* convert to lets
			((eqv?	(car expr) 'let*)
				(occurs-free? var (let*->let expr)))
			; if the expr is an if expression
			((eqv? (car expr) 'if)
				; and it does occur free in test, then, or else parts
				(or (occurs-free? var (cadr expr))
					(occurs-free? var (caddr expr))
					(occurs-free? var (cadddr expr))))
			; if the expr is a set expression
			((eqv? (car expr) 'set!)
				; it is free if it is the second symbol
				(eqv? (caddr expr) var))
			; if the var is free in the first or second part of the application
			(else (ormap (lambda (expr2) (occurs-free? var expr2)) expr)))))
			
(define occurs-bound?
	(lambda (var expr)
		(cond
			; var must be a symbol, but not one of our reserved words
			((not (valid-var? var)) #f)
			; if the expr is just a var, it contains nothing bound
			((symbol? expr) #f)
			; if the expr is a lambda
			((eqv? (car expr) 'lambda)
				(occurs-bound-in-lambda? var expr))
			; if the expr is a let
			((eqv? (car expr) 'let)
				; occurs on lhs of def and free in bod, bound on rhs of def, or not in defs but in body
				(let ((firstsls (firsts (cadr expr)))
						(secondsls (seconds (cadr expr))))
					;if on lhs of def and free in bod
					(or (and (ormap (lambda (exprl) (occurs-free? var exprl)) firstsls)
							(occurs-free? var (caddr expr)))
						; or bound in rhs of def
						(ormap (lambda (expr1) (occurs-bound? var expr1)) secondsls)
						; check if bound in body when not defined
						(and (occurs-bound? var (caddr expr))))))
			; if the expr is a let* convert to lets
			((eqv?	(car expr) 'let*)
				(occurs-bound? var (let*->let expr)))
			; if the expr is an if expression
			((eqv? (car expr) 'if)
				; and it does occur free in test, then, or else parts
				(or (occurs-bound? var (cadr expr))
					(occurs-bound? var (caddr expr))
					(occurs-bound? var (cadddr expr))))
			; if the expr is a set expression
			((eqv? (car expr) 'set!)
				; it occurs bound if it is bound in the second arg to set
				(occurs-bound? var (caddr expr)))
			; if the var is bound in the first or second part of the application
			(else (ormap (lambda (expr2) (occurs-bound? var expr2)) expr)))))
					
(define firsts
	(lambda (ls)
		(if (null? ls)
			'()
			(cons (car (car ls)) (firsts (cdr ls))))))
			
(define seconds
	(lambda (ls)
		(if (null? ls)
			'()
			(cons (cadr (car ls)) (seconds (cdr ls))))))
			
			
(define valid-var?
	(lambda (var)
		(if (or (eqv? var 'let) (eqv? var 'lambda) 
			(eqv? var 'if) (eqv? var 'let*) (eqv? var 'lambda) (eqv? var 'set!))
				#f
				(symbol? var))))
				
(define let*->let
	(lambda (expr)
		; strip off the let (i.e. ignore the car)
		; then call recursive procedure on the cadr
		; then attach the caddr
		(let*->let-help (cadr expr) (caddr expr))))
			
(define let*->let-help
	(lambda (expr last)
		; if you're out of subexpressions, return the last elem in the let* expr
		(cond	[(null? expr) last]
				; if you are in the middle, put the current in its own parens and let
				[else (list 'let (list (car expr)) (let*->let-help (cdr expr) last))])))			
			
(define occurs-free-in-lambda?
	(lambda (var lexpr)
		; it's not free if it occurs in the args to the lambda
		(if (memq var (cadr lexpr))
			#f
			(occurs-free? var (caddr lexpr)))))
			
(define occurs-bound-in-lambda?
	(lambda (var lexpr)
		; it's bound if it occurs bound in the body
		; or if it occurs in both the args and body
		(if (memq var (cadr lexpr))
			(occurs-free? var (caddr lexpr))
			(occurs-bound? var (caddr lexpr)))))
			
;;  takes an LcExpr and returns a copy with every bound occurrence of a variable v replaced by a list (: depth position)

(define (lexical-address slist)
	(lexical-address-help slist '()))
	
; scopLs is a backward list of all the lists args to lambdas and defs in lets
(define (lexical-address-help expr scopLs)
    (cond
		; if the expr is just a var
		((symbol? expr)
			(let ([depthPos (lookup expr scopLs 0)])
				; if the variable isn't bound
				(if (not depthPos)
					(list ': 'free expr)
					; if it is bound
					(cons ': depthPos))))
		; if the expr is a lambda
		((eqv? (car expr) 'lambda)
			; make a list of lambda, the args, and the recursive call on the body
			; note that the args to current lambda are added to scopLs
			(list 'lambda (cadr expr)
				(lexical-address-help (caddr expr) (cons (cadr expr) scopLs))))
		; if the expr is a let
		((eqv? (car expr) 'let)
			; make a list of let, its list of defs, and the recursive call on the body
			; note that the firsts of the defs (the vars defined)
			; are added to scopLs for the body
			(list 'let (let-defs (cadr expr) scopLs)
				(lexical-address-help (caddr expr) 
					(cons (firsts (cadr expr)) scopLs))))
		; if the expr is an if expression
		((eqv? (car expr) 'if)
			; return a list of if and a list of three sublists, one for the recursive call
			; of the test, the then, and the else
			(list 'if 
				(lexical-address-help (cadr expr) scopLs)
				(lexical-address-help (caddr expr) scopLs)
				(lexical-address-help (cadddr expr) scopLs)))
		; if the expr is a set expression
		((eqv? (car expr) 'set!)
			; make a list of set!, the first "arg", and the recursive call one what it's set to
			(list 'set! (cadr expr) (lexical-address-help (caddr expr) scopLs)))
		; if there is a list of vars, lexical-address all of them
		(else (map (lambda (expr1) (lexical-address-help expr1 scopLs)) expr))))

(define lookup
	; call with depth = 0 yourself; recursive calls increment depth
	(lambda (id scopLs depth)
		; if you've no more vars left, return false
		(if (null? scopLs)
			#f
			; if you have found the identifier you're looking for in the current list of vars
			(if (memq id (car scopLs))
				(list depth (pos-in id (car scopLs) 0))
				; otherwise, increment the depth by one and look at the next deepest layer of scopLs
				(lookup id (cdr scopLs) (+ 1 depth))))))
				
(define pos-in
	(lambda (elem ls ind)
		(if (null? ls)
			#f
			(if (equal? elem (car ls))
				ind
				(pos-in elem (cdr ls) (+ 1 ind))))))

			
(define let-defs
	(lambda (defLs scopLs)
		; lexical addr all second elems in pairs
		(map (lambda (pair) 
			(list (car pair) (lexical-address-help (cadr pair) scopLs)))
				defLs)))
				
;; reverses the above procedure
		
(define un-lexical-address
	(lambda (ls)
		(un-lexical-address-help ls '())))

(define (un-lexical-address-help expr scopLs)
	(cond
		; if expr is null, return the empty list
		;((null? expr) '())
		; if the expr is just a symbol, return it
		((symbol? expr) expr)
		; if the expr is just a free var
		((equal? 'free (cadr expr))
			; return that variable
			(caddr expr))
		; if the expr is a var that's not free
		((number? (cadr expr))
			; get the var it's supposed to be
			(get-by-ref (cdr expr) scopLs))
		; if the expr is a lambda
		((eqv? (car expr) 'lambda)
			; make a list of lambda, the args, and the recursive call on the body
			; note that the args to current lambda are added to scopLs
			;(let ((args (un-lexical-address-help (cadr expr) scopLs)))
				(list 'lambda (cadr expr)
					(un-lexical-address-help (caddr expr) (cons (cadr expr) scopLs))))
		; if the expr is a let
		((eqv? (car expr) 'let)
			; make a list of let, its list of defs, and the recursive call on the body
			; note that the firsts of the defs (the vars defined) are added to scopLs for the body
			; note that the defs need un-lexical-addressing
			(list 'let (un-let-defs (cadr expr) scopLs)
				(un-lexical-address-help (caddr expr) 
					(cons (firsts (cadr expr)) scopLs))))
		; if the expr is an if expression
		((eqv? (car expr) 'if)
			; return a list of if and a list of three sublists, one for the recursive call
			; of the test, the then, and the else
			(list 'if 
				(un-lexical-address-help (cadr expr) scopLs)
				(un-lexical-address-help (caddr expr) scopLs)
				(un-lexical-address-help (cadddr expr) scopLs)))
		; if the expr is a set expression
		((eqv? (cadr expr) 'set!)
			; make a list of set!, the first "arg", and the recursive call one what it's set to
			(list 'set! (cadr expr) (un-lexical-address-help (caddr expr) scopLs)))
		; if there is a list of vars, lexical-address all of them
		(else (map (lambda (expr1) (un-lexical-address-help expr1 scopLs)) expr))))
	
(define get-by-ref
	(lambda (depthPos scopLs)
		(list-ref (list-ref scopLs (car depthPos)) (cadr depthPos))))
		
(define un-let-defs
	(lambda (ls scopLs)
		; call unlexical on each definition, but on none of the things defined
		(map (lambda (defPair) (list (car defPair)
			(un-lexical-address-help (cadr defPair) scopLs))) ls)))
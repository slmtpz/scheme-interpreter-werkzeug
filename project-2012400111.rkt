#lang racket
; compiling: yes
; complete: yes
; 2012400111

; (get-binding state var) -> number?
; state : list?
; var : atom?
;;
;Gets the value of an atom from the state of the program.
;;
;Example:
; > (get-binding '((x . 5)(y . 3)) 'y)
; => 3
(define (get-binding state var)
  (cond ((null? state) '())
        ((equal? (car (car state)) var) (cdr (car state)))
        (else (get-binding (cdr state) var))))

; (set-binding state var val) -> list?
; state : list?
; var : atom?
; val : atom?
;;
;Sets the value of an atom on the state of the program.
;;
;Example:
; > (set-binding '((x . 5)(y . 3)) 'y 5)
; => '((y . 5) (x . 5))
(define (set-binding state var val)
  (cons (cons var val) (filter (lambda (p) (not(equal? (car p) var))) state)))

(define empty-state null)

; (eval-expr syntax-tree state) -> list?
; syntax-tree : list?
; state : list?
;;
;Evaluates a single expression.
;;
;Example:
; > (eval-expr '(set! (var x) (var y)) '((y 5)))
; => '(5 ((x . 5) (y . 5)))
(define (eval-expr syntax-tree state)
  (cond ((null? syntax-tree) '())
        ((equal? (car syntax-tree) 'apply) (eval-apply (car (cdr syntax-tree)) (list (car (cdr (cdr syntax-tree))) (car (cdr (cdr (cdr syntax-tree))))) state))
        ((equal? (car syntax-tree) 'set!) (eval-set! (car (cdr (car (cdr syntax-tree)))) (car (cdr (cdr syntax-tree))) state))
        ((equal? (car syntax-tree) 'if) (eval-if (car (cdr syntax-tree)) (car (cdr (cdr syntax-tree))) (car (cdr (cdr (cdr syntax-tree)))) state))
        ((equal? (car syntax-tree) 'while) (eval-while (car (cdr syntax-tree)) (car (cdr (cdr syntax-tree))) state))
        (else (cons (iterate (list syntax-tree) state) (list state)))))

; (eval-apply f args state) -> list?
; f : atom?
; args : list?
; state : list?
;;
;Applies the function determined by f to args.
;;
;Example:
; > (eval-apply '+ '((var x) (value 3)) '((x . 7)))
; => '(10 ((x . 7)))
(define (eval-apply f args state)
  (cons (eval (cons f (iterate args state))) (list state)))

; (iterate args state) -> list?
; args : list?
; state : list?
;;
;Iterates the list 'args' and for every pair in the list,
;gets the desired output which makes the calculations easier later
;and adds to end of the list.
;;
;Example:
; > (iterate '((var x) (value 10)) '((x . 5)))
; => '(5 10)
; > (iterate '((apply + (var x) (value 10))) '((x . 5)))
; => '(15)
(define (iterate args state)
  (cond ((null? (car args)) null)
        ((number? (car args)) args)
        ((not (list? (car args))) (car args))
        ((equal? (car (car args)) 'var) (iterate (reverse (cons (cdr (car (filter (lambda (lis) (equal? (car lis) (car (cdr (car args))))) state))) (reverse (cdr args)))) state))
        ((equal? (car (car args)) 'value) (iterate (reverse (cons (car (cdr (car args))) (reverse (cdr args)))) state))
        ((equal? (car (car args)) 'apply) (iterate (reverse (cons (car (eval-apply (car (cdr (car args))) (cdr (cdr (car args))) state)) (reverse (cdr args)))) state))
        (else '())))

; (eval-if test then-clause else-clause state) -> list?
; test : list?
; then-clause : list?
; else-clause : list?
; state : list?
;;
;Evaluates the if-statement which means if test evaluates true
;evaluate then-clause, otherwise evaluate else-clause.
;;
;Example:
; > (eval-if '(apply > (var x) (value 3)) (value bigger) (value smaller) '((x . 5)))
; => '(bigger ((x . 5)))
(define (eval-if test then-clause else-clause state)
  ((lambda (t) 
     (cond (t (cons (car (eval-exprs (list then-clause) state)) (list state)))
           (else (cons (iterate (list else-clause) state) (list state))))
   ) (iterate (list test) state)))

; (eval-while test body state) -> list?
; test : list?
; body : list?
; state : list?
;;
;Evaluates the while-statement which means if test evaluates true
;evaluate body, otherwise quit.
;;
;Example:
; > (eval-while '(apply > (var n) (value 1)) '((set! (var fact) (apply * (var fact) (var n))) (set! (var n) (apply - (var n) (value 1)))) '((n . 5) (fact . 1)))
; => '(() ((n . 1) (fact . 120)))
(define (eval-while test body state)
  (cond ((iterate (list test) state) ((lambda (e) (eval-while test body (car (cdr e)))) (eval-exprs body state)))
        (else (cons '() (list state)))))

; (eval-set! var expr state) -> list?
; var : atom?
; expr : list?
; state : list?
;;
;Sets the value of the variable 'var' to the value derived from 'expr'.
;;
;Example:
; > (eval-set! 'x '(var y) '((y . 5)))
; => '(5 ((x . 5) (y . 5)))
(define (eval-set! var expr state)
  (cons (car (iterate (list expr) state)) (list (set-binding state var (car (iterate (list expr) state))))))

; (eval-exprs exprs state) -> list?
; exprs : list?
; state : list?
;;
;Evaluates expressions given as a list in 'exprs' one by one.
;;
;Example:
; > (eval-exprs '((set! (var fact) (value 1)) (set! (var n) (value 5)) (while (apply > (var n) (value 1)) ((set! (var fact) (apply * (var fact) (var n))) (set! (var n) (apply - (var n) (value 1)))))) '((x . 8) (n . nonsense)))
; => '(() ((n . 1) (fact . 120) (x . 8)))
(define (eval-exprs exprs state)
  ((lambda (e)
     (cond ((null? (cdr exprs)) e)
           (else (eval-exprs (cdr exprs) (car (cdr e))))))
   (eval-expr (car exprs) state)))
#lang racket/base

(require compatibility/mlist)
(require mzlib/compat)
(require rackunit)
(require profile)

(provide chapter1-scheme)

(define (mcaar l)
  (mcar (mcar l)))

(define (mcdar l)
  (mcdr (mcar l)))

; 1.3 Evaluating Atoms
; defined in mzlib/compat
; (define (atom? x)
;    (not (pair? x)))

(define the-false-value (cons "false" "boolean"))

; Exercise 1.1
; add function tracing
(define (trace args fn)
  (begin
    (displayln args)
    (let ([r (fn args)])
      (display "-> ")
      (displayln r) r)))

; 1.4 Evaluating Forms
(define (evaluate e env)
  (if (atom? e)
    (cond [(symbol? e) (lookup e env)]
          [(or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e]
          [else (error "Cannot evaluate" e)])
    (case (car e)
      [(quote)  (cadr e)]
      [(if)     (if (evaluate (cadr e) env)
                  (evaluate (caddr e) env)
                  (evaluate (cadddr e) env))]
      [(begin)  (eprogn (cdr e) env)]
      [(set!)   (update! (cadr e) env (evaluate (caddr e) env))]
      [(lambda) (make-function (cadr e) (cddr e) env)]
      [else     (trace e
                       (lambda (e)
                         (invoke (evaluate (car e) env)
                                 (evlis (cdr e) env))))])))

(define (eprogn exps env)
  (if (pair? exps)
    (if (pair? (cdr exps))
        (begin (evaluate (car exps) env)
               (eprogn (cdr exps) env))
        (evaluate (car exps) env))
    '()))

; (define (evlis exps env)
;   (if (pair? exps)
;     (cons (evaluate (car exps) env)
;           (evlis (cdr exps) env))
;     '()))

; Ex 1.2 evlis
(define (evlis exps env)
  (begin
    (if (pair? exps)
      (if (pair? (cdr exps))
        (cons (evaluate (car exps) env)
              (evlis (cdr exps) env))
        (list (evaluate (car exps) env)))
    '())))

; 1.5 Representing the Environment
(define (lookup id env)
  (if (mpair? env)
    (if (eq? (mcaar env) id)
        (mcdar env)
        (lookup id (mcdr env)))
    (error "No such binding" id)))

(define (update! id env value)
  (if (mpair? env)
    (if (eq? (mcaar env) id)
        (begin (set-mcdr! (mcar env) value)
               value)
        (update! id (mcdr env) value))
    (error "No such binding" id)))

(define env.init (mcons '() '()))

(define (extend env variables values)
  (cond [(pair? variables)
         (if (pair? values)
             (cons (cons (car variables) (car values))
                   (extend env (cdr variables) (cdr values)))
             (error "Too few values"))]
        [(null? variables)
         (if (null? values)
            env
            (error "Too many values"))]
        [(symbol? variables) (mcons (cons variables values) env)]))

; Exercise 1.3 extend
; (define (extend env names values)
;   (cons (cons names values) env))

; 1.6 Representing Functions
(define (invoke fn args)
  (if (procedure? fn)
      (fn args)
      (error "Not a function" fn)))

(define (make-function variables body env)
  (lambda (values)
    (eprogn body (extend env variables values))))


; 1.7 Global Environment
(define env.global env.init)

(define-syntax definitial
  (syntax-rules ()
    ((definitial name)
     (begin (set! env.global (mcons (mcons 'name 'void) env.global))
            'name))
     ((definitial name value)
      (begin (set! env.global (mcons (mcons 'name value) env.global))
             'name))))

; Exercise 1.8 apply
(define (my.apply fn lst)
  (cond [(null? lst) (fn)]
        [(null? (cdr lst)) (fn (car lst))]
        [(null? (cddr lst)) (fn (car lst) (cadr lst))]
        [else
            (fn (car lst) (my.apply fn (cdr lst)))]))

(check-equal? (my.apply + '()) 0)
(check-equal? (my.apply + '(1)) 1)
(check-equal? (my.apply + '(1 2)) 3)
(check-equal? (my.apply + '(1 2 3)) 6)

(define-syntax defprimitive
  (syntax-rules ()
    ; Exercise 1.6 add non-arity defprimitive
    ((defprimitive name value)
     (definitial name
        (lambda (values)
          (my.apply value values))))
    ((defprimitive name value arity)
     (definitial name
        (lambda (values)
          (if (= arity (length values))
              (my.apply value values)
              (error "Incorrect arity"
                     (list 'name values))))))))

(definitial t #t)
(definitial f the-false-value)
(definitial nil '())

(definitial foo)
(definitial bar)
(definitial fib)
(definitial fact)

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive set-mcdr! set-mcdr! 2)
(defprimitive + + 2)
(defprimitive eq? eq? 2)
; (defprimitive < < 2)

(check-equal? (evaluate 't env.global) #t)
(check-equal? (evaluate 'f env.global) the-false-value)
(check-equal? (evaluate '(car '(1 2)) env.global) 1)
(check-equal? (evaluate '(cons 1 '()) env.global) '(1))
(check-equal? (evaluate '(+ 1 1) env.global) 2)
(check-equal? (evaluate '(eq? 1 1) env.global) #t)
(check-equal? (evaluate '(if #t 1 2) env.global) 1)

; Exercise 1.5
; Not sure if this is the right solution, but adding a primitive of a lambda that compares and returns #t or the-false-value
(defprimitive < (lambda (n m) (if (< n m) #t the-false-value)) 2)
(check-equal? (evaluate '(< 1 2) env.global) #t)

; Exercise 1.6 list
(defprimitive list list)
(check-equal? (evaluate '(list 1 1) env.global) '(1 1))
(check-equal? (evaluate '(list 1) env.global) '(1))

; Exercise 1.7
; call/cc

; Exercise 1.9
(defprimitive end exit)

(define (chapter1-scheme)
  (define (toplevel)
    (displayln (evaluate (read) env.global))
    (toplevel))
  (begin
    (displayln "Welcome to LiSP. Type (end) to exit.")
    (toplevel)))

(check-equal? (evaluate '(< 2 1) env.global) the-false-value)

; Shallow functions with a stack
(define (s.make-function variables body env)
  (lambda (values current.env)
    (let ([old-bindings
            (map (lambda (var val)
                   (let ([old-value (getprop var 'apval)])
                     (putprop var 'aplval val)
                     (cons var old-value)))
                 variables
                 values)])
      (let ([result (eprogn body current.env)])
        (for-each (lambda (b) (putprop (car b) 'apval (cdr b)))
                  old-bindings)
        result))))

(define (s.lookup id env)
  (getprop id 'apval))

(define (s.update! id env value)
  (putprop id 'apval value))

(define (s.evaluate e env)
  (if (atom? e)
    (cond [(symbol? e) (s.lookup e env)]
          [(or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e]
          [else (error "Cannot evaluate" e)])
    (case (car e)
      [(quote)  (cadr e)]
      [(if)     (if (s.evaluate (cadr e) env)
                  (s.evaluate (caddr e) env)
                  (s.evaluate (cadddr e) env))]
      [(begin)  (eprogn (cdr e) env)]
      [(set!)   (update! (cadr e) env (s.evaluate (caddr e) env))]
      [(lambda) (s.make-function (cadr e) (cddr e) env)]
      [else     (trace e
                       (lambda (e)
                         (invoke (s.evaluate (car e) env)
                                 (s.evlis (cdr e) env))))])))

(define (s.evlis exps env)
  (begin
    (if (pair? exps)
      (if (pair? (cdr exps))
        (cons (s.evaluate (car exps) env)
              (s.evlis (cdr exps) env))
        (list (s.evaluate (car exps) env)))
    '())))

(s.update! 't env.global #t)
(s.update! 'f env.global the-false-value)

(check-equal? (s.lookup 't env.global) #t)
(check-equal? (s.lookup 'f env.global) the-false-value)

; Exercise 1.10
; Not sure how to performance benchmark racket yet

; Exercise 1.11
; ????

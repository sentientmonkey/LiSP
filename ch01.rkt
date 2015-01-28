#lang racket/base

(require compatibility/mlist)
(require rackunit)

(provide chapter1-scheme)

(define (mcaar l)
  (mcar (mcar l)))

(define (mcdar l)
  (mcdr (mcar l)))

; 1.3 Evaluating Atoms
(define (atom? x)
    (not (pair? x)))

(define the-false-value (cons "false" "boolean"))

; Exercise 1.1
; add function tracing
(define (evaluate e env)
  (begin
    (let ([r (ievaluate e env)])
      (display e)
      (display " ≡ ")
      (displayln r) r)))

; 1.4 Evaluating Forms

(define (ievaluate e env)
  (if (atom? e)
    (cond [(symbol? e) (lookup e env)]
          [[(or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e]]
          [else (error "Cannot evaluate" e)])
    (case (car e)
      [(quote)  (cadr e)]
      [(if)     (if (evaluate (cadr e) env)
                  (evaluate (caddr e) env)
                  (evaluate (cadddr e) env))]
      [(begin)  (eprogn (cdr e) env)]
      [(set!)   (update! (cadr e) env (evaluate (caddr e) env))]
      [(lambda) (make-function (cadr e) (cddr e) env)]
      [else     (invoke (evaluate (car e) env)
                        (evlis (cdr e) env))])))

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

(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name
        (lambda (values)
          (if (= arity (length values))
              (apply value values)
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
(defprimitive < < 2)

(define (chapter1-scheme)
  (define (toplevel)
    (display (evaluate (read) env.global))
    (toplevel))
  (toplevel))

(check-equal? (evaluate 't env.global) #t)
(check-equal? (evaluate 'f env.global) the-false-value)
(check-equal? (evaluate '(car '(1 2)) env.global) 1)
(check-equal? (evaluate '(cons '1 '()) env.global) '(1))
(check-equal? (evaluate '(cons '1 '()) env.global) '(1))
; (check-equal? (evaluate '(+ 1 1) env.global) 2)

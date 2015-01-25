#lang racket/base

(require scheme/mpair)
(require rackunit)

; 1.3 Evaluating Atoms
(define (atom? x)
    (not (pair? x)))

; 1.4 Evaluating Forms
(define (evaluate e env)
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

(define (evlis exps env)
  (if (pair? exps)
    (cons (evaluate (car exps) env)
          (evlis (cdr exps) env))
    '()))

; 1.5 Representing the Environment
(define (lookup id env)
  (if (pair? env)
    (if (eq? (caar env) id)
        (cadr env)
        (lookup id (cdr env)))
    (error "No such binding" id)))

(define (update! id env value)
  (if (pair? env)
    (if (eq? (caar env) id)
        (begin (set-cdr! (car env) value)
               value)
        (update! id (cdr env) value))
    (error "No such binding" id)))

(define env.init '())

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
        [(symbol? variables) (cons (cons variables values) env)]))

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
     (begin (set! env.global (cons (cons 'name 'void) env.global))
            'name))
     ((definitial name value)
      (begin (set! env.global (cons (cons 'name value) env.global))
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
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive eq? eq? 2)
(defprimitive < < 2)

(define (chapter1-scheme)
  (define (toplevel)
    (display (evalute (read) env.global))
    (toplevel))
  (toplevel))

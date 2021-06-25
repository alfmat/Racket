#lang racket

(require "a3.rkt")
(module+ test
  (require (only-in rackunit
                    check-equal?
                    )))

(define (new-environment) (hash))

(define (lookup-name env name)
  (hash-ref env name))

(define (add-binding env name val)
  (hash-set env name val))

(define (eval code)
  (eval-program (parse code) (new-environment)))

(define (eval-program parsed-code env)
  ;; program := exprList
  (let*
      ([expr-list (second parsed-code)]
       [evaluated-expr-list (eval-expr-list expr-list env)]
       [expr-to-eval (last evaluated-expr-list)])
    expr-to-eval
   )
 )

(define (eval-name name env)
  ;; returns the function based off of symbol
  (case name
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(string-append) string-append]
    [(string<?) string<?]
    [(string=?) string=?]
    [(not) not]
    [(=) =]
    [(<) <]
    [else (lookup-name env name)] 
    )
)

(define (eval-expr-list expr-list env)
  ;; exprList := expr optExprList
  (let*
      ([expr (second expr-list)]
       [opt-expr-list (third expr-list)]
       [expr-subtree (second expr)]
       [expr-subtree-type (first expr-subtree)])
    (case expr-subtree-type
      [(atom invocation let lambda) (cons (eval-expr expr env) (eval-opt-expr-list opt-expr-list env))]
      [(define) (let*
                    ([define-result (eval-define expr-subtree env)]
                     [new-env (cdr define-result)]
                     [define-value (car define-result)]
                     [opt-expr-list-evaled (eval-opt-expr-list opt-expr-list new-env)])
                  (cons define-value opt-expr-list-evaled))])   
    )
)

(define (eval-expr expr env)
  ;; expr := atom | invocation | let | define | lambda
  (let*
      ([expr-body (second expr)]
       [expr-type (first expr-body)])
    (if (eq? 'atom expr-type)
        (eval-atom expr-body env)
        (if (eq? 'invocation expr-type)
            (eval-invocation expr-body env)
            (if (eq? 'let expr-type)
                (eval-let expr-body env)
                (if (eq? 'define expr-type)
                    (eval-define expr-body env)
                    (eval-lambda expr-body env)
                    )
                )
            )
        )
    )
  )

(define (eval-atom atom env)
  ;; atom := NAME | STRING | number
  (let*
      ([atom-body (second atom)]
       [atom-type (first atom-body)])
    (if (eq? 'number atom-type)
        (eval-number atom-body env)
        (if (eq? 'NAME atom-type)
            (eval-name (second atom-body) env)
            (second atom-body)
        )
    )
  )
  )

(define (eval-invocation invocation env)
  ;; invocation := OPAREN exprList CPAREN
  (let*
      ([expr-list (third invocation)]
       [expr-list-resolved (eval-expr-list expr-list env)])
        (apply (first expr-list-resolved) (rest expr-list-resolved))
        )
    
    )

(define (eval-let let-expr env)
  ;; let := LET OPAREN NAME expr CPAREN expr
  (let*
      ([name-token (fourth let-expr)]
       [name (second name-token)]
       [name-val-expr (fifth let-expr)]
       [expr-to-eval (seventh let-expr)])
    (eval-expr expr-to-eval
               (add-binding env name (eval-expr name-val-expr env))))
 )

(define (eval-define define-expr env)
  ;; define := DEFINE NAME expr
  (let* ([name-token (third define-expr)]
         [name (second name-token)]
         [expr-to-eval (fourth define-expr)]
         [val-expr (eval-expr expr-to-eval env)])
    (cons val-expr (add-binding env name val-expr))
    )
 )

(define (eval-lambda lambda-expr env)
  ;; lambda := LAMBDA OPAREN NAME CPAREN expr
    (let*
        ;; (NAME x)
        ([expr-to-eval (sixth lambda-expr)]
         [name (second (fourth lambda-expr))])
      
  (lambda (argument) (eval-expr expr-to-eval (add-binding env name argument)))
 ))

(define (eval-number number env)
  ;; returns actual number and not token
  (let*
      ([number-body (second number)]
       [number-type (first number-body)]
       [actual-number (second number-body)])
    actual-number)
  )


(define (eval-opt-expr-list opt-expr-list env)
  ;; optExprList := ɛ | exprList
  (if (> (length opt-expr-list) 1)
      (let*
          ([expr-list (second opt-expr-list)])
        (eval-expr-list expr-list env))
      null
      )
)


(module+ test
  
  ;; a6 tests
  (check-equal? (eval "define square lambda (x) (* x x) (square 7)") 49)
  (check-equal? (eval "(lambda (x) (* x x) 7)") 49)
  (check-equal? (eval "define foo (/ 8 2) let (x (+ 1 2)) (+ x foo)") 7)
  (check-equal? (eval "let (x (+ 1 2)) (+ x 3)") 6)
  (check-equal? (eval "define foo 5 foo") 5)
  (check-equal? (eval "define foo 2") 2)
  (check-equal? (eval "let (y 2) let (prody lambda (x) (* y x)) (prody 3)") 6)
  
  ;; a5 tests
  (check-equal? (eval "5") 5)
  (check-equal? (eval "(* 7 8)") 56)
  (check-equal? (eval "(* 7 (- 8))") -56)
  (check-equal? (eval "(* (+ 1 2) (/ 8 4))") 6)
  (check-equal? (eval "(string<? \"abc\" (string-append \"a\" \"b\" \"b\"))") #f)
  (check-equal? (eval "(not (string<? \"abc\" (string-append \"a\" \"b\" \"b\")))") #t)
  (check-equal? (eval "(string=? \"abc\" (string-append \"a\" \"b\" \"c\"))") #t)
  
  )

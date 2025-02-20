#lang racket

(require "a3.rkt")
(module+ test
  (require (only-in rackunit
                    check-equal?
                    )))


(define (eval code)
  (eval-program (parse code)))

(define (eval-program parsed-code)
  ;; program := exprList
  (let*
      ([expr-list (second parsed-code)]
       [evaluated-expr-list (eval-expr-list expr-list)]
       [expr-to-eval (last evaluated-expr-list)])
    expr-to-eval
   )
 )

(define (eval-operator op-symbol)
  ;; returns the function based off of symbol
  (case op-symbol
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
    [else (error "wrong name provided")] 
    )
)

(define (eval-expr-list expr-list)
  ;; exprList := expr optExprList
  (let*
      ([expr (second expr-list)]
       [opt-expr-list (third expr-list)]
       [evaluated-opt-expr-list (eval-opt-expr-list opt-expr-list)]
       [evaluated-expr (eval-expr expr)])
    (cons evaluated-expr evaluated-opt-expr-list)   
    )
)

(define (eval-expr expr)
  ;; expr := atom | invocation
  (let*
      ([expr-body (second expr)]
       [expr-type (first expr-body)])
    (if (eq? 'atom expr-type)
        (eval-atom expr-body)
        (eval-invocation expr-body)
        ))
  )

(define (eval-atom atom)
  ;; atom := NAME | STRING | number
  (let*
      ([atom-body (second atom)]
       [atom-type (first atom-body)])
    (if (eq? 'number atom-type)
        (eval-number atom-body)
        (if (eq? 'NAME atom-type)
            (eval-operator (second atom-body))
            (second atom-body)
        )
    )
  )
  )

(define (eval-invocation invocation)
  ;; invocation := OPAREN exprList CPAREN
  (let*
      ([expr-list (third invocation)]
       [expr-list-resolved (eval-expr-list expr-list)])
    (apply (first expr-list-resolved) (rest expr-list-resolved))
    )
  )

(define (eval-number number)
  ;; returns actual number and not token
  (let*
      ([number-body (second number)]
       [number-type (first number-body)]
       [actual-number (second number-body)])
    actual-number)
  )


(define (eval-opt-expr-list opt-expr-list)
  ;; optExprList := ɛ | exprList
  (if (> (length opt-expr-list) 1)
      (let*
          ([expr-list (second opt-expr-list)])
        (eval-expr-list expr-list))
      null
      )
)


(module+ test
  (check-equal? (eval "5") 5))

(module+ test
  (check-equal? (eval "(* 7 8)") 56))

(module+ test
  (check-equal? (eval "(* 7 (- 8))") -56))

(module+ test
  (check-equal? (eval "(* (+ 1 2) (/ 8 4))") 6))

(module+ test
  (check-equal? (eval "(string<? \"abc\" (string-append \"a\" \"b\" \"b\"))") #f))

(module+ test
  (check-equal? (eval "(not (string<? \"abc\" (string-append \"a\" \"b\" \"b\")))") #t))

(module+ test
  (check-equal? (eval "(string=? \"abc\" (string-append \"a\" \"b\" \"c\"))") #t))

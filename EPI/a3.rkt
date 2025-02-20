#lang racket

(require (only-in (file "lex.rkt")
                  lex))

;; Grammar:
;;
;; program ::= exprList
;; exprList ::= expr optExprList
;; optExprList ::= ɛ | exprList
;; expr ::= atom | invocation
;; atom ::= NAME | STRING | number
;; number ::= INT | FLOAT
;; invocation ::= OPAREN exprList CPAREN

(module+ test
  (require (only-in rackunit
                    check-equal?
                    check-exn
                    check-not-exn)))

(define tokens (make-parameter null))

(define DEBUG #f)
(define (trace label)
  (when DEBUG
    (writeln (~a label " "
                 (if (empty? (tokens))
                     'null
                     (first (tokens)))))))

(define (check type)
  (if (empty? (tokens))
      #f
      (equal? type (first (first (tokens))))))

(module+ test
  (check-equal? (parse "define foo (/ 8 2)
let (x (+ 1 2))
  (+ x foo)") 1)
  (check-equal? (parameterize ([tokens null])
                  (check 'DEF))
                #f)
  (check-equal? (parameterize ([tokens (list (list 'DEF #f))])
                  (check 'DEF))
                #t)
  (check-equal? (parameterize ([tokens (list (list 'DEF #f))])
                  (check 'FUN))
                #f))

(define (consume type)
  (when (empty? (tokens))
    (error (~a "expected token of type " type " but no remaining tokens")))
  (let ([token (first (tokens))])
    (when (not (equal? type (first token)))
      (error (~a "expected token of type " type " but actual token was " token)))
    (tokens (rest (tokens)))
    (when DEBUG
      (displayln (~a "(consume '" token ")")))
    token))

(module+ test
  (check-exn exn:fail?
             (lambda ()
               (parameterize ([tokens null])
                 (consume 'DEF))))
  (check-not-exn (lambda ()
                   (parameterize ([tokens (list (list 'DEF #f))])
                     (consume 'DEF))))
  (check-exn exn:fail?
             (lambda ()
               (parameterize ([tokens (list (list 'DEF #f))])
                 (consume 'FUN)))))

(define (parse-number)
  ;; number ::= INT | FLOAT
  (list 'number
        (if (check 'INT)
            (consume 'INT)
            (consume 'FLOAT))))

(define (number-pending?)
  (or (check 'INT)
      (check 'FLOAT)))

(module+ test
  (check-equal? (parameterize ([tokens (list (list 'INT 7))])
                  (parse-number))
                (list 'number (list 'INT 7)))
  (check-equal? (parameterize ([tokens (list (list 'FLOAT 7.7))])
                  (parse-number))
                (list 'number (list 'FLOAT 7.7)))
  (check-exn exn:fail?
             (lambda ()
               (parameterize ([tokens (list (list 'DEF #f))])
                 (parse-number)))))

(define (parse-atom)
  ;; atom ::= NAME | STRING | number
  (list 'atom
        (cond
          [(check 'NAME) (consume 'NAME)]
          [(check 'STRING) (consume 'STRING)]
          [else (parse-number)])))

(module+ test
  (check-equal? (parameterize ([tokens (list (list 'NAME 'foo))])
                  (parse-atom))
                (list 'atom (list 'NAME 'foo)))
  (check-equal? (parameterize ([tokens (list (list 'STRING "foo"))])
                  (parse-atom))
                (list 'atom (list 'STRING "foo")))
  (check-equal? (parameterize ([tokens (list (list 'INT 7))])
                  (parse-atom))
                (list 'atom (list 'number (list 'INT 7))))
  (check-equal? (parameterize ([tokens (list (list 'FLOAT 7.7))])
                  (parse-atom))
                (list 'atom (list 'number (list 'FLOAT 7.7))))
  (check-exn exn:fail?
             (lambda ()
               (parameterize ([tokens (list (list 'DEF #f))])
                 (parse-atom)))))

(define (parse-invocation)
  ;; invocation ::= OPAREN exprList CPAREN
  (list 'invocation
        (consume 'OPAREN)
        (parse-expr-list)
        (consume 'CPAREN)))


(define (parse-expr)
  ;; expr ::= atom | invocation | let | define | lambda
  (list 'expr
        (if (atom-pending?)
            (parse-atom)
            (if (invocation-pending?)
                (parse-invocation)
                (if (let-pending?)
                    (parse-let)
                    (if (define-pending?)
                        (parse-define)
                        (parse-lambda)
                        )
                    )
                )
            )
        )
  )

(define (expr-pending?)
  (or (atom-pending?)
      (invocation-pending?)
      (let-pending?)
      (define-pending?)
      (lambda-pending?)))

(define (atom-pending?)
  (or (check 'NAME)
      (check 'STRING)
      (number-pending?)))

(define (invocation-pending?)
  (check 'OPAREN))

(define (let-pending?)
  (check 'LET))

(define (define-pending?)
  (check 'DEFINE))

(define (lambda-pending?)
  (check 'LAMBDA))

(define (parse-let)
  (list 'let
        (consume 'LET)
        (consume 'OPAREN)
        (consume 'NAME)
        (parse-expr)
        (consume 'CPAREN)
        (parse-expr)
        )
  )

(define (parse-define)
  (list 'define
        (consume 'DEFINE)
        (consume 'NAME)
        (parse-expr)
        )
  )

(define (parse-lambda)
  (list 'lambda
        (consume 'LAMBDA)
        (consume 'OPAREN)
        (consume 'NAME)
        (consume 'CPAREN)
        (parse-expr)
        )
  )

(define (parse-expr-list)
  ;; exprList ::= expr optExprList
  (list 'exprList
        (parse-expr)
        (parse-opt-expr-list)))

(define (expr-list-pending?)
  (expr-pending?))

(define (parse-opt-expr-list)
  ;; optExprList ::= ɛ | exprList
  (if (expr-list-pending?)
      (list 'optExprList (parse-expr-list))
      (list 'optExprList)))

(define (parse-program)
  ;; program ::= exprList
  (list 'program
        (parse-expr-list)))

(define (parse code)
  (parameterize ([tokens (lex code)])
    (parse-program)))

(provide parse)

(module+ test ;; smaller integration tests
  (check-equal? (parse "7")
                '(program
                  (exprList
                   (expr
                    (atom
                     (number
                      (INT 7))))
                   (optExprList))))

  (check-equal? (parse "7.7")
                '(program
                  (exprList
                   (expr
                    (atom
                     (number
                      (FLOAT 7.7))))
                   (optExprList))))

  (check-equal? (parse "\"a string\"")
                '(program
                  (exprList
                   (expr
                    (atom
                     (STRING "a string")))
                   (optExprList))))

  (check-equal? (parse "foo")
                '(program
                  (exprList
                   (expr
                    (atom
                     (NAME foo)))
                   (optExprList))))

  ;; invocation
  (check-equal? (parse "(list)")
                '(program
                  (exprList
                   (expr
                    (invocation
                     (OPAREN #f)
                     (exprList
                      (expr
                       (atom
                        (NAME list)))
                      (optExprList))
                     (CPAREN #f)))
                   (optExprList))))

  ;; concatenation
  (check-equal? (parse "7 8")
                '(program
                  (exprList
                   (expr
                    (atom
                     (number
                      (INT 7))))
                   (optExprList
                    (exprList
                     (expr
                      (atom
                       (number
                        (INT 8))))
                     (optExprList))))))
  )
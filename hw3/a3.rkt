#lang racket

(require (only-in (file "lex.rkt") lex))

;; create tokens as global variable
(define tokens (make-parameter null))

(define (parse code)
  (parameterize ([tokens (lex code)])
    (parse-program)))

(define (consume type)
  (when (empty? (tokens))
    (error (~a "expected token of type " type " but no remaining tokens")))
  (let ([token (first (tokens))])
    (when (not (equal? type (first token)))
      (error (~a "expected token of type " type " but actual token was " token)))
    (tokens (rest (tokens)))  ; update tokens: remove first token
    token))

(define (check type)
  (if (empty? (tokens))
      #f
      (equal? type (first (first (tokens))))))

(define (parse-program)
  (list 'program
        (parse-expr-list)))

(define (parse-expr-list)
  (list 'exprList
        (parse-expr)
        (parse-opt-expr-list)))

(define (parse-opt-expr-list)
        (if (or (check-atom) (check-invocation))
            (list 'optExprList (parse-expr-list))
                (list 'optExprList)
                )
            )

(define (check-atom)
  (if(or (check 'INT) (check 'FLOAT) (check 'NAME) (check 'STRING))
       #t
       #f
   )
  )

(define (check-invocation)
  (if (check 'OPAREN)
       #t
       #f
   )
  )

(define (parse-expr)
  (list 'expr
        (if (check-invocation)
            (parse-invocation)
            (parse-atom)
            )   
        ))

(define (parse-atom)
  (list 'atom
        (if (check 'NAME)
            (consume 'NAME)
            (if (check 'STRING)
                (consume 'STRING)
                        (parse-number)
                        )
                    )
                )
            )
        

(define (parse-number)
  (list 'number
        (if (check 'INT)
            (consume 'INT)
            (consume 'FLOAT)
            )
        )
  )

(define (parse-invocation)
  (list 'invocation
        (consume 'OPAREN)
        (parse-expr-list)
        (consume 'CPAREN)
        )
  )

;; local testing
(module+ test
  (require (only-in rackunit
                    check-equal?))
  (check-equal? (lex "") null)
  (check-equal? (parse
                 "(define factorial
  (fun (n)
    (if (< n 0.9)
        1  ;; base case
        (factorial (- n 1) ;* recursive case *; ))))

(print (+ \"5! is \" (factorial 5)))")
'(program
  (exprList
   (expr
    (invocation
     (OPAREN #f)
     (exprList
      (expr
       (atom
        (NAME define)))
      (optExprList
       (exprList
        (expr
         (atom
          (NAME factorial)))
        (optExprList
         (exprList
          (expr
           (invocation
            (OPAREN #f)
            (exprList
             (expr
              (atom
               (NAME fun)))
             (optExprList
              (exprList
               (expr
                (invocation
                 (OPAREN #f)
                 (exprList
                  (expr
                   (atom
                    (NAME n)))
                  (optExprList))
                 (CPAREN #f)))
               (optExprList
                (exprList
                 (expr
                  (invocation
                   (OPAREN #f)
                   (exprList
                    (expr ; if
                     (atom
                      (NAME if)))
                    (optExprList
                     (exprList
                      (expr ; (< n 0.9)
                       (invocation
                        (OPAREN #f)
                        (exprList
                         (expr (atom (NAME <)))
                         (optExprList
                          (exprList
                           (expr (atom (NAME n)))
                           (optExprList
                            (exprList
                             (expr (atom (number (FLOAT 0.9))))
                             (optExprList))))))
                        (CPAREN #f)))
                      (optExprList
                       (exprList
                        (expr (atom (number (INT 1))))
                        (optExprList
                         (exprList
                          (expr
                           (invocation
                            (OPAREN #f)
                            (exprList
                             (expr (atom (NAME factorial)))
                             (optExprList
                              (exprList
                               (expr
                                (invocation
                                 (OPAREN #f)
                                 (exprList
                                  (expr (atom (NAME -)))
                                  (optExprList
                                   (exprList
                                    (expr (atom (NAME n)))
                                    (optExprList
                                     (exprList
                                      (expr (atom (number (INT 1))))
                                      (optExprList))))))
                                 (CPAREN #f)))
                               (optExprList))))
                            (CPAREN #f)))
                          (optExprList))))))))
                   (CPAREN #f)))
                 (optExprList))))))
            (CPAREN #f)))
          (optExprList))))))
     (CPAREN #f)))
   (optExprList
    (exprList
     (expr
      (invocation
       (OPAREN #f)
       (exprList
        (expr
         (atom (NAME print)))
        (optExprList
         (exprList
          (expr
           (invocation
            (OPAREN #f)
            (exprList
             (expr
              (atom
               (NAME +)))
             (optExprList
              (exprList
               (expr
                (atom
                 (STRING "5! is ")))
               (optExprList
                (exprList
                 (expr
                  (invocation
                   (OPAREN #f)
                   (exprList
                    (expr
                     (atom
                      (NAME factorial)))
                    (optExprList
                     (exprList
                      (expr
                       (atom
                        (number
                         (INT 5))))
                      (optExprList))))
                   (CPAREN #f)))
                 (optExprList))))))
            (CPAREN #f)))
          (optExprList))))
       (CPAREN #f)))
     (optExprList)))))
                )
  )


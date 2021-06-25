#lang racket

;; starter code for A7, including:
;; 1. a parser that works with the A7 grammar:
(require (only-in (file "parse.rkt") parse))

;; 2. A6 solution code (everything below):

(provide eval repl)

(module+ test (require rackunit))

(define new-environment make-hash)

(define (add-binding env name value)
  (hash-set! env name value)
  env)

(define (update-binding env name value)
  (if (hash-has-key? env name)
      (begin
        (hash-set! env name value)
        env)
      (error (~a "Cannot update nonexistent name " name))))

(define lookup-name hash-ref)

(define (new-environment-with-object%)
  (let* ([new-env (new-environment)]
         [method-table (new-environment)]
         [Object (list
                   #f          ; 1. superclass
                   '()         ; 2. field-names
                   (add-binding method-table 'initialize (list null '(exprList (expr (atom (number (INT 1)))) (optExprList)))); 3. method-table
                   new-env     ; 4. defining-environment
                   )])
    (add-binding new-env 'Object Object)))

(define (find-method-class-or-fail class method-name)
  (or (find-method class method-name)
      (error (~a "Method not found: " method-name))))

(define (find-method class method-name)
  (let ([superclass (first class)]
        [method-table (third class)])
        (if (hash-has-key? method-table method-name)
            class
            (if (not superclass)
                #f
                (find-method superclass method-name)
                )
            )
    ))

(define (add-bindings env names values)
  (if (empty? names)
      env
      (add-bindings (add-binding env (first names) (first values))
                    (rest names) (rest values))))

(define (eval code-string)
  (last (eval-program (parse code-string) (new-environment-with-object%))))

(define (eval-program program-expr env)
  ;; program     := exprList
  (eval-exprList (second program-expr) env))

(define (eval-exprList exprList-expr env)
  ;; exprList    := expr optExprList
  (let* ([expr-expr (second exprList-expr)]
         [expr-tag (first (second expr-expr))]
         [optExprList-expr (third exprList-expr)])
    (if (equal? expr-tag 'define)
      ;; define      := DEFINE NAME expr
      (let* ([define-expr (second expr-expr)]
             [name (second (third define-expr))]
             [value-expr (fourth define-expr)]
             [new-env (add-binding env name (eval-expr value-expr env))])
        (eval-optExprList (lookup-name new-env name)
                          optExprList-expr
                          new-env))
      ;; normal stuff
      (eval-optExprList (eval-expr expr-expr env)
                        optExprList-expr
                        env))))

(define (eval-optExprList value optExprList-expr env)
  ;; optExprList := ɛ | exprList
  (cons value (if (empty? (rest optExprList-expr))
                null
                (eval-exprList (second optExprList-expr) env))))

(define (eval-expr expr-expr env)
  ;; expr        := atom | invocation | let | define | lambda | class | new | send | super | set
  (let* ([expr-to-eval (second expr-expr)]
         [tag (first expr-to-eval)]
         [evaluator (case tag
                      [(atom) eval-atom]
                      [(invocation) eval-invocation]
                      [(let) eval-let]
                      ;; define case is handled in `eval-exprList`
                      [(lambda) eval-lambda]
                      [(class) eval-class]
                      [(new) eval-new]
                      [(send) eval-send]
                      [(super) eval-super]
                      [(set) eval-set])])
    (evaluator expr-to-eval env)))

(define (eval-class class-expr env)
  ;; class := CLASS NAME OPAREN optNameList CPAREN OBRACE optMethodList CBRACE
  (let*
      ([name-token (third class-expr)]
       [superclass-name (second name-token)]
       [superclass (lookup-name env superclass-name)]
       [opt-name-list (fifth class-expr)]
       [opt-method-list (eighth class-expr)]
       [method-table (make-hash
                      (for/list ([method-expr (eval-opt-method-list opt-method-list env)])
                        (let
                            ([method-name (first method-expr)]
                             [method-params (second method-expr)]
                             [method-body (third method-expr)])
                          (cons method-name (list method-params method-body)))
                        ))])
    (list superclass (eval-opt-name-list opt-name-list env) method-table (hash-copy env)))
  )

(define (eval-new new-expr env)
  ;; new := NEW NAME OPAREN optExprList CPAREN
  (let*
      ([class-name-token (third new-expr)]
       [class-name (second class-name-token)]
       [class (lookup-name env class-name)]
       [opt-expr-list (fifth new-expr)]
       [field-names (collect-field-names class)]
       [class-env (fourth class)]
       [object-env (add-field-bindings (hash-copy class-env) field-names)]
       [object (list class object-env)])
    (call-method object 'initialize opt-expr-list env)
    object))

(define (eval-send send-expr env)
  ;; send := SEND NAME NAME OPAREN optExprList CPAREN
  (let*
      ([object-name-token (third send-expr)]
       [object-name (second object-name-token)]
       [method-name-token (fourth send-expr)]
       [method-name (second method-name-token)]
       [opt-expr-list (sixth send-expr)]
       [object (lookup-name env object-name)])
    (call-method object method-name opt-expr-list env)))

(define (eval-super super-expr env)
  ;; super := SUPER OPAREN optExprList CPAREN
  (let*
      ([object (lookup-name env 'this)]
       [super-method-info (lookup-name env 'super)]
       [method-name (first super-method-info)]
       [relative-class (second super-method-info)]
       [method-class (find-method-class-or-fail relative-class method-name)]
       [opt-expr-list (fourth super-expr)])
    (call-method2 object method-class method-name opt-expr-list env)))

(define (eval-set set-expr env)
  ;; set := SET NAME expr
  (let* ([name-token (third set-expr)]
         [name (second name-token)]
         [value-expr (fourth set-expr)]
         [value (eval-expr value-expr env)])
    (update-binding env name value)
    value))

(define (call-method object method-name rand-exprs rand-env)
  (let* ([class (first object)]
         [method-class (find-method-class-or-fail class method-name)])
    (call-method2 object method-class method-name rand-exprs rand-env)))

(define (call-method2 object method-class method-name rand-exprs rand-env)
  (let* ([method-table (third method-class)]
         [method-info (hash-ref method-table method-name)]
         [method-params (first method-info)]
         [method-body (second method-info)]
         [args (if (> (length rand-exprs) 1)
                            (eval-exprList (second rand-exprs) rand-env)
                            null
                            )]
         [object-env (second object)]
         [env-with-this (add-binding object-env 'this object)]
         [object-env-with-parameter-bindings (add-bindings env-with-this
                                                           method-params
                                                           args)]
         [super-method-class (first method-class)]
         [method-env (add-binding object-env-with-parameter-bindings
                                  'super
                                  (list method-name super-method-class))])
    (if (= (length args) (length method-params))
        (last (eval-exprList method-body method-env))
        (error (~a "Arguments and parameter count don't match " method-params args))
        )
    ))

(define (add-field-bindings env field-names)
  (if (empty? field-names)
      env
      (add-field-bindings (add-binding env (first field-names) (void))
                          (rest field-names))))

(define (collect-field-names class)
  (let ([superclass (first class)]
        [field-names (second class)])
    (if (not superclass)
        field-names
        (remove-duplicates (append field-names
                                   (collect-field-names superclass))))))

(define (eval-opt-method-list opt-method-list env)
  (if (> (length opt-method-list) 1)
      (let*
          ([method-list (second opt-method-list)]
           [method (second method-list)]
           [method-name-token (second method)]
           [method-name (second method-name-token)]
           [method-body-expr (seventh method)]
           [method-params (eval-opt-name-list (fourth method) env)]
           [nested-opt-method-list (third method-list)])
        (cons (list method-name method-params method-body-expr) (eval-opt-method-list nested-opt-method-list env)))
      null
      ))

(define (eval-opt-name-list opt-name-list env)
  (if (> (length opt-name-list) 1)
      (let*
          ([name-list (second opt-name-list)]
           [name-token (second name-list)]
           [name (second name-token)]
           [nested-opt-name-list (third name-list)])
        (cons name (eval-opt-name-list nested-opt-name-list env)))
      null
      ))

(define (eval-atom atom-expr env)
  ;; atom        := NAME | STRING | number
  (let* ([name-string-number (second atom-expr)]
         [tag (first name-string-number)]
         [evaluator (case tag
                      [(NAME) eval-name]
                      [(STRING) eval-string]
                      [(number) eval-number])])
    (evaluator name-string-number env)))

(define (eval-name name-expr env)
  ;; + - * / string-append string<? string=? not = <
  (case (second name-expr)
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
    [else (lookup-name env (second name-expr))]))

(define (eval-string string-expr env) (second string-expr))
(define (eval-number number-expr env)
  ;; number      := INT | FLOAT
  (second (second number-expr)))

(define (eval-let let-expr env)
  ;; let         := LET OPAREN NAME expr CPAREN expr
  (let* ([name (second (fourth let-expr))]
         [value-expr (fifth let-expr)]
         [body-expr (seventh let-expr)])
    (eval-expr body-expr
               (add-binding env name (eval-expr value-expr env)))))

(define (eval-lambda lambda-expr env)
  ;; lambda      := LAMBDA OPAREN NAME CPAREN expr
  (let* ([name (second (fourth lambda-expr))]
         [body-expr (sixth lambda-expr)])
    (lambda (value)
      (eval-expr body-expr
                 (add-binding env name value)))))

(define (eval-invocation invocation-expr env)
  ;; invocation  := OPAREN exprList CPAREN
  (let* ([exprList-expr (third invocation-expr)]
         [rator-expr (second (second exprList-expr))]
         [values (eval-exprList exprList-expr env)]
         [rator (first values)]
         [rands (rest values)])
    (apply rator rands)))

(define (repl)
  (parameterize ([current-read-interaction (lambda (_ in)
                                             (read-line in))]
                 [current-eval (lambda (e)
                                 (when (non-empty-string? (cdr e))
                                   (eval (cdr e))))])
    (read-eval-print-loop)))


;; a7 tests
(module+ test
  (check-equal? (eval #<<CODE
define Box
  class Object (x) {
    initialize(x0) {
      set x x0
    }
    reset(new-x) {
      set x new-x
    }
    fetch() {
      x
    }
  }

define NumberBox
  class Box (x) {
    initialize(x0) {
      super(x0)
    }
    add1() {
      set x (+ x 1)
    }
    sub1() {
      set x (- x 1)
    }
  }

define AdderBox
  class NumberBox (x) {
    initialize(x0) {
      super(x0)
    }
    add(y) {
      set x (+ x y)
    }
    sub(y) {
      send this add((- y))
    }
  }

define StatelessAdder
  class Object () {
    add2(x y) {
      let (box1 new AdderBox(x))
       send box1 add(y)
    }
  }

define adder new StatelessAdder()
send adder add2(3 39)
CODE
) 42)
  (check-equal?
(eval #<<EOF
define Tester
class Object(x) {
initialize() {
set x -1
}
tick() {
set x (+ 1 x)
x
}
}

define eg new Tester()
send eg tick()
EOF
) 0)
  )

(module+ test
  (check-equal? (eval "7") 7)
  (check-equal? (eval "7.7") 7.7)
  (check-equal? (eval "\"a string\"") "a string")
  (check-exn exn:fail? (thunk (eval "foo")))
  (check-exn exn:fail? (thunk (eval "(list)")))
  (check-equal? (eval "7 8") 8)
  (check-equal? (eval "(+)") 0)
  (check-equal? (eval "(+ 7)") 7)
  (check-equal? (eval "(+ 7 8)") 15)
  (check-equal? (eval "(+ 7 8 15.0)") 30.0)
  (check-exn exn:fail? (thunk (eval "(-)")))
  (check-equal? (eval "(- 7)") -7)
  (check-equal? (eval "(- 7 -8)") 15)
  (check-equal? (eval "(- 7 -8 15.0)") 0.0)
  (check-equal? (eval "(*)") 1)
  (check-equal? (eval "(* 7)") 7)
  (check-equal? (eval "(* 7 8)") 56)
  (check-equal? (eval "(* 7 8 15.0)") 840.0)
  (check-exn exn:fail? (thunk (eval "(/)")))
  (check-exn exn:fail? (thunk (eval "(/ 1 0)")))
  (check-equal? (eval "(/ 7)") 1/7)
  (check-equal? (eval "(/ 7 8)") 7/8)
  (check-equal? (eval "(/ 7 8 15.0)") (/ 7 8 15.0))
  (check-equal? (eval "(+ 7 (- 8 1))") (+ 7 (- 8 1)))
  (check-equal? (eval "(+ 7 (+ 8 1)) (+ 7 (- 8 1))") (+ 7 (- 8 1)))
  (check-equal? (eval "(string-append)") "")
  (check-equal? (eval "(string-append \"abc\")") "abc")
  (check-equal? (eval "(string-append \"abc\" \"def\")") (string-append "abc" "def"))
  (check-equal? (eval "(string-append \"abc\" \"def\" \"ghi\")") (string-append "abc" "def" "ghi"))
  (check-exn exn:fail? (thunk (eval "(string<?)")))
  (check-equal? (eval "(string<? \"abc\")") #t)
  (check-equal? (eval "(string<? \"abc\" \"def\")") (string<? "abc" "def"))
  (check-equal? (eval "(string<? \"def\" \"abc\")") (string<? "def" "abc"))
  (check-equal? (eval "(string<? \"abc\" \"def\" \"ghi\")") (string<? "abc" "def" "ghi"))
  (check-exn exn:fail? (thunk (eval "(string=?)")))
  (check-equal? (eval "(string=? \"abc\")") #t)
  (check-equal? (eval "(string=? \"abc\" \"def\")") (string=? "abc" "def"))
  (check-equal? (eval "(string=? \"abc\" \"abc\")") (string=? "abc" "abc"))
  (check-equal? (eval "(string=? \"abc\" \"def\" \"ghi\")") (string=? "abc" "def" "ghi"))
  (check-exn exn:fail? (thunk (eval "(not)")))
  (check-exn exn:fail? (thunk (eval "(not 1 2)")))
  (check-equal? (eval "(not 1)") #f)
  (check-equal? (eval "(not 1.0)") #f)
  (check-equal? (eval "(not (= 0 1))") #t)
  (check-exn exn:fail? (thunk (eval "(=)")))
  (check-equal? (eval "(= 1)") #t)
  (check-equal? (eval "(= 0 1)") #f)
  (check-equal? (eval "(= 1.0 1)") #t)
  (check-equal? (eval "(= 0 1 2)") #f)
  (check-equal? (eval "(= (+ 1 0) 1 (- 2 1))") #t)
  (check-exn exn:fail? (thunk (eval "(<)")))
  (check-equal? (eval "(< 1)") #t)
  (check-equal? (eval "(< 1 0)") #f)
  (check-equal? (eval "(< 0 1)") #t)
  (check-equal? (eval "(< 0 1 2)") #t)
  (check-equal? (eval "(< 0 1 2.0)") #t)
  (check-equal? (eval "(< (+ 1 0) 1 (- 2 1))") #f)

  (check-equal? (eval "(+ 7 (+ 8 1)) (+ 7 (- 8 1)) (< 1 1 1) \"end\"") "end")

  (check-equal? (eval "let (x (+ 1 2)) (+ x 3)") 6)
  ;; these are allowed to be implementation defined, so I just return a
  ;; procedure; there are other ways to proceed
  #;(check-equal? (eval "lambda (x) (* x x)")
                (list 'x
                      '(expr
                         (invocation
                           (OPAREN #f)
                           (exprList
                             (expr (atom (NAME *)))
                             (optExprList
                               (exprList
                                 (expr (atom (NAME x)))
                                 (optExprList (exprList (expr (atom (NAME x))) (optExprList))))))
                           (CPAREN #f)))
                      (new-environment)))
  #;(check-equal? (eval "let (y 1) lambda (x) (* y x)")
                (list 'x
                      '(expr
                         (invocation
                           (OPAREN #f)
                           (exprList
                             (expr (atom (NAME *)))
                             (optExprList
                               (exprList
                                 (expr (atom (NAME y)))
                                 (optExprList (exprList (expr (atom (NAME x))) (optExprList))))))
                           (CPAREN #f)))
                      #hash((y . 1))))
  (check-equal? (eval "(lambda (x) (* x x) 7)") 49)
  (check-equal? (eval "let (square lambda (x) (* x x)) (square 7)") 49)
  (check-equal? (eval "define foo 3") 3)
  (check-equal? (eval "define foo 3 foo") 3)
  (check-equal? (eval "define foo 3 4") 4)
  (check-equal? (eval "define foo 3 (+ 1 foo)") 4)
  (check-equal? (eval "define foo (/ 8 2) let (x (+ 1 2)) (+ x foo)") 7)
  )

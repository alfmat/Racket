#lang racket

;; The `[data #f]` is a default value for the 2nd positional argument.
;; That way, this function can take 1 arg or 2 args.
(define (token type [data #f])
  (list type data))

;;;; Token creator functions
;;;;
;;;; Given a string matching a regexp, return a token for that input or #f for
;;;; no token.

(define (skip-match str) null)

(define (punctuation-token str)
  (token
    (case str
      [(";") 'SEMICOLON]
      [("(") 'OPAREN]
      [(")") 'CPAREN]
      [("{") 'OBRACE]
      [("}") 'CBRACE]
      [(",") 'COMMA]
      [(".") 'PERIOD]
      )))

(define (float-token str)
  (token 'FLOAT (string->number str)))

(define (int-token str)
  (token 'INT (string->number str)))

(define (name-or-keyword-token str)
  (case str
    [("def" "fun" "if" "not" "and" "or")
     (token (string->symbol (string-upcase (string-trim str))))]
    [else (token 'NAME (string->symbol str))]))

(define (string-literal str)
  (token 'STRING str)
  )

(define (invalid-literal str)
  (list (token 'INVALID str))
  )

;;;; Lexing rules table
;;;;
;;;; Each item in the table is a 2-tuple (i.e. list of 2 elements):
;;;; 1. a regexp to detect a token at the beginning of a string
;;;; 2. a function (from above) to take the matched string and create a token

(define re-table
  (list
    (list #rx"^[ \r\n\t]+" skip-match) ; whitespace
    (list #rx"^//[^\n]+(\n|$)|^/\\*.*\\*/" skip-match) ; // comments
    (list #rx"^[;\\(\\){},.]" punctuation-token)
    (list #rx"^[-]*[0-9]+\\.[0-9]+(?=[\r\n\t (){},;.]|$)" float-token)
    (list #rx"^[-]*[0-9]+(?=[\r\n\t (){},;.]|$)" int-token)
    (list #rx"^[A-Za-z][A-Za-z0-9]*(?=[\r\n\t (){},;.]|$)|^.(?=[\r\n\t (){},;.]|$)" name-or-keyword-token)
    (list #rx"^\"(.*)\"(?=[\r\n\t (){},;.]|$)" string-literal)
    ))

(define (longest-match n pos match items)
  (if (null? items)
      (if (= match 0)
          7
          pos
          )
      (if (list? (first items))
          (if (> (string-length (first (first items))) match)
              (longest-match (+ n 1) n (string-length (first (first items))) (rest items))
              (longest-match (+ n 1) pos match (rest items))
              )
          (longest-match (+ n 1) pos match (rest items))
      )
  )
)

(define (get-item n items)
  (if (null? items)
      null
      (if (<= n 0)
          (if (boolean? (first items))
              "1"
              (first (first items))
              )
          (get-item (- n 1) (rest items))
      )
  )
  )

(define (get-string n items)
  (if (null? items)
      null
      (if (<= n 0)
          (if (boolean? (first items))
              "1"
              (first (rest (first items)))
              )
          (get-string (- n 1) (rest items))
      )
  )
  )

(define (check-match items)
  (define n (longest-match 0 0 0 items))
  (if (= n 7)
      #f
      (if (or (= n 0) (= n 1))
              (skip-match (get-item n items))
              (if (= n 2)
                  (punctuation-token (get-item n items))
                  (if (= n 3)
                      (float-token (get-item n items))
                      (if (= n 5)
                          (name-or-keyword-token (get-item n items))
                          (if (= n 6)
                              (string-literal (get-string n items))
                              (if (= n 4)
                                  (int-token (get-item n items))
                                  #f
                                  )
                              )
                          )
                      )
                  )
              )
      )
  )

(define (cut-length str items)
  (define n (longest-match 0 0 0 items))
  (if (= n 7)
      (substring str 1)
      (substring str (string-length (get-item n items)) 
          )
      )
  )

(define (lex str)
  (if (= (string-length str) 0)
      null
      (if (list? (check-match 
              (map (lambda (entry) (regexp-match (first entry) str)) re-table)
              ))
          (if (null? (check-match 
              (map (lambda (entry) (regexp-match (first entry) str)) re-table)
              ))
              (lex (cut-length str (map (lambda (entry) (regexp-match (first entry) str)) re-table)))
              (cons (check-match 
                     (map (lambda (entry) (regexp-match (first entry) str)) re-table)
                     )
                    (lex (cut-length str (map (lambda (entry) (regexp-match (first entry) str)) re-table)))
                    )
              )
          (invalid-literal str)
          )
      )
  )

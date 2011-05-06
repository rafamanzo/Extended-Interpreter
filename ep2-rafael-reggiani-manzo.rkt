;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Segundo Exercício-Programa: Interpretador Estendido      ;;
;; MAC0316/MAC5754 - Conceitos de Linguagens de Programação ;;
;;                                                          ;;
;; Rafael Reggiani Manzo                                    ;;
;; NUSP 6797150                                             ;;
;; Bacharelado em Ciência da Computação                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO Testes e comentários para todas as funções
;;TODO Implementar interpretação postergada (em arquivo separado)

#lang plai

(require racket/trace)

(define-type CFAE
  [num (n number?)] 
  [binop (op procedure?) (lhs CFAE?) (rhs CFAE?)]
  [id (name symbol?)]
  [if0 (c CFAE?) (t CFAE?) (e CFAE?)]
  [fun (args (listof symbol?)) (body CFAE?)]
  [app (f CFAE?) (args (listof CFAE?))])

(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value CFAE-Value?) (env Env?)])

(define-type CFAE-Value
  [numV (n number?)]
  [closureV (params (listof symbol?))
            (body CFAE?)
            (env Env?)])

;; notinlist? : ? listof? precedure -> boolaen
;; Checks if one item is in the given list
(define (notinlist? item l pred)
  (if (empty? l)
    #t
    (if (pred item (car l))
      #f
      (notinlist? item (cdr l) pred)))) 

;; symbolListValidate-aux : listofsymbol -> Boolean
;; Checks the list of symbol for duplicated symbols
(define (symbolListValidate-aux l)
  (if (empty? l)
    #t
    (if (notinlist? (car l) (cdr l) symbol=?)
      (symbolListValidate-aux (cdr l))
      #f)))

;; symbolListValidate-aux : listofsymbol -> listofsymbol
;; Checks the list of symbol for duplicated symbols
(define (symbolListValidate l)
  (if (symbolListValidate-aux l)
    l
    (error 'symbolListValidate "Symbol already taken")))

;; itsaapp : s-expression -> CFAE
;; creates an app from a s-expression
(define (itsaapp sexp)
  (app (parse (first sexp)) (map parse (cdr sexp))))

;; parse : s-exp -> CFAE
;; Consumes an s-expression and generates the corresponding CFAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
      (case (length sexp)
        [(0 1) (error 'parse "Invalid list length")] 
        [(3) (case (first sexp)
          [(+) (binop + (parse (second sexp)) (parse (third sexp)))]
          [(-) (binop - (parse (second sexp)) (parse (third sexp)))] 
          [(*) (binop * (parse (second sexp)) (parse (third sexp)))]
          [(/) (binop / (parse (second sexp)) (parse (third sexp)))]
          [(with) (app (fun (symbolListValidate (map first (second sexp))) (parse (third sexp))) (map (lambda (l) (parse (second l))) (second sexp)))]
          [(fun) (fun (symbolListValidate (second sexp)) (parse (third sexp)))]
          [else (itsaapp sexp)])]
        [(4) (case (first sexp)  
          [(if0) (if0 (parse (second sexp)) (parse (third sexp)) (parse (third (cdr sexp))))]
          [else (itsaapp sexp)])]
        [else (itsaapp sexp)])]))

;; operate : procedure numV numV -> num
(define (operate o l r)    
  (if (and (eq? o /) (= (numV-n r) 0))
    (error 'operate "Division by 0")
    (numV (o (numV-n l) (numV-n r)))))

;;TODO Implementar substituição postergada
;;TODO Implementar interpretação de funções
;;TODO Implementar interpretação de with 0
;; interp : CFAE -> CFAE-Value
;; Consumes a CFAE representation of an expression and computes
;;   the corresponding numerical result
(define (interp expr env)
  (type-case  CFAE expr
    [num (n) (numV n)]
    [binop (o l r) 
      (operate o (interp l env) (interp r env))]
    [if0 (c t e)
      (if (= (numV-n (interp c env)) 0)
        (interp t env)
        (interp e env))]
    [fun (args body)
      (closureV 
    [id (v) (error 'interp "Free identifier")]))

;; notinlist? tests
;;(test (notinlist? 5 '(1 2 3 4) =) #t)
;;(test (notinlist? 5 '(1 2 3 4 5) =) #f)
;;(test (notinlist? 5 '() =) #t)

;;symbolListValidate tests
;;(test (symbolListValidate '(a b c)) '(a b c))
;;(test/exn (symbolListValidate '(a b c c)) "Symbol already taken")
;;(test (symbolListValidate '()) '())

;;(trace parse)
;; parser tests
;;(test (parse '{with {{x 2} {y 3}}
;;                {with {{z {+ x y}}}
;;                  {+ x z}}})
;;  (app (fun '(x y)
;;         (app (fun '(z) (binop + (id 'x) (id 'z))) (list (binop + (id 'x) (id 'y)))))
;;         (list (num 2) (num 3))))
;;(test (parse '{fun {x y} {* x y}}) (fun '(x y) (binop * (id 'x) (id 'y))))
;;(test (parse '{macaco x 5 voador}) (app (id 'macaco) (list (id 'x) (num 5) (id 'voador))))
;;(test (parse '{macaco voador macaco}) (app (id 'macaco) (list (id 'voador) (id 'macaco))))
;;(test (parse '{with {} {+ 2 x}}) (app (fun '() (binop + (num 2) (id 'x))) '()))
;;(test (parse '{if0 0 0 0}) (if0 (num 0) (num 0) (num 0))) 
;;(test/exn (parse '{}) "Invalid list length")
;;(test/exn (parse '{+}) "Invalid list length")
;;(test/exn (parse '{with {{x 2} {x 3}} {x}}) "Symbol already taken")
;;(test/exn (parse '{fun {x x} {x}}) "Symbol already taken")

;;interp tests
;;(test/exn (interp (binop / (num 8) (binop - (num 2) (num 2)))) "Division by 0")
;;(test/exn (interp (with '() (binop + (id 'x) (num 2)))) "free identifier")
;;(test (interp (with (list (binding 'x (num 2)) (binding 'y (num 3))) (with (list (binding 'z (binop + (id 'x) (id 'y)))) (binop + (id 'x) (id 'z))))) 7)

;;(parse(read))
;;(interp(parse(read)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Segundo Exercício-Programa: Interpretador Estendido      ;;
;; MAC0316/MAC5754 - Conceitos de Linguagens de Programação ;;
;;                                                          ;;
;; Rafael Reggiani Manzo                                    ;;
;; NUSP 6797150                                             ;;
;; Bacharelado em Ciência da Computação                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO Implementar substituição postergada
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

;;TODO validar listas de simbolos (em busca de repetidos) 
;;TODO Adaptar with para aplicação de função
;;TODO Implementar parse de funções
;;TODO Implementar parse de if0
;; parse : s-exp -> CFAE
;; Consumes an s-expression and generates the corresponding CFAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
      (case (length sexp)
        [(0 1 2) (error 'parse "Invalid list length")] 
        [(3) (case (first sexp)
          [(+) (binop + (parse (second sexp)) (parse (third sexp)))]
          [(-) (binop - (parse (second sexp)) (parse (third sexp)))] 
          [(*) (binop * (parse (second sexp)) (parse (third sexp)))]
          [(/) (binop / (parse (second sexp)) (parse (third sexp)))]
          [(with) (app (fun (map first (second sexp)) (parse (third sexp))) (map (lambda (l) (parse (second l))) (second sexp)))]
          [(fun) (fun (second sexp) (parse (third sexp)))]
          [else (error 'parse "Unexpected symbol")])]
        [(4)  (if (symbol=? (first sexp) 'if0)
          [(if0 (parse (second sexp)) (parse (third sexp)) (parse (third (cdr sexp))))]
          [(error 'parse "Unexpected symbol")])]
        [else 
          (app (parse (second sexp)) (map parse (cdr (cdr sexp))))])]))

;;TODO Implementar interpretação de funções
;;TODO Implementar interpretação de with 0
;; interp : WAE -> number
;; Consumes a WAE representation of an expression and computes
;;   the corresponding numerical result
;;(define (interp expr)
;;  (type-case WAE expr
;;    [num (n) n]
;;    [binop (o l r) 
;;      (if (and (eq? o /) (= (interp r) 0))
;;        (error 'interp "Division by 0")
;;        (o (interp l) (interp r)))]
;;    [with (lob bound-body) (interp (subst bound-body lob))]
;;    [id (v) (error 'interp "free identifier")]))

(trace parse)
;;TODO testes para if0, fun e app
;; parser tests
(test (parse '{with {{x 2} {y 3}}
                {with {{z {+ x y}}}
                  {+ x z}}})
  (app (fun '(x y)
         (app (fun '(z) (binop + (id 'x) (id 'z))) (list (binop + (id 'x) (id 'y)))))
         (list (num 2) (num 3))))
(test (parse '{with {} {+ 2 x}}) (app (fun '() (binop + (num 2) (id 'x))) '()))
(test/exn (parse '{macaco voador macaco}) "Unexpected symbol")
(test/exn (parse '{+ 2}) "Invalid list length")

;;interp tests
;;(test/exn (interp (binop / (num 8) (binop - (num 2) (num 2)))) "Division by 0")
;;(test/exn (interp (with '() (binop + (id 'x) (num 2)))) "free identifier")
;;(test (interp (with (list (binding 'x (num 2)) (binding 'y (num 3))) (with (list (binding 'z (binop + (id 'x) (id 'y)))) (binop + (id 'x) (id 'z))))) 7)

(parse(read))
;;(interp(parse(read)))

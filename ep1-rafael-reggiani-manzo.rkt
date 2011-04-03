;TODO Interpreter para a linguagem WAE (inclui implementar subst)
;TODO Escrever testes e contratos para todas as funções
;TODO checar se o inglês está correto =D

#lang plai
(require racket/trace)

(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])

(define-type WAE
  [num (n number?)] 
  [binop (op procedure?) (lhs WAE?) (rhs WAE?)]
  [with (lob (listof Binding?)) (body WAE?)]
  [id (name symbol?)])

;; fing_binding : listofbinding binding -> boolean
;; auxiliary function that searches a list of bindings for a binding
(define (find_binding lob binding)
  (cond
    [(= (length lob) 0) #f]
    [(symbol=? binding (binding-name (car lob))) #t]
    [else (find_binding (cdr lob) binding)]))

;; produce_lob : s-exp -> listofbinding
;; auxiliary function that produces the list of bindings for an multiarmed with
(define (produce_lob sexp actual)
  (if (= (length sexp) 0) actual
    (if (= (length (car sexp)) 2)
      (if (symbol? (first (car sexp)))
        (if (find_binding actual (first (car sexp)))
          (error 'produce_lob "There are multiple bindings of the same identifier in a single with")         
          (produce_lob (cdr sexp) (cons (binding (first (car sexp)) (parse (second (car sexp)))) actual)))
        (error 'produce_lob (format "The name of a binding must be a symbol. Given: ~a" (first (car sexp)))))
      (error 'produce_lob "Invalid list length count")))) 

;; parse : s-exp -> WAE
;; Consumes an s-expression and generates the corresponding WAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
      (if (= (length sexp) 3)
        (case (first sexp)
          [(+) (binop + (parse (second sexp)) (parse (third sexp)))]
          [(-) (binop - (parse (second sexp)) (parse (third sexp)))] 
          [(*) (binop * (parse (second sexp)) (parse (third sexp)))]
          [(/) (binop / (parse (second sexp)) (parse (third sexp)))]
          [(with) (with (produce_lob (second sexp) '()) (parse (third sexp)))]
          [else (error 'parse "Unexpected symbol")])
        (error 'parse "Invalid list length count"))]))

;; interp : WAE -> number
;; Consumes a WAE representation of an expression and computes
;;   the corresponding numerical result
;(define (interp expr)
;  ...)

;;find_binding tests
;(test (find_binding '() 'x) #f)
;(test (find_binding (list (binding 'x (binop + (num 8) (num 10))) (binding 'z (id 'macaco))) 'y) #f)
;(test (find_binding (list (binding 'x (binop + (num 8) (num 10))) (binding 'z (id 'macaco))) 'z) #t)

;;produce_lob tests
;(test/exn (parse '{with {{x 2} {x 3}}
;	                  {+ x 2}})
;          "There are multiple bindings of the same identifier in a single with")
;(test (produce_lob '() '()) '())
;(test (produce_lob '((x 2) (y (+ 3 5)) (z w)) '()) (list (binding 'z (id 'w)) (binding 'y (binop + (num 3) (num 5))) (binding 'x (num 2))))
;(test/exn (produce_lob '((2 2)) '()) "The name of a binding must be a symbol")
;(test/exn (produce_lob '((x)) '()) "Invalid list length count")
;(test/exn (produce_lob '((x 2 3)) '()) "Invalid list length count")

;; parser tests
;(test (parse '{with {{x 2} {y 3}}
;                {with {{z {+ x y}}}
;                  {+ x z}}})
;  (with (list (binding 'y (num 3)) (binding 'x (num 2)))
;    (with (list (binding 'z (binop + (id 'x) (id 'y))))
;      (binop + (id 'x) (id 'z)))))
;(test (parse '{with {} {+ 2 x}}) (with '() (binop + (num 2) (id 'x))))
;(test/exn (parse '{macaco voador}) "Unexpected symbol")
;(test/exn (parse '{+ 2 3 4 5}) "Invalid list length count")
;(test/exn (parse '{+ 2}) "Invalid list length count")

;;interpreter tests
;
;(parse (read))

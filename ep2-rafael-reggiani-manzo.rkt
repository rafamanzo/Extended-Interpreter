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

(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])

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

;;TODO Adaptar with para aplicação de função
;;TODO Implementar parse de funções
;;TODO Implementar parse de with 0
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

;; subst_lob : listofbinding symbol WAE listofbinding
;; substitutes the WAE for every occurence of the symbol on the first list of bindings and put the result on the lasts listofbindings
;; when the first list is full processed, the last is returned
(define (subst_lob lob sub-id val result)
  (cond
    [(= (length lob) 0) result]
    [else (subst_lob (cdr lob) sub-id val (cons (binding (binding-name (car lob)) (subst_aux (binding-named-expr (car lob)) sub-id val)) result))]))  

;; subst_aux : WAE symbol WAE → WAE
;; substitutes second argument with third argument in first argument,
;; as per the rules of substitution; the resulting expression contains
;; no free instances of the second argument
(define (subst_aux expr sub-id val)
  (type-case WAE expr
    [num (n) expr]
    [binop (o l r) (binop o (subst_aux l sub-id val) (subst_aux r sub-id val))]
    [with (lob bound-body)
    (if (find_binding lob sub-id)
      expr
      (with (subst_lob lob sub-id val '()) (subst_aux bound-body sub-id val)))]
    [id (v) (if (symbol=? v sub-id) val expr)]))

;; subst : WAE listofbinding -> WAE
;; substitutes every element of the listofbinding on the WAE
(define (subst expr lob)
  (cond
    [(= (length lob) 0) expr]
    [else (subst (subst_aux expr (binding-name (car lob)) (binding-named-expr (car lob))) (cdr lob))]))

;;TODO Implementar parse de funções
;;TODO Implementar parse de with 0
;; interp : WAE -> number
;; Consumes a WAE representation of an expression and computes
;;   the corresponding numerical result
(define (interp expr)
  (type-case WAE expr
    [num (n) n]
    [binop (o l r) 
      (if (and (eq? o /) (= (interp r) 0))
        (error 'interp "Division by 0")
        (o (interp l) (interp r)))]
    [with (lob bound-body) (interp (subst bound-body lob))]
    [id (v) (error 'interp "free identifier")]))

;;find_binding tests
(test (find_binding '() 'x) #f)
(test (find_binding (list (binding 'x (binop + (num 8) (num 10))) (binding 'z (id 'macaco))) 'y) #f)
(test (find_binding (list (binding 'x (binop + (num 8) (num 10))) (binding 'z (id 'macaco))) 'z) #t)

;;produce_lob tests
(test/exn (parse '{with {{x 2} {x 3}}
	                  {+ x 2}})
          "There are multiple bindings of the same identifier in a single with")
(test (produce_lob '() '()) '())
(test (produce_lob '((x 2) (y (+ 3 5)) (z w)) '()) (list (binding 'z (id 'w)) (binding 'y (binop + (num 3) (num 5))) (binding 'x (num 2))))
(test/exn (produce_lob '((2 2)) '()) "The name of a binding must be a symbol")
(test/exn (produce_lob '((x)) '()) "Invalid list length count")
(test/exn (produce_lob '((x 2 3)) '()) "Invalid list length count")

;; parser tests
(test (parse '{with {{x 2} {y 3}}
                {with {{z {+ x y}}}
                  {+ x z}}})
  (with (list (binding 'y (num 3)) (binding 'x (num 2)))
    (with (list (binding 'z (binop + (id 'x) (id 'y))))
      (binop + (id 'x) (id 'z)))))
(test (parse '{with {} {+ 2 x}}) (with '() (binop + (num 2) (id 'x))))
(test/exn (parse '{macaco voador macaco}) "Unexpected symbol")
(test/exn (parse '{+ 2 3 4 5}) "Invalid list length count")
(test/exn (parse '{+ 2}) "Invalid list length count")

;;subst_lob
(test (subst_lob (list (binding 'y (binop + (id 'x) (num 2))) (binding 'z (num 3))) 'x (num 2) '()) (list (binding 'z (num 3)) (binding 'y (binop + (num 2) (num 2))))) 
(test (subst_lob (list (binding 'y (binop + (id 'x) (num 2))) (binding 'z (num 3))) 'w (num 2) '()) (list (binding 'z (num 3)) (binding 'y (binop + (id 'x) (num 2))))) 

;;subst_aux tests
(test (subst_aux (binop + (id 'x) (num 2)) 'x (num 2)) (binop + (num 2) (num 2))) 
(test (subst_aux (binop + (id 'x) (num 2)) 'y (num 3)) (binop + (id 'x) (num 2)))
(test (subst_aux (with (list (binding 'x (num 3)) (binding 'y (num 4))) (binop + (id 'x) (id 'z))) 'x (num 5))  
  (with (list (binding 'x (num 3)) (binding 'y (num 4))) (binop + (id 'x) (id 'z))))
(test (subst_aux (with (list (binding 'x (num 3)) (binding 'y (num 4))) (binop + (id 'x) (id 'z))) 'z (num 5))  
  (with (list (binding 'y (num 4)) (binding 'x (num 3))) (binop + (id 'x) (num 5))))
(test (subst_aux (with (list (binding 'z (binop + (id 'x) (id 'y)))) (binop + (id 'x) (id 'z))) 'x (num 2))
  (with (list (binding 'z (binop + (num 2) (id 'y)))) (binop + (num 2) (id 'z))))

;;subst tests
(test (subst (with (list (binding 'z (binop + (id 'x) (num 2))) (binding 'w (num 2))) (binop + (id 'x) (id 'w))) (list (binding 'x (num 4)) (binding 'y (num 1))))
  (with (list (binding 'z (binop + (num 4) (num 2))) (binding 'w (num 2))) (binop + (num 4) (id 'w))))  

;;interp tests
(test/exn (interp (binop / (num 8) (binop - (num 2) (num 2)))) "Division by 0")
(test/exn (interp (with '() (binop + (id 'x) (num 2)))) "free identifier")
(test (interp (with (list (binding 'x (num 2)) (binding 'y (num 3))) (with (list (binding 'z (binop + (id 'x) (id 'y)))) (binop + (id 'x) (id 'z))))) 7)

(interp(parse(read)))

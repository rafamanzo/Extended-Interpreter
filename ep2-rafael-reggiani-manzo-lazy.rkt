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
            (env Env?)]
  [exprV (expr CFAE?) (env Env?)])

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

;; operate : procedure numV numV -> numV
;; consumes two numVs and operates them returning the numV resultant
(define (operate o l r)    
  (if (and (eq? o /) (= (numV-n r) 0))
    (error 'operate "Division by 0")
    (numV (o (numV-n l) (numV-n r)))))

;; notinenv? : Env symbol -> Env
;; checks if an indentifier isn't bound
(define (notinenv? env id)
  (if (mtEnv? env)
    #t
    (if (symbol=? id (anEnv-name env))
      #f
      (notinenv? (anEnv-env env) id))))

;; generateEnv : listofsymbol listofCFAE Env -> Env
;; consumes a list of params and another of args bounding them to the enviroment
(define (generateEnv args vals env)
  (if (and (empty? args) (empty? vals))
    env
    (if (empty? args)
      (error 'generateEnv "Too many arguments")
      (if (empty? vals)
        (error 'generateEnv "More arguments were expected")
        (if (notinenv? env (car args)) 
          (generateEnv (cdr args) (cdr vals) (anEnv (car args) (exprV (car vals) env) env))
          (error 'generateEnv "Indentifier already bound"))))))

;; lookup : symbol Env -> CFAE-Value
(define (lookup name env)
  (type-case Env env
    [mtEnv () (error 'lookup "no binding for identifier")]
    [anEnv (bound-name bound-value rest-env)
      (if (symbol=? bound-name name)
        bound-value
        (lookup name rest-env))]))

;; strict : CFAE-Value -> CFAE-Value [excluding exprV]
(define (strict e)
  (type-case CFAE-Value e
    [exprV (expr env)
      (strict (interp expr env))]
    [else e]))

;; interp : CFAE -> CFAE-Value
;; Consumes a CFAE representation of an expression and computes
;;   the corresponding numV result
(define (interp expr env)
  (type-case  CFAE expr
    [num (n) (numV n)]
    [binop (o l r) 
      (operate o (strict (interp l env)) (strict (interp r env)))]
    [if0 (c t e)
      (if (= (numV-n (strict (interp c env))) 0)
        (interp t env)
        (interp e env))]
    [fun (args body)
      (closureV args body env)]
    [app (f args)
      (local ([define fun-val (strict (interp f env))])
        (interp
          (closureV-body fun-val)
          (generateEnv (closureV-params fun-val) args env)))]
    [id (v) (lookup v env)]))

;; notinlist? tests
(test (notinlist? 5 '(1 2 3 4) =) #t)
(test (notinlist? 5 '(1 2 3 4 5) =) #f)
(test (notinlist? 5 '() =) #t)

;;symbolListValidate tests
(test (symbolListValidate '(a b c)) '(a b c))
(test/exn (symbolListValidate '(a b c c)) "Symbol already taken")
(test (symbolListValidate '()) '())

;; parser tests
(test (parse '{with {{x 2} {y 3}}
                {with {{z {+ x y}}}
                  {+ x z}}})
  (app (fun '(x y)
         (app (fun '(z) (binop + (id 'x) (id 'z))) (list (binop + (id 'x) (id 'y)))))
         (list (num 2) (num 3))))
(test (parse '{fun {x y} {* x y}}) (fun '(x y) (binop * (id 'x) (id 'y))))
(test (parse '{macaco x 5 voador}) (app (id 'macaco) (list (id 'x) (num 5) (id 'voador))))
(test (parse '{macaco voador macaco}) (app (id 'macaco) (list (id 'voador) (id 'macaco))))
(test (parse '{with {} {+ 2 x}}) (app (fun '() (binop + (num 2) (id 'x))) '()))
(test (parse '{if0 0 0 0}) (if0 (num 0) (num 0) (num 0))) 
(test/exn (parse '{}) "Invalid list length")
(test/exn (parse '{+}) "Invalid list length")
(test/exn (parse '{with {{x 2} {x 3}} {x}}) "Symbol already taken")
(test/exn (parse '{fun {x x} {x}}) "Symbol already taken") 

;; operate tests
(test (operate - (numV 2) (numV 2)) (numV 0))
(test (operate + (numV 2) (numV 2)) (numV 4))
(test (operate / (numV 2) (numV 2)) (numV 1))
(test (operate * (numV 2) (numV 2)) (numV 4))
(test/exn (operate / (numV 1) (numV 0)) "Division by 0")
 
;;notinenv? tests
(test (notinenv? (mtEnv) 'x) #t)
(test (notinenv? (anEnv 'y (numV 2) (mtEnv)) 'x) #t)
(test (notinenv? (anEnv 'x (numV 2) (mtEnv)) 'x) #f)

;;generateEnv tests
(test (generateEnv (list 'x 'y) (list (num 2) (num 3)) (mtEnv)) (anEnv 'y (exprV (num 3) (anEnv 'x (exprV (num 2) (mtEnv)) (mtEnv))) (anEnv 'x (exprV (num 2) (mtEnv)) (mtEnv))))
(test (generateEnv (list 'x 'y) (list (num 2) (num 3)) (anEnv 'z (exprV (num 1) (mtEnv)) (mtEnv))) (anEnv 'y (exprV (num 3) (anEnv 'x (exprV (num 2) (anEnv 'z (exprV (num 1) (mtEnv)) (mtEnv))) (anEnv 'z (exprV (num 1) (mtEnv)) (mtEnv)))) (anEnv 'x (exprV (num 2) (anEnv 'z (exprV (num 1) (mtEnv)) (mtEnv))) (anEnv 'z (exprV (num 1) (mtEnv)) (mtEnv)))))
(test/exn (generateEnv (list 'x 'y) (list (num 2) (num 3)) (anEnv 'y (numV 1) (mtEnv))) "Indentifier already bound")
(test/exn (generateEnv (list 'x) (list (num 2) (num 3)) (anEnv 'y (numV 1) (mtEnv))) "Too many arguments")
(test/exn (generateEnv (list 'x 'y 'z) (list (num 2) (num 3)) (anEnv 'w (numV 1) (mtEnv))) "More arguments were expected")

;;lookup tests
(test (lookup 'x (anEnv 'y (numV 1) (anEnv 'x (numV 2) (anEnv 'z (numV 3) (mtEnv))))) (numV 2))
(test/exn (lookup 'w (anEnv 'y (numV 1) (anEnv 'x (numV 2) (anEnv 'z (numV 3) (mtEnv))))) "no binding for identifier")
(test/exn (lookup 'x (mtEnv)) "no binding for identifier")

;;interp tests
(test/exn (interp (binop / (num 8) (binop - (num 2) (num 2))) (mtEnv)) "Division by 0")
(test/exn (interp (app (id 'macaco) (list (num 2))) (mtEnv)) "no binding for identifier")
(test/exn (interp (app (fun (list 'x) (if0 (binop * (id 'x) (id 'y)) (num 0) (num 1))) (list (binop / (num 6) (num 3)) (num 3))) (mtEnv)) "Too many arguments")
(test/exn (interp (app (fun (list 'x 'y) (if0 (binop * (id 'x) (id 'y)) (num 0) (num 1))) (list (num 3))) (mtEnv)) "More arguments were expected")
(test/exn (interp (app (fun (list 'x 'x) (if0 (binop * (id 'x) (id 'y)) (num 0) (num 1))) (list (num 2) (num 3))) (mtEnv)) "Indentifier already bound")
(test (interp (app (fun (list 'x 'y) (if0 (binop * (id 'x) (id 'y)) (num 0) (num 1))) (list (binop / (num 6) (num 3)) (num 3))) (mtEnv)) (numV 1))

;;(parse(read))
;;(interp(parse(read)))

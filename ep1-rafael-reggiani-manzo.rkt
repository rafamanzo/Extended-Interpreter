;TODO Parser e interpreter para a linguagem WAE (inclui implementar subst)
;TODO Escrever testes e contratos para todas as funções
;TODO Ao invés de tratar + e - individualmente, implementar operações binárias
;TODO Implementar / e *
;TODO With com multiplos identificadores (os identificadores são únicos)

(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])

(define-type WAE
  [num (n number?)]
  [binop (op procedure?) (lhs WAE?) (rhs WAE?)]
  [with (lob (listof Binding?)) (body WAE?)]
  [id (name symbol?)])

;; parse : s-exp -> WAE
;; Consumes an s-expression and generates the corresponding WAE
(define (parse sexp)
  ...)

;; interp : WAE -> number
;; Consumes a WAE representation of an expression and computes
;;   the corresponding numerical result
(define (interp expr)
  ...)


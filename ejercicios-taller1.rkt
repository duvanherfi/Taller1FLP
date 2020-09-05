#lang racket

;;JUAN SEBASTIAN GRAJALES CRUZ - 202010004
;;DUVAN HERNANDEZ FIGUEROA     - 202010009
;;DIEGO FERNANDO MUÑOZ ARCE    - 202010032

;; copy :
;; Propósito:
;; Procedimiento que recibe un número n y una entrda x
;; y retorna una lista con n ocurrencias de x.
#|
(define copy (lambda (n x)
               (if (= n 0)
                   empty
                   (cons x (copy (- n 1) x))
                 )
               )
  )

;; Pruebas
(copy 7 'seven)
(copy 4 (list 1 2 3))
(copy 0 (list 5 6 7))

;; list-tails:
;; Propósito:
;; Procedimiento que recibe como argumento una lista L
;; y retorna en una lista todas las
;; sublistas de los elementos consecutivos de la lista L.

(define list-tails (lambda (L)
                     (if (empty? L)
                        empty
                        (cons L (list-tails (cdr L)))
                        )
                     )
  )

;; Pruebas
(list-tails '(1 2 3 4 5))
(list-tails '(1 a (e 4) 5 v))


;; sublist:
;; Propósito:
;; Procedimiento recibe como argumento una lista L,
;; un núumero inicial i y un núumero final j.
;; y retorna la sublista entre el elemento en posición i y
;; el elemento en posición j (ambos elementos incluidos).

(define sublist (lambda (L i j)
                     (if (or (empty? L) (> i j))
                        empty                        
                        (cond
                          [(= i 0) (cons (car L) (sublist (cdr L) i (- j 1)))]
                          [(= i j) empty]
                          [else (sublist (cdr L) (- i 1) (- j 1))]
                        )
                     )
                  )
  )

;; Pruebas
(sublist '(a b c d e) 1 3)
(sublist '((a b) c a b c 9) 3 4)
|#
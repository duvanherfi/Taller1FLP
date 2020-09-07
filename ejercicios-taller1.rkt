#lang racket

;;JUAN SEBASTIAN GRAJALES CRUZ - 202010004
;;DUVAN HERNANDEZ FIGUEROA     - 202010009
;;DIEGO FERNANDO MUÑOZ ARCE    - 202010032
;----------------------------------------------------------------------------
  ;1.
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
;------------------------------------------------------------------------------
  ;2
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
;------------------------------------------------------------------------------
  ;3

;; sublist:
;; Propósito:
;; Procedimiento que recibe como argumento una lista L,
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
;---------------------------------------------------------------------------------
  ;4
;; exists?:
;; Propósito:
;; Procedimiento que recibe dos argumentos:
;; un predicado P y una lista L, y retorna
;; #t si algúun elemento de la lista L satisface el predicado P.
;; Devuelve #f en caso contrario.

(define exists? (lambda (P L)
                     (cond
                       [(empty? L) #f]
                       [(P (car L)) #t]             
                       [else (exists? P (cdr L))]
                     )
                  )
  )

;; Pruebas
(exists? empty? '(a b c c e))
(exists? symbol? '(a b c d 4))
;----------------------------------------------------------------------------------
  ;5
;; list-fibo:
;; Propósito:
;; Procedimiento que recibe como argumento un numero entero n,
;; y retorna una lista ascendente con los n-términos de la
;; sucesión fibonacci.

(define list-fibo (lambda (n)
                     (cond
                       [(< n 0) (error "No se pueden números negativos")]
                       [(= n 0) '(0)]
                       [(= n 1) '(0 1)]
                       [else (append (list-fibo (- n 1)) (list (+ (last (list-fibo (- n 2))) (last (list-fibo (- n 1))))))]                      
                       )
                  )
  )

;; Pruebas
(list-fibo 1)
(list-fibo 6)
;----------------------------------------------------------------------------------
  ;6
;; factorial:
;; Propósito:
;; Procedimiento que recibe como argumento un numero entero n,
;; y retorna el factorial del número n

(define factorial (lambda (n)
                     (cond
                       [(< n 0) (error "No se pueden números negativos")]
                       [(= n 0) 1]
                       [else (* n (factorial (- n 1)))]                                    
                       )
                  )
  )

;; Pruebas
;(factorial 4)
;(factorial 5)


;; list-facts-two:
;; Propósito:
;; Procedimiento que recibe como argumento un numero entero n,
;; y retorna una lista incremental de factoriales dobles.
;; Un factorial doble inicia en 1! si n es impar y continuará
;; calculando los factoriales de los núumeros impares hasta n!.
;; Si n es par, entonces inicia en 2! y continuará calculando
;; los factoriales de núumeros pares hasta n!

(define list-facts-two (lambda (n)
                     (cond
                       [(< n 0) (error "No se pueden números negativos")]
                       [(= n 0) empty]
                       [(= n 1) '(1)]
                       [(even? n) (append (list-facts-two (- n 2)) (list (factorial n)))]
                       [(not (even? n)) (append (list-facts-two (- n 2)) (list (factorial n)))]
                       )
                  )
  )

;; Pruebas
(list-facts-two 5)
(list-facts-two 8)
;---------------------------------------------------------------------------------------------------
  ;7
;; count-occurrences:
;; Propósito:
;; Procedimiento que recibe como argumento un elemento x
;; y una lista L.
;; Y retorna el numero de ocurrencias del elemento en la lista.

(define count-occurrences (lambda (x L)
                     0
                  )
  )

;; Pruebas
;(count-occurrences 'x '((f x) y (((x z) () x))))
;(count-occurrences 2 '((f x) y (((x 2) x))))
;--------------------------------------------------------------------------------------------------
  ;8
;; flatten:
;; Propósito:
;; Procedimiento que recibe como argumento una lista L
;; Y retorna una lista eliminando los parentesis internos.

(define flatten (lambda (L)
                     (cond
                       [(empty? L) empty]
                       [(list? (car L)) (flatten (car L))]
                       [else (cons (car L) (flatten (cdr L)))]
                       )
                  )
  )

;; Pruebas
;(flatten '((a b) c (((d)) e)));solo evalua la primera lista ****
;--------------------------------------------------------------------------------------------------
  ;9
;; every?:
;; Propósito:
;; Procedimiento que recibir dos argumentos: un predicado P
;; y una lista L.
;; Y retorna #t si TODOS los elementos
;; de la lista L satisfacen el predicado P.
;; Devuelve #f en caso contrario.

(define every? (lambda (P L)
                     (if (empty? L)
                         #t                     
                         (and (P (car L)) (every? P (cdr L)))                           
                         )
                  )
  )

;; Pruebas
(every? symbol? '(a b c 3 e))
(every? number? '(1 2 3 5 4))
;----------------------------------------------------------------------------------------------------
|#

  ;10. Elabore una función llamada upside-down que recibe como argumento un número n y devuelve el numero
;;en el orden inverso al que se recibió.

(define carac (lambda (c)
                (cond
                  [(empty? c) ""]
                  [else (string-append (string (car c)) (carac (cdr c)))]
                )))

(define upside-down (lambda (n)
                     (if (number? n)                                           
                         (string->number (string-append (upside-down (string->number (carac (cdr (string->list (number->string n))))))
                                 (string (car (string->list (number->string n)))))                                  
                         )
                         ""
                  )
  ))
  
;; (string->number (string (car (string->list (number->string 12345)))))
;; Pruebas
 (upside-down 513)
 (upside-down 9513)
;; 3159







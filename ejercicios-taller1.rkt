#lang eopl

;;JUAN SEBASTIAN GRAJALES CRUZ - 202010004
;;DUVAN HERNANDEZ FIGUEROA     - 202010009
;;DIEGO FERNANDO MUÑOZ ARCE    - 202010032
;----------------------------------------------------------------------------
;;1.
;; copy :
;; Propósito:
;; Procedimiento que recibe un número n y una entrda x
;; y retorna una lista con n ocurrencias de x.

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
;;2.
;; list-tails:
;; Propósito:
;; Procedimiento que recibe como argumento una lista L
;; y retorna en una lista todas las
;; sublistas de los elementos consecutivos de la lista L.

(define list-tails (lambda (L)
                     (if (null? L)
                        empty
                        (cons L (list-tails (cdr L)))
                        )
                     )
  )

;; Pruebas
(list-tails '(1 2 3 4 5))
(list-tails '(1 a (e 4) 5 v))
;------------------------------------------------------------------------------
;;3.
;; sublist:
;; Propósito:
;; Procedimiento que recibe como argumento una lista L,
;; un núumero inicial i y un núumero final j.
;; y retorna la sublista entre el elemento en posición i y
;; el elemento en posición j (ambos elementos incluidos).

(define sublist (lambda (L i j)
                     (if (or (null? L) (> i j))
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
;;4.
;; exists?:
;; Propósito:
;; Procedimiento que recibe dos argumentos:
;; un predicado P y una lista L, y retorna
;; #t si algúun elemento de la lista L satisface el predicado P.
;; Devuelve #f en caso contrario.

(define exists? (lambda (P L)
                     (cond
                       [(null? L) #f]
                       [(P (car L)) #t]             
                       [else (exists? P (cdr L))]
                     )
                  )
  )

;; Pruebas
(exists? null? '(a b c c e))
(exists? symbol? '(a b c d 4))
;----------------------------------------------------------------------------------
;;5.
;; list-fibo:
;; Propósito:
;; Procedimiento que recibe como argumento un numero entero n,
;; y retorna una lista ascendente con los n-términos de la
;; sucesión fibonacci.

(define list-fibo (lambda (n)
                     (letrec ([fibonacci (lambda (n)
                                            (cond
                                              [(= n 0) 0]
                                              [(= n 1) 1]
                                              [else (+ (fibonacci (- n 2)) (fibonacci (- n 1)))]
                                              )
                                            )
                                          ])
                              (cond
                                [(= n 0) '(0)]
                                [else (append (list-fibo (- n 1)) (list (fibonacci n)))]
                                )
                              )
                  )
  )

;; Pruebas
(list-fibo 1)
(list-fibo 6)
;----------------------------------------------------------------------------------
;;6.
;; factorial:
;; Propósito:
;; Procedimiento que recibe como argumento un numero entero n,
;; y retorna el factorial del número n

(define factorial (lambda (n)
                     (cond
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
;;7.
;; count-occurrences:
;; Propósito:
;; Procedimiento que recibe como argumento un elemento x
;; y una lista L.
;; Y retorna el numero de ocurrencias del elemento en la lista.

(define count-occurrences (lambda (x L)
                     (cond
                       [(null? L) 0]                       
                       [else (+
                              (if (list? (car L))
                                  (count-occurrences x (car L))
                                  (if (equal? x (car L))
                                    1
                                    0)
                                  )                            
                              (count-occurrences x (cdr L)))]
                       )
                  )
  )

;; Pruebas
(count-occurrences 'x '(x 3 x r 2 x))
(count-occurrences 'x '((f x) y (((x z) () x))))
(count-occurrences 2 '((f x) y (((x 2) x))))
(count-occurrences 'x '((f x x) y (((x x z) ((x) x) x))))

;--------------------------------------------------------------------------------------------------
;;8.
;; flatten:
;; Propósito:
;; Procedimiento que recibe como argumento una lista L
;; Y retorna una lista eliminando los parentesis internos.

(define flatten (lambda (L)
                     (cond
                       [(null? L) empty]                       
                       [else (append
                              (if (list? (car L))
                                  (flatten (car L))
                                  (list(car L))
                                  )                            
                              (flatten (cdr L))
                              )]
                       )
  ))

;; Pruebas
(flatten '((a b) c (((d)) e)))
(flatten '((a) () (2 ()) () (c)))
;--------------------------------------------------------------------------------------------------
;;9.
;; every?:
;; Propósito:
;; Procedimiento que recibe dos argumentos: un predicado P
;; y una lista L.
;; Y retorna #t si TODOS los elementos
;; de la lista L satisfacen el predicado P.
;; Devuelve #f en caso contrario.

(define every? (lambda (P L)
                     (if (null? L)
                         #t                     
                         (and (P (car L)) (every? P (cdr L)))                           
                         )
                  )
  )

;; Pruebas
(every? symbol? '(a b c 3 e))
(every? number? '(1 2 3 5 4))
;----------------------------------------------------------------------------------------------------

;;10.
;; splitnumber:
;; Propósito: Función que recibe un número n y retorna un string con
;; el número n inverso.
(define splitnumber (lambda (n)
                      (if(< n 10)
                         (number->string n)
                         (string-append (number->string (remainder n 10))  (splitnumber (quotient n 10)))
                         ) 
                      )
  )
;; pruebas
(splitnumber 432)
(splitnumber 9513)

;; upside-down:
;; Propósito:
;; Procedimiento que recibe como argumento un número n.
;; y devuelve el numero en el orden inverso al que se recibió.

(define upside-down (lambda (n)
                      (string->number (splitnumber n))                         
                      )
  )

;; Pruebas
 (upside-down 432)
 (upside-down 9513)

;;-------------------------------------------------------------------
;; 11.
;; merge:
;; Propósito: función que recibe como entrada dos listas
;; de enteros ordenadas ascendentemente L1 y L2.
;; Y retorna una lista ordenada de todos los elementos de las listas L1 y L2.

(define merge (lambda (L1 L2)
                        (cond
                          [(null? L1) L2]
                          [(null? L2) L1]
                          [(<= (car L1) (car L2)) (cons  (car L1) (merge (cdr L1) L2))]
                          [else (cons  (car L2) (merge L1 (cdr L2)))]
                          )
                      )
  )
;;pruebas
(merge '(1 4) '(1 2 8))
(merge '(35 62 81 90 91) '(3 83 85 90))

;;--------------------------------------------------------
;;12.
;; zip:
;; Propósito: función que recibe como entrada tres parámetros:
;; una función binaria (función que espera recibir dos argumentos) F, y dos listas
;; L1 y L2, ambas de igual tamaño.
;; Y retorna una lista donde la posición n-énesima corresponde al resultado de aplicar la
;; función F sobre los elementos en la posición n-énesima en L1 y L2.

(define zip (lambda (F L1 L2)
                        (cond
                         [(null? L1) empty]
                         [else (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2)))]
                         )
                      )
  )
;;pruebas
(zip + '(1 4) '(6 2))
(zip * '(11 5 6) '(10 9 8))

;;--------------------------------------------------------
;;13.
;; filter-acum:
;; Propósito: función que recibe como entrada 5 parámetros: dos números a y b,
;; una función binaria F, un valor inicial acum y una función unaria filter.
;; Y retorna acum aplicando función binaria F en el intervalo de a y b si cumple con la
;; función unaria filter.

(define filter-acum (lambda (a b F acum filter)
                        (cond
                        [(> a b) acum]
                        [(filter a) (F acum a (filter-acum (+ a 1) b F acum filter))]
                        [else (filter-acum (+ a 1) b F acum filter)]
                        
                        )
                      )
  )
;;pruebas
(filter-acum 1 10 + 0 odd?)
(filter-acum 1 10 + 0 even?)

;--------------------------------------------------------------
;;14.


;; eliminar:
;; Propósito: Funcion auxiliar que recibe como argumento un numero n y una lista L,
;; y retorna la lista sin ese elemento n.

(define eliminar (lambda (n L)
                   (cond
                     [(null? L) empty]
                     [(eq? n (car L)) (cdr L)]
                     [else (cons (car L) (eliminar n (cdr L)))]
                     )
                   )
  )

;;pruebas
(eliminar 2 '(2 4))
(eliminar 3 '(5 3 2))

;; organizar:
;; Propósito: Funcion Auxiliar que recibe una lista L y una funcion de comparación
;; F y retorna el numero que cumpla con la funcion de comparación F

(define organizar (lambda (L F)
                    (cond
                      [(null? L) empty]
                      [(null? (cdr L)) (car L)]
                      [(F (car L) (car (cdr L))) (organizar (cons (car L) (cddr L)) F)]
                      [else (organizar (cdr L) F)]
                      )
                    )
  )

;;pruebas
(organizar '(3 4 2 1) <)
(organizar '(2 4 7 6) >)

;; sort:
;; Propósito: función que recibe como entrada dos argumentos:
;; una lista de elementos L y una función de comparación F.
;; Y retorna la lista L ordenada aplicando la función de comparación F.

(define sort (lambda (L F)
               (cond
                 [(null? L) empty]
                 [else (cons (organizar L F) (sort (eliminar (organizar L F) L) F))])
               )
  )

;;pruebas
(sort '(8 2 5 2 3) <)
(sort '(8 2 5 2 3) >)
(sort '("a" "c" "bo" "za" "lu") string>?)

;--------------------------------------------------------------------------
;;15.
;; hermite:
;; Propósito: función que recibe como entrada dos argumentos:
;; el orden n del polinomio de Hermite y la abscisa x.
;; Y retorna el resultado del calculo. 

(define hermite (lambda (n x)
                  (cond
                    [(= n 0) 1]
                    [(= n 1) (* 2 x)]
                    [else (- (* (* 2 x) (hermite (- n 1) x)) (* (* 2 (- n 1)) (hermite (- n 2) x)))]
                    )
                  )  
  )
;;pruebas
(hermite 5 2)
(hermite 5 8)

;---------------------------------------------------------------------------
;16.
;; bubble-sort:
;; Propósito: función que recibe como entrada una lista
;; de números L.
;; Y retorna la lista L ordenada de manera ascendente.

(define bubble-sort (lambda (L)
                      (letrec ([f1 (lambda (L)
                                     (cond
                                       [(null? (cdr L)) L]
                                       [(<= (car L) (cadr L)) (cons (car L) (f1 (cdr L)))]
                                       [else (f1 (cons (cadr L) (cons (car L) (cddr L))))]
                                       )
                                     )]
                               [f2 (lambda (n F L)
                                     (cond
                                       [(= n 0) L]
                                       [else (f2 (- n 1) F (F L))]
                                       )
                                     )])
                        (f2 (- (length L) 1) f1 L)
                      )) 
  )
;;pruebas
(bubble-sort '(17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
(bubble-sort '(5 4 3 2 1))
(bubble-sort '(8 7 2 4 3))

;----------------------------------------------------------------
;17.
;; path:
;; Propósito: Funcipón recibe como entrada dos parámetros:
;; un número n y un árbol binario de búsqueda.
;; Y retorna una lista con la ruta a tomar (iniciando desde el nodo raíz), indicada
;; por cadenas left y right, hasta llegar al número n recibido.
;; Si el número n es encontrado en el nodo raíz,
;; el procedimiento debe retornar una lista vacía.

(define path (lambda (n BST)
               (cond
                 [(null? BST) empty]
                 [(= n (car BST)) empty]
                 [(< n (car BST)) (cons  'left (path n (cadr BST)))]
                 [(> n (car BST)) (cons 'right (path n (caddr BST)))]
                 )
               ) 
  )
;;pruebas
(path 13 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
(path 17 '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ()))))
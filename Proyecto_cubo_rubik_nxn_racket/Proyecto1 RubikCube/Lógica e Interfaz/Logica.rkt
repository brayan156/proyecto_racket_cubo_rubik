#lang racket


(define cubo4x4 '(((y y y y) (y y y y) (y y y y) (y y y y)) ((a a a a) (a a a a) (a a a a) (a a a a)) ((r n v y) (a r y v) (r y r v) (v r a y)) ((v y a a) (a y y r) (n r v v) (a y y r)) ((r r r r) (r r r r) (r r r r) (r r r r)) ((v y a a) (a y y r) (n r v v) (a y y r))))
(define cubo6x6 '(((v y a a v v) (a y y r v a) (a y y r r y) (a y y r v a) (n r v y v v) (a y y r v a)) ((a y v r a a) (a y y r v a) (r n v y v r) (a y y r v a) (r v r n v y) (r n r y v y)) ((r n v v n y) (a r y r r v) (a r y r r v) (r y n r v n) (a r y r r v) (v r a y r a)) ((r n v v n y) (a r y r r v) (a r y r r v) (r y n r v n) (a r y r r v) (v r a y r a)) ((r n v v n y) (a r y r r v) (a r y r r v) (r y n r v n) (a r y r r v) (v r a y r a)) ((r n v v n y) (a r y r r v) (a r y r r v) (r y n r v n) (a r y r r v) (v r a y r a))))
(define cubo '(((v y a) (n r v) (a y y)) ((a y a) (n v y) (n v y)) ((a r y) (r y r) (v r a)) ((n n n) (n n n) (n n n)) ((v v v) (v v v) (v v v)) ((y y y) (y y y) (y y y))))
(define cubow '(((a a a) (a a a) (a a a)) ((r r r) (r r r) (r r r)) ((y y y) (y y y) (y y y)) ((n n n) (n n n) (n n n)) ((v v v) (v v v) (v v v)) ((b b b) (b b b) (b b b))))
(require "Interfaz.rkt")

;(RS 3 '(((v y a) (n r v) (a y y))
        ;((a y a) (n v y) (n v y))
       ; ((a r y) (r y r) (v r a))
        ;;((n n n) (n n n) (n n n))
        ;((v v v) (v v v) (v v v))
       ; ((y y y) (y y y) (y y y)))
       ; '("F2D" "F1I" "C1B"))

;(RS 3 cubo '("F2D" "C3A" "F1I" "C2B"))
;(RS 4 cubo4x4 '("F2D"))

(define(get x lista) ;obtener un elemento en la pocisión x de una lista
  (cond ((equal? x 1)
         (car lista))
        {else (get (- x 1) {cdr lista} )}
   )
)

(define(eliminar_indice x lista) ;eliminar un elemento en la pocisión x de una lista
  (cond ((equal? lista (list ))
         (list ))
        ((equal? x 1)
         (eliminar_indice (- x 1) {cdr lista} ))
        {else (cons {car lista} (eliminar_indice (- x 1) {cdr lista} ))}
   )
)

(define (alfinal dato lista) ;añade un elemento al final de la lista
  (cond ((null? lista)
         (list dato))
        (else
         (cons (car lista) (alfinal dato (cdr lista))))
        )
 )

(define (poner_adelante  dato x lista);añade un elemento adelante de la posición x de la lista
    (cond ((equal? lista (list ))
           (cond ((equal? x 0)
                  (list dato))
           (else (list ))))
        ((equal? x 0)
         (cons dato (poner_adelante  dato (- x 1) lista )))
        {else (cons {car lista} (poner_adelante dato (- x 1)  {cdr lista} ))}
   )
  )

(define (poner_atras dato x lista) ;añade un elemento atras de la posición x de la lista
    (cond ((equal? lista (list ))
         (list ))
        ((equal? x 1)
         (cons dato (poner_atras dato (- x 1) lista )))
        {else (cons {car lista} (poner_atras dato (- x 1)  {cdr lista} ))}
   )
 )

(define (editar dato pos lista) ;cambia el valor del dato en la posición pos de la lista
    (cond ((equal? lista (list ))
         (list ))
        ((equal? pos 1)
         (cons dato (editar dato (- pos 1) {cdr lista} )))
        {else (cons {car lista} (editar dato (- pos 1) {cdr lista} ))}
   )
 )

(define (invertir lista);invierte la lista
  (cond ((null? lista)
         (list ))
        (else
         (append (invertir (cdr lista)) (list (car lista))))
        )
  )
 
(define (rotar_adelante i f matriz) ;rota una matriz de i a f en dirección de las manecillas del reloj
(rotar_izquierda_a i f (+ i 1) (rotar_abajo_a i f i (rotar_derecha_a f (+ i 1) (rotar_arriba_a i f (+ i 1) matriz)))))


(define (rotar_arriba_a i f p matriz) ;funciones creadas para para movilizar por partes filas y columnas del cubo hasta dar la vuelta completa
  (cond ((equal? p (+ f 1))           ;tienen orden de arriba-derecha-abajo-izquierda desde el primero al ultimo paso que se debe realizar
         matriz)                      ;y cada una mueve los datos a trasladar de cada fila o columna hacia la columna o fila del frente en direccion horaria
  (else (rotar_arriba_a i f (+ p 1) (mover_ele_matriz_adelante i (+ i 1) f p matriz))))
  )
(define (rotar_derecha_a f p matriz)
  (cond ((equal? p f)
         matriz) 
  (else (rotar_derecha_a f (+ p 1) (mover_ele_matriz_adelante p f f f matriz))))
  )
(define (rotar_abajo_a i f p matriz)
  (cond ((equal? p f)
         matriz) 
  (else (rotar_abajo_a i f (+ p 1) (mover_ele_matriz_atras f i i p matriz))))
  ) 
(define (rotar_izquierda_a i f p matriz)
  (cond ((equal? p f)
         matriz) 
  (else (rotar_izquierda_a  i f (+ p 1) (mover_ele_matriz_adelante p (+ i 1) i i matriz))))
  )



  
(define (rotar_atras i f matriz) ;rota una matriz de i a f en dirección contraria de las manecillas del reloj
(rotar_derecha_at i f  (- f 1) (rotar_abajo_at i f (- f 1) (rotar_izquierda_at (+ i 1) f (- f 1) (rotar_arriba_at i f (- f 1) matriz))))  
)
(define (rotar_arriba_at i f p matriz);funciones creadas para para movilizar por partes filas y columnas del cubo hasta dar la vuelta completa
  (cond ((equal? p (- i 1))           ;tienen orden de arriba-izquierda-abajo-derecha desde el primero al ultimo paso que se debe realizar
         matriz)                      ;y cada una mueve los datos a trasladar de cada fila o columna hacia la columna o fila de atras en direccion anti-horaria
  (else (rotar_arriba_at i f (- p 1) (mover_ele_matriz_atras i p i (- (+ i f) p) matriz))))
  )
(define (rotar_derecha_at i f p matriz)
  (cond ((equal? p i)
         matriz) 
  (else (rotar_derecha_at i f (- p 1) (mover_ele_matriz_adelante p f i i matriz))))
  )
(define (rotar_abajo_at i f p matriz)
  (cond ((equal? p (- i 1))
         matriz)
        ((equal? p i)
         (rotar_abajo_at i f (- p 1) (mover_ele_matriz_adelante f (+ f 1) i p matriz)))
  (else (rotar_abajo_at i f (- p 1) (mover_ele_matriz_adelante f (+ f 1) f p matriz))))
  ) 
(define (rotar_izquierda_at i f p matriz)
  (cond ((equal? p (- i 1))
         matriz) 
  (else (rotar_izquierda_at  i f (- p 1) (mover_ele_matriz_atras p i i f matriz))))
  )



(define (mover_ele_matriz_adelante fila columna columnafin filafin  matriz);mueve un elemento de la matriz en la posición i a la posicion f de la matriz y lo coloca en frente
  (editar (poner_adelante (get columna (get fila matriz)) columnafin (get filafin matriz)) filafin (editar (eliminar_indice columna (get fila matriz)) fila matriz))
  )
(define (mover_ele_matriz_atras fila columna columnafin filafin  matriz);mueve un elemento de la matriz en la posición i a la posicion f de la matriz y lo coloca atras
  (editar (poner_atras (get columna (get fila matriz)) columnafin (get filafin matriz)) filafin (editar (eliminar_indice columna (get fila matriz) ) fila matriz))
  )

(define (get_columna x matriz) ;obtiene una lista de la columna de una matriz en la posición x
  (cond ((equal? matriz (list ))
                (list ))
        (else (cons (get x (car matriz)) (get_columna x (cdr matriz))))
   )
)


(define (editar_columna datos x matriz) ;edita los valores de una columna en la posición x de la matriz
  (cond ((equal? matriz (list ))
                (list ))
        (else (cons (editar (car datos) x (car matriz)) (editar_columna (cdr datos) x (cdr matriz))))
   )
  )


(define (rotar_cara_atras i f matriz) ;rota todas las partes de i a f de una matriz incluyendo las internas en direccion contraria de las manecillas del reloj
  (cond ((>= i f)
         matriz)
        {else(rotar_cara_atras (+ i 1) (- f 1) (rotar_atras i f matriz))})
  )


(define (rotar_cara_adelante i f matriz);rota todas las partes de i a f de una matriz incluyendo las internas en direccion de las manecillas del reloj
  (cond ((>= i f)
         matriz)
        {else(rotar_cara_adelante (+ i 1) (- f 1) (rotar_adelante i f matriz))})
) 

(define (rotar_f x dir cubo n) ;simula el mover una fila n en un cubo en una dirección D derecha o I izquierda
  (cond ((equal? dir "D")
         (cond ((equal? x 1)
           (AnimarCaras12Aux (get x (car ( rotar_derecha_cubo x   (editar (rotar_cara_atras 1 n (get 3 cubo)) 3 cubo)))) (get x (cadr ( rotar_derecha_cubo x   (editar (rotar_cara_atras 1 n (get 3 cubo)) 3 cubo)))) 250 (+ 200 (* (- x 1) 52)) (+ 250 (* n 52)) (- (+ 200 (* (- x 1) 52)) 25) 0 1 0)
           ;(DibujarCara3 ( rotar_derecha_cubo x   (editar (rotar_cara_atras 1 n (get 3 cubo)) 3 cubo)) (caddr ( rotar_derecha_cubo x   (editar (rotar_cara_atras 1 n (get 3 cubo)) 3 cubo))) x posx posy)
            (DibujarCara1 ( rotar_derecha_cubo x   (editar (rotar_cara_atras 1 n (get 3 cubo)) 3 cubo)) (* n n) n n (car ( rotar_derecha_cubo x   (editar (rotar_cara_atras 1 n (get 3 cubo)) 3 cubo))) 250 200)
           ( rotar_derecha_cubo x   (editar (rotar_cara_atras 1 n (get 3 cubo)) 3 cubo))
         )
          ((equal? x n)
           
           (AnimarCaras12Aux (get x (car ( rotar_derecha_cubo x   (editar (rotar_cara_adelante 1 n (get 6 cubo)) 6 cubo)))) (get x (cadr ( rotar_derecha_cubo x   (editar (rotar_cara_adelante 1 n (get 6 cubo)) 6 cubo)))) 250 (+ 200 (* (- x 1) 52)) (+ 250 (* n 52)) (- (+ 200 (* (- x 1) 52)) 25) 0 1 0)
           ( rotar_derecha_cubo x   (editar (rotar_cara_adelante 1 n (get 6 cubo)) 6 cubo))
           )
          
          (else        
               
               (AnimarCaras12Aux (get x (car (rotar_derecha_cubo x cubo))) (get x (cadr (rotar_derecha_cubo x cubo))) 250 (+ 200 (* (- x 1) 52)) (+ 250 (* n 52)) (- (+ 200 (* (- x 1) 52)) 25) 0 1 0)
               (rotar_derecha_cubo x cubo)
         )))
        ((equal? dir "I")
         (cond ((equal? x 1)
            ( rotar_izquierda_cubo x     (editar (rotar_cara_adelante 1 n (get 3 cubo)) 3 cubo))
         )((equal? x n)
           
          
          (AnimarCaras12Aux (get x (car (rotar_izquierda_cubo x  (editar (rotar_cara_atras 1 n (get 6 cubo)) 6 cubo)))) (get x (cadr (rotar_izquierda_cubo x  (editar (rotar_cara_atras 1 n (get 6 cubo)) 6 cubo)))) (+ 250 (* (- n 1) 52)) (+ 200 (* (- x 1) 52)) (+ (+ 250 (* n 52)) (* (- n 1) 22))  (- (+ 200 (* (- x 1) 52)) (* 25 (- x 1)) 25) 0 0 1)
          (rotar_izquierda_cubo x  (editar (rotar_cara_atras 1 n (get 6 cubo)) 6 cubo))
          )(else
               
               
               (AnimarCaras12Aux (get x (car (rotar_izquierda_cubo x cubo))) (get x (cadr (rotar_izquierda_cubo x cubo))) (+ 250 (* (- n 1) 52)) (+ 200 (* (- x 1) 52)) (+ (+ 250 (* n 52)) (* (- n 1) 22)) (+ (+ (+ (+ (+ (+ (- 200 (* n 25)) (* x 25)) x) (* 25 (- x n))) 50) (* 1 (- x 2))) (* (- n 4) 25))  0 0 1)
               (rotar_izquierda_cubo x cubo)
               )
         )
        )
        
        (else (print "notación incorrecta use D e I para la dirección de las filas"))
  )
        
)

(define (rotar_c x dir cubo n);simula el mover una columna n en un cubo en una dirección A arriba O B abajo (+ (- (+ 200 (* (- x 2) 52)) (* 25  x)) 25)
  (cond ((equal? dir "A")
         (cond ((equal? x 1)
                (DibujarCara1 ( rotar_arriba_cubo x (editar (rotar_cara_atras 1 n (get 5 cubo)) 5 cubo) n) (* n n) n n (car (rotar_arriba_cubo x (editar (rotar_cara_atras 1 n (get 5 cubo)) 5 cubo) n)) 250 200)
            ( rotar_arriba_cubo x (editar (rotar_cara_atras 1 n (get 5 cubo)) 5 cubo) n)
         )((equal? x n)
           (DibujarCara1 ( rotar_arriba_cubo x  (editar (rotar_cara_adelante 1 n (get 2 cubo)) 2 cubo) n) (* n n) n n (car ( rotar_arriba_cubo x  (editar (rotar_cara_adelante 1 n (get 2 cubo)) 2 cubo) n)) 250 200)
          ( rotar_arriba_cubo x  (editar (rotar_cara_adelante 1 n (get 2 cubo)) 2 cubo) n)
          
         )(else
               (DibujarCara1 ( rotar_arriba_cubo x cubo n) (* n n) n n (car ( rotar_arriba_cubo x cubo n)) 250 200)
               ( rotar_arriba_cubo x cubo n))
         )
         )
        ((equal? dir "B")
         (cond ((equal? x 1)
                (DibujarCara1 ( rotar_abajo_cubo x (editar (rotar_cara_adelante 1 n (get 5 cubo)) 5 cubo) n) (* n n) n n (car ( rotar_abajo_cubo x (editar (rotar_cara_adelante 1 n (get 5 cubo)) 5 cubo) n)) 250 200)
            ( rotar_abajo_cubo x (editar (rotar_cara_adelante 1 n (get 5 cubo)) 5 cubo) n)
         ) ((equal? x n)
            (DibujarCara1 ( rotar_abajo_cubo x (editar (rotar_cara_atras 1 n (get 2 cubo)) 2 cubo) n) (* n n) n n (car ( rotar_abajo_cubo x (editar (rotar_cara_atras 1 n (get 2 cubo)) 2 cubo) n)) 250 200)
          ( rotar_abajo_cubo x (editar (rotar_cara_atras 1 n (get 2 cubo)) 2 cubo) n)
         ) (else
            (DibujarCara1 ( rotar_abajo_cubo x cubo n) (* n n) n n (car ( rotar_abajo_cubo x cubo n)) 250 200)
              ( rotar_abajo_cubo x cubo n) )
         )
         )
        
        (else (print "notación incorrecta use A e B para la dirección de las columnas"))
  )
)

(define (rotar_derecha_cubo x cubo) ;simula el moviento hacia la derecha de una fila moviendo datos de las matrices
         (editar (editar (get x (get 5 cubo)) x  (get 1 cubo)) 1 
         (editar (editar (get x (get 4 cubo)) x  (get 5 cubo)) 5 
         (editar (editar (get x (get 2 cubo)) x  (get 4 cubo)) 4 
         (editar (editar (get x (get 1 cubo)) x  (get 2 cubo)) 2 cubo)
          )
          )
          )
 )

(define (rotar_izquierda_cubo x cubo);simula el moviento hacia la izquierda de una fila moviendo datos de las matrices
         (editar (editar (get x (get 2 cubo)) x  (get 1 cubo)) 1 
         (editar (editar (get x (get 4 cubo)) x  (get 2 cubo)) 2 
         (editar (editar (get x (get 5 cubo)) x  (get 4 cubo)) 4 
         (editar (editar (get x (get 1 cubo)) x  (get 5 cubo)) 5 cubo)
          )
          )
          )
  )
(define (rotar_arriba_cubo x cubo n);simula el moviento hacia arriba de una fila moviendo datos de las matrices
         (editar (editar_columna (get_columna x (get 6 cubo)) x  (get 1 cubo)) 1 
         (editar (editar_columna (invertir (get_columna (- n (- x 1)) (get 4 cubo))) x  (get 6 cubo)) 6 
         (editar (editar_columna (invertir (get_columna x (get 3 cubo))) (- n (- x 1))  (get 4 cubo)) 4 
         (editar (editar_columna (get_columna x (get 1 cubo)) x  (get 3 cubo)) 3 cubo)
          )
          )
          )
  )
(define (rotar_abajo_cubo x cubo n);simula el moviento hacia abajo de una fila moviendo datos de las matrices
         (editar (editar_columna (get_columna x (get 3 cubo)) x  (get 1 cubo)) 1 
         (editar (editar_columna (invertir (get_columna (- n (- x 1)) (get 4 cubo))) x  (get 3 cubo)) 3 
         (editar (editar_columna (invertir (get_columna x (get 6 cubo))) (- n (- x 1))  (get 4 cubo)) 4 
         (editar (editar_columna (get_columna x (get 1 cubo)) x  (get 6 cubo)) 6 cubo)
          )
          )
          )
  )


(define (es_int x);verifica que el valor en String sea un numero
  (cond ((equal? (string->number x) #false)
         #false)
        (else #true)
    )
  )

(define (RS x cubo movs);función principal del juego x el tamaño del cubo, cubo son el conjunto de matrices del cubo de tamaño x y movs la lista de movimientos del cubo
  (cond((integer? x)
        (cond((es_len_fila 6 cubo)
              (cond((es_len_fila x (car cubo))
                    (cond((es_len_columna x (car cubo))
                          (RX x cubo)
                          (sleep 2)
                          (realizar_movimientos x cubo movs)
                          )(else (print "numero de columnas incorrecta o dimension del cubo incorrecta"))
                         ))(else (print "numero de filas incorrecta o dimension del cubo incorrecta"))
             ))(else (print "numero de caras incorrecta"))
             ))(else (print "x debe ser un numero"))
       )
  )

(define (realizar_movimientos x cubo movs);va mandando a realiar cada movimiento del cubo en el orden dado
  (cond((equal? movs (list))
        cubo)
       (else
        (realizar_movimientos x (realizar_mov x cubo (car movs)) (cdr movs))
        )
  )
)

(define (realizar_mov x cubo mov); desglosa y realiza el tipo de movimiento ordenado en mov en el cubo
         (sleep 1)
         (cond((es_int (substring mov 1 (- (string-length mov) 1)))
               (cond((and (<= (string->number(substring mov 1 (- (string-length mov) 1))) x) (> (string->number (substring mov 1 (- (string-length mov) 1))) 0))
         (cond ((equal? (substring mov 0 1) "F")
                (rotar_f (string->number (substring mov 1 (- (string-length mov) 1))) (substring mov (- (string-length mov) 1) (string-length mov)) cubo x))
               ((equal? (substring mov 0 1) "C")
                (rotar_c (string->number (substring mov 1 (- (string-length mov) 1))) (substring mov (- (string-length mov) 1) (string-length mov)) cubo x))
               (else (print "coloque F o C para el tipo de movimiento"))
               ))(else (print "movimiento fuera de rango del cubo"))
                    )
               )
              (else (print "segundo substring debe ser un número"))
              
         )        
)
  
(define (es_len_fila x fila);verifica que la fila sea de tamaño x
  (cond((equal? fila (list))
        (cond((equal? x 0)
              #true)
         (else #false)
       ))
       (else (es_len_fila (- x 1) (cdr fila)))
  )
  )
(define (es_len_columna x matriz); verifica que la columna de una matriz sea de tamaño x
    (cond((equal? matriz (list))
        (cond((equal? x 0)
              #true)
         (else #false)
       ))
       (else (es_len_fila (- x 1) (cdr matriz)))
  )
  )

#lang racket/gui

(require 2htdp/universe)
(require racket/draw)
(require (lib "graphics.ss" "graphics"))
(open-graphics)

(provide RX)
(provide AnimarCaras12Aux)
(provide DibujarCara1)




(define ventana (open-viewport "RubikSimulator" 1200 800))



(define (RX X Cubo)
  ((clear-viewport ventana))
  ((draw-solid-rectangle ventana) (make-posn 0 0) 1200 800 "black")
  (cond ((= X 0) "Debe dar dimensiones del cubo")
        (else (DibujarCara1 Cubo (* X X) X X (car Cubo) 250 200))))



;Funciones_encargadas_de_dibujar_cada_casilla_de_la_cara_frontal_del_cubo

(define (DibujarCara1 Cubo CasillasPorCara Dimension CasillasPorFila Cara posx posy)
  (cond ((null? Cara) (DibujarCara2 Cubo (cadr Cubo) Dimension (+ posx (* Dimension 52)) (- (- posy 25) (* Dimension 52))))
        (else (DibujarCara1Aux Cubo CasillasPorCara Dimension CasillasPorFila Cara (car Cara) posx posy))))

(define (DibujarCara1Aux Cubo CasillasPorCara Dimension CasillasPorFila Cara Fila posx posy)
  (cond ((null? Fila) (DibujarCara1 Cubo CasillasPorCara Dimension CasillasPorFila (cdr Cara) 250 (+ posy 52)))
        ((equal? (car Fila) 'a) ((draw-solid-rectangle ventana) (make-posn posx posy) 50 50 "blue"))
        ((equal? (car Fila) 'b) ((draw-solid-rectangle ventana) (make-posn posx posy) 50 50 "white"))
        ((equal? (car Fila) 'r) ((draw-solid-rectangle ventana) (make-posn posx posy) 50 50 "red"))
        ((equal? (car Fila) 'n) ((draw-solid-rectangle ventana) (make-posn posx posy) 50 50 "green"))
        ((equal? (car Fila) 'v) ((draw-solid-rectangle ventana) (make-posn posx posy) 50 50 "orange"))
        ((equal? (car Fila) 'y) ((draw-solid-rectangle ventana) (make-posn posx posy) 50 50 "yellow")))
  (if (equal? Fila '()) "" (DibujarCara1Aux Cubo (- CasillasPorCara 1) Dimension CasillasPorFila Cara (cdr Fila) (+ posx 52) posy)))

;//////////


(define (DibujarCara2 Cubo Cara Dimension posx posy)
(cond ((null? Cara) (DibujarCara3 Cubo (caddr Cubo) Dimension  (+ (- (- posx (+ 30 Dimension)) (* Dimension 52)) (* Dimension 25)) (- (- (+ posy 25) (* Dimension 52)) (* Dimension 25))))
      (else (DibujarCara2Aux Cubo Cara Dimension (car Cara) posx posy))))
  
(define (DibujarCara2Aux Cubo Cara Dimension Fila posx posy)
  (cond ((null? Fila) (DibujarCara2 Cubo (cdr Cara) Dimension (- posx (* 22 Dimension)) (+ (+ posy 52) (* 25 Dimension))))
        ((equal? (car Fila) 'a) ((draw-solid-polygon ventana) (list (make-posn 0 25) (make-posn 20 0) (make-posn 20 50) (make-posn 0 75)) (make-posn posx posy) "blue"))
        ((equal? (car Fila) 'b) ((draw-solid-polygon ventana) (list (make-posn 0 25) (make-posn 20 0) (make-posn 20 50) (make-posn 0 75)) (make-posn posx posy) "white"))
        ((equal? (car Fila) 'r) ((draw-solid-polygon ventana) (list (make-posn 0 25) (make-posn 20 0) (make-posn 20 50) (make-posn 0 75)) (make-posn posx posy) "red"))
        ((equal? (car Fila) 'n) ((draw-solid-polygon ventana) (list (make-posn 0 25) (make-posn 20 0) (make-posn 20 50) (make-posn 0 75)) (make-posn posx posy) "green"))
        ((equal? (car Fila) 'v) ((draw-solid-polygon ventana) (list (make-posn 0 25) (make-posn 20 0) (make-posn 20 50) (make-posn 0 75)) (make-posn posx posy) "orange"))
        ((equal? (car Fila) 'y) ((draw-solid-polygon ventana) (list (make-posn 0 25) (make-posn 20 0) (make-posn 20 50) (make-posn 0 75)) (make-posn posx posy) "yellow")))
        
  (if (equal? Fila '()) "" (DibujarCara2Aux Cubo Cara Dimension (cdr Fila) (+ posx 22) (- posy 25))))

;///////////////////////////


(define (DibujarCara3 Cubo Cara Dimension posx posy)
(cond ((null? Cara) "")
      (else (DibujarCara3Aux Cubo Cara Dimension (car Cara) posx posy))))
  
(define (DibujarCara3Aux Cubo Cara Dimension Fila posx posy)
  (cond ((null? Fila) (DibujarCara3 Cubo (cdr Cara) Dimension (- (- posx 22) (* Dimension 52)) (+ posy 24)))
        ((equal? (car Fila) 'a) ((draw-solid-polygon ventana) (list (make-posn 0 22) (make-posn 50 22) (make-posn 70 0) (make-posn 20 0)) (make-posn posx posy) "blue"))
        ((equal? (car Fila) 'b) ((draw-solid-polygon ventana) (list (make-posn 0 22) (make-posn 50 22) (make-posn 70 0) (make-posn 20 0)) (make-posn posx posy) "white"))
        ((equal? (car Fila) 'r) ((draw-solid-polygon ventana) (list (make-posn 0 22) (make-posn 50 22) (make-posn 70 0) (make-posn 20 0)) (make-posn posx posy) "red"))
        ((equal? (car Fila) 'n) ((draw-solid-polygon ventana) (list (make-posn 0 22) (make-posn 50 22) (make-posn 70 0) (make-posn 20 0)) (make-posn posx posy) "green"))
        ((equal? (car Fila) 'v) ((draw-solid-polygon ventana) (list (make-posn 0 22) (make-posn 50 22) (make-posn 70 0) (make-posn 20 0)) (make-posn posx posy) "orange"))
        ((equal? (car Fila) 'y) ((draw-solid-polygon ventana) (list (make-posn 0 22) (make-posn 50 22) (make-posn 70 0) (make-posn 20 0)) (make-posn posx posy) "yellow")))
        
  (if (equal? Fila '()) "" (DibujarCara3Aux Cubo Cara Dimension (cdr Fila) (+ posx 52) posy)))

;Funciones encargadas de la animacion de cara 1 y 2, las filas

(define (AnimarCaras12Aux Fila1 Fila2 posx1 posy1 posx2 posy2 estado derecha izquierda)
  (print posy1)
  (print posy2)
  (cond ((equal? estado 0)
         (sleep 0.3)
         (cond ((null? Fila1) "")
               ((equal? (car Fila1) 'a) ((draw-solid-rectangle ventana) (make-posn posx1 posy1) 50 50 "blue"))
               ((equal? (car Fila1) 'b) ((draw-solid-rectangle ventana) (make-posn posx1 posy1) 50 50 "white"))
               ((equal? (car Fila1) 'r) ((draw-solid-rectangle ventana) (make-posn posx1 posy1) 50 50 "red"))
               ((equal? (car Fila1) 'n) ((draw-solid-rectangle ventana) (make-posn posx1 posy1) 50 50 "green"))
               ((equal? (car Fila1) 'v) ((draw-solid-rectangle ventana) (make-posn posx1 posy1) 50 50 "orange"))
               ((equal? (car Fila1) 'y) ((draw-solid-rectangle ventana) (make-posn posx1 posy1) 50 50 "yellow")))
         
         (cond ((equal? derecha 0) (if (equal? Fila1 '()) "" (AnimarCaras12Aux (cdr Fila1) Fila2 (- posx1 52) posy1 posx2 posy2 1 derecha izquierda)))
               (else (if (equal? Fila1 '()) "" (AnimarCaras12Aux (cdr Fila1) Fila2 (+ posx1 52) posy1  posx2 posy2 1 derecha izquierda)))))
  (else (cond ((null? Fila2) "")
              ((equal? (car Fila2) 'a) ((draw-solid-polygon ventana) (list (make-posn 0 25) (make-posn 20 0) (make-posn 20 50) (make-posn 0 75)) (make-posn posx2 posy2) "blue"))
              ((equal? (car Fila2) 'b) ((draw-solid-polygon ventana) (list (make-posn 0 25) (make-posn 20 0) (make-posn 20 50) (make-posn 0 75)) (make-posn posx2 posy2) "white"))
              ((equal? (car Fila2) 'r) ((draw-solid-polygon ventana) (list (make-posn 0 25) (make-posn 20 0) (make-posn 20 50) (make-posn 0 75)) (make-posn posx2 posy2) "red"))
              ((equal? (car Fila2) 'n) ((draw-solid-polygon ventana) (list (make-posn 0 25) (make-posn 20 0) (make-posn 20 50) (make-posn 0 75)) (make-posn posx2 posy2) "green"))
              ((equal? (car Fila2) 'v) ((draw-solid-polygon ventana) (list (make-posn 0 25) (make-posn 20 0) (make-posn 20 50) (make-posn 0 75)) (make-posn posx2 posy2) "orange"))
              ((equal? (car Fila2) 'y) ((draw-solid-polygon ventana) (list (make-posn 0 25) (make-posn 20 0) (make-posn 20 50) (make-posn 0 75)) (make-posn posx2 posy2) "yellow")))
        (cond ((equal? izquierda 1) (if (equal? Fila2 '()) "" (AnimarCaras12Aux Fila1 (cdr Fila2) posx1 posy1 (- posx2 22) (+ posy2 25) 0 derecha izquierda)))
              (else (if (equal? Fila2 '()) "" (AnimarCaras12Aux Fila1 (cdr Fila2) posx1 posy1 (+ posx2 22) (- posy2 25) 0 derecha izquierda)))))
  )
)  
        

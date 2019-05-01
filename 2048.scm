(require graphics/graphics)
(require 2htdp/image)
;variables globales
(define-struct jugador (nombre punteo gana))
(define gTablero (make-vector 4 0))
(define gCeros 0)
(define vv 0)




(define (gen-tablero)
  ;Almacena coordenadas de ceros en la matriz (casillas vacias)
  (set! gCeros (make-vector 16))
  (define idx 0)
  (define (ciclo  step)
    (define (interno stepI)
      (if (< stepI 4)
          (begin
            ;Recorre cada casilla del tablero para recuperar coordenadas (x,y) de casillas vacias
            (if (= 0 (vector-ref (vector-ref gTablero step) stepI))
                (begin
                  (vector-set! gCeros idx (mcons 0 0))
                  (set-mcar! (vector-ref gCeros idx) step)
                  (set-mcdr! (vector-ref gCeros idx) stepI)
                  (set! idx (add1 idx))
                  (interno (add1 stepI))
                  )
                )
            )
          ))
    (if (< step 4)
        (begin
          (vector-set! gTablero step (make-vector 4 0))
          (interno 0)
          (ciclo (add1 step))
          )
        ))
  (ciclo 0)
  (if  (and (map eq? (gen-baldoza) (gen-baldoza)))
       (gen-baldoza)
       )
  
  ;ceros
  gTablero
  )

;Recorre listado de ceros para agregar una baldoza 2 o 4 en una casilla disponible
(define (gen-baldoza)
  (let ([x 0] [y 0] [rand (random 16)] [val (random 5)])
    (set! x (mcar (vector-ref gCeros rand)))
    (set! y (mcdr (vector-ref gCeros rand)))
    (if (and (number? x) (number? y)) 
        (begin
          (set-mcar! (vector-ref gCeros rand) "X")
          (set-mcdr! (vector-ref gCeros rand) "Y")
          (cond 
            [(= val 0) (vector-set! (vector-ref gTablero x) y 2)]
            [(= val 1) (vector-set! (vector-ref gTablero x) y 2)]
            [(= val 2) (vector-set! (vector-ref gTablero x) y 2)]
            [(= val 3) (vector-set! (vector-ref gTablero x) y 2)]
            [(= val 4) (vector-set! (vector-ref gTablero x) y 4)]
            ))
        (gen-baldoza)
        )
    (list x y)
    )
  )


(define (merge)
  (define act 0) (define sig 0)
  (define (ciclo  row)
    (define (interno col)
      (if (< col 3)
          (begin
            (set! act (vector-ref (vector-ref gTablero row) col))
            (set! sig (vector-ref (vector-ref gTablero row) (add1 col)))
            (if (not (= act 0))
                (begin
                  (if (= act sig)
                      ;Hace merge de 2 baldozas iguales
                      (begin
                        (vector-set! (vector-ref gTablero row) (add1 col) (* 2 act))
                        (vector-set! (vector-ref gTablero row) col 0)
                        (interno (+ 1 col))
                        )
                      (begin
                        (if (= sig 0)
                            ;Sustituye un cero siguiente por numero actual
                            (begin 
                              (vector-set! (vector-ref gTablero row) (add1 col) act)
                              (vector-set! (vector-ref gTablero row) col 0)
                              (interno (add1 col))
                              )
                            (begin
                              (interno (add1 col))
                              (if (> 0 col) (begin
                                              (interno (- 1 col)))
                                  (compact-row))
                              )
                            )
                        )
                      )
                  )
                (begin (interno (add1 col)))
                )
            )
          )
      )
    (if (< row 4)
        (begin
          ;(vector-set! gTablero (make-vector 4 0))
          (interno 0)
          (ciclo (add1 row))
          )
        ))
  (ciclo 0)
(rCeros)
  (gen-baldoza)
  )
  

#|Algoritmo  modificado, tomado bajo licencia open source de https://gist.github.com/liamgriffiths/9023179|#
(define (transpose m)
  (apply map list m))

(define (rotate-90)
  (set! gTablero (list->vector (map list->vector
                                    (transpose (reverse (map vector->list (vector->list gTablero))))
                                    )
                               )
        )
  )

(define (parse-cmd cmd)
  (cond
    [(eq?  cmd 'up) (up)]
    [(eq? cmd 'down) (down)]
    [(eq? cmd 'right) (right)]
    [(eq? cmd 'left) (left)]
    )
  )

(define (right)
  (merge)
  (gen-baldoza)
  )

(define (left)
  (rotate-90)
  (rotate-90)
  (merge)
  (rotate-90)
  (rotate-90)
  (gen-baldoza)
  )
(define (up)
  (rotate-90)
  (merge)
  (rotate-90)
  (rotate-90)
  (rotate-90)
  (gen-baldoza)
  )
(define (down)
  (rotate-90)
  (rotate-90)
  (rotate-90)
  (merge)
  (rotate-90)
  (gen-baldoza)
  )


(define (compact-row)
  (define act 0)(define sig 0) (define ant 0)
  (define (ciclo  row)
    (define (interno col)
      (if (< col 3)
          (begin
            (set! act (vector-ref (vector-ref gTablero row) col))
            (set! sig (vector-ref (vector-ref gTablero row) (add1 col)))
            (if (not (= act 0))
                (begin
                  (if (= sig 0)
                      (begin
                        (vector-set! (vector-ref gTablero row) (add1 col) act)
                        (vector-set! (vector-ref gTablero row) col 0)
                        )
                      (begin (interno (add1 col)
                                      )
                             )
                      )
                  )
                (begin
                  (if (not (= col 0))
                      (begin (set! ant (- (vector-ref (vector-ref gTablero row) col) 1))
                             (if (= 0 ant)
                                 (begin 
                                   (vector-set! (vector-ref gTablero row) col ant)
                                   (vector-set! (vector-ref gTablero row) (- 1 col) 0)
                                   (interno (add1 col))
                                   )
                                 )
                             )
                      ) 
                  ) 
                )
            )
          )
      )
    (if (< row 4)
        (begin
          ;(vector-set! gTablero (make-vector 4 0))dasd
          (interno 0)
          (ciclo (add1 row))
          )))
  (ciclo 0)
  )

(define (rCeros)
  (define idx 0)
  (define (ciclo  step)
    (define (interno stepI)
      (if (< stepI 4)
          (begin
            ;Recorre cada casilla del tablero para recuperar coordenadas (x,y) de casillas vacias
            (if (= 0 (vector-ref (vector-ref gTablero step) stepI))
                (begin
                  (set-mcar! (vector-ref gCeros idx) step)
                  (set-mcdr! (vector-ref gCeros idx) stepI)
                  (set! idx (add1 idx))
                  (interno (add1 stepI))
                  )
                )
            )
          ))
    (if (< step 4)
        (begin
          (interno 0)
          (ciclo (add1 step))
          )
        ))   
    (ciclo 0)
    )
  
  
  ;##################################################################################
  (define (main)
    
    (open-graphics)

    (display "Juan Pablo Monzón")(newline)(display "Diego Andre Porras")(newline)(display "Miguel Itzep Castro")(newline)(display "Jose Carlos King")(newline)
    (display "\n We play 2048!!\n Input player name: ")
    (define name (read-line))
    (gen-tablero)
    
    
    (set! vv (open-viewport "Game-2048" 400 600))
    
    ((draw-viewport vv) (make-rgb 0.9 0.7 0.2))
    
    ((draw-rectangle vv)(make-posn 23 35) 88 20 "white")
    
    ((draw-string vv)(make-posn 25  50)  "GAME 2048")
    ((draw-string vv)(make-posn 25  100)  "Player: ")
    ((draw-string vv)(make-posn 80  100)  name  "white")
    
    
    ;line vertical
    ((draw-solid-rectangle vv)(make-posn 25 230)  10 350 "brown")
    ((draw-solid-rectangle vv)(make-posn 110 230) 10 350 "brown")
    ((draw-solid-rectangle vv)(make-posn 195 230) 10 350 "brown")
    ((draw-solid-rectangle vv)(make-posn 280 230) 10 350 "brown")
    ((draw-solid-rectangle vv)(make-posn 365 230) 10 350 "brown")
    ;line horisontalddd
    ((draw-solid-rectangle vv)(make-posn 25 230) 350 10 "brown")
    ((draw-solid-rectangle vv)(make-posn 25 315) 350 10 "brown")
    ((draw-solid-rectangle vv)(make-posn 25 400) 350 10 "brown")
    ((draw-solid-rectangle vv)(make-posn 25 485) 350 10 "brown")
    ((draw-solid-rectangle vv)(make-posn 25 570) 350 10 "brown")
    (let ciclo
      ((kp (get-key-press vv)))
      (define key (key-value kp))
      (if (equal? key #\return)
          (begin
            (display "terminado")
            
            (close-graphics)
            )
          (begin
            (if (not (equal? key 'release))
                (begin
                  (parse-cmd key)
                  ;variables para las posiciones de los numeros en la matriz
                  (set! fila 0)
                  (set! columna 0)
                  ;variables para las posiciones de los cuadros en la ventana
                  (set! x 35)
                  (set! y 240)
                  ;(display gTablero)
                  
                  ;                (newline)
                  ;              (clear-viewport vv)
                  
                  ;(newline)
                  
                  )
                )
            (ma)
            (ciclo (get-key-press vv)) 
            )
          )
      )
    )
  
  
  
  
  
  ;##################################################################################################################################################
  ;variables para las posiciones de los numeros en la matriz
  (define fila 0)
  (define columna 0)
  ;variables para las posiciones de los cuadros en la ventana
  (define x 35)
  (define y 240)
  
  ;buscador de posicion de umero en la matriz
  ;luego en la misma funcion lo debuja en la ventana grafica
  (define (ma)  
    (if (< fila 4)
        (begin
          (if (< columna 4)
              (begin
                (cond
                  [(= (vector-ref (vector-ref gTablero fila) columna)    0)((draw-solid-rectangle vv)(make-posn x y) 75 75 (make-rgb 0.9 0.7 0.2)) ((draw-string vv) (make-posn (+ x 25) (+ y 45))    " ")]
                  [(= (vector-ref (vector-ref gTablero fila) columna)    2)((draw-solid-rectangle vv)(make-posn x y) 75 75 (make-rgb 0.1 0.2 0.9)) ((draw-string vv) (make-posn (+ x 25) (+ y 45))    "2")]
                  [(= (vector-ref (vector-ref gTablero fila) columna)    4)((draw-solid-rectangle vv)(make-posn x y) 75 75 (make-rgb 0.4 0.5 0.6)) ((draw-string vv) (make-posn (+ x 25) (+ y 45))    "4")]
                  [(= (vector-ref (vector-ref gTablero fila) columna)    8)((draw-solid-rectangle vv)(make-posn x y) 75 75 (make-rgb 0.7 0.8 0.9)) ((draw-string vv) (make-posn (+ x 25) (+ y 45))    "8")]
                  [(= (vector-ref (vector-ref gTablero fila) columna)   16)((draw-solid-rectangle vv)(make-posn x y) 75 75 (make-rgb 0.9 0.8 0.7)) ((draw-string vv) (make-posn (+ x 25) (+ y 45))   "16")]
                  [(= (vector-ref (vector-ref gTablero fila) columna)   32)((draw-solid-rectangle vv)(make-posn x y) 75 75 (make-rgb 0.6 0.5 0.4)) ((draw-string vv) (make-posn (+ x 25) (+ y 45))   "32")]
                  [(= (vector-ref (vector-ref gTablero fila) columna)   64)((draw-solid-rectangle vv)(make-posn x y) 75 75 (make-rgb 0.3 0.3 0.1)) ((draw-string vv) (make-posn (+ x 25) (+ y 45))   "64")]
                  [(= (vector-ref (vector-ref gTablero fila) columna)  128)((draw-solid-rectangle vv)(make-posn x y) 75 75 (make-rgb 0.1 0.4 0.7)) ((draw-string vv) (make-posn (+ x 25) (+ y 45))  "128")]
                  [(= (vector-ref (vector-ref gTablero fila) columna)  256)((draw-solid-rectangle vv)(make-posn x y) 75 75 (make-rgb 0.9 0.6 0.3)) ((draw-string vv) (make-posn (+ x 25) (+ y 45))  "256")]
                  [(= (vector-ref (vector-ref gTablero fila) columna)  512)((draw-solid-rectangle vv)(make-posn x y) 75 75 (make-rgb 0.5 0.1 0.8)) ((draw-string vv) (make-posn (+ x 25) (+ y 45))  "512")]
                  [(= (vector-ref (vector-ref gTablero fila) columna) 1024)((draw-solid-rectangle vv)(make-posn x y) 75 75 (make-rgb 0.8 0.3 0.3)) ((draw-string vv) (make-posn (+ x 25) (+ y 45)) "1024")]
                  [(= (vector-ref (vector-ref gTablero fila) columna) 2048)((draw-solid-rectangle vv)(make-posn x y) 75 75 (make-rgb 0.3 0.6 0.1)) ((draw-string vv) (make-posn (+ x 25) (+ y 45)) "2048")] 
                  )            
                (set! columna (+ columna 1))
                (set! x (+ x 85))
                (ma)
                )             
              )
          (set! columna 0)
          (set! x 35)
          (set! fila (+ fila 1))
          (set! y (+ y 85))
          (ma)
          )
        )
    )
  
  
  ;####################################################################################################################################################
  
  
  (main)
  
  
  
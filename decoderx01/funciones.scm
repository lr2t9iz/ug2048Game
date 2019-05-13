; PUEDE AGREGAR MÁS FUNCIONES E INSTRUCCIONES SEGÚN SU NECESIDAD
(define L 26) ; constante numeros de letras
; encodeChar codifica un caracter en base a otro caracter de llave
(define (encodeChar chTxt chLlave)
  ;su implementación
  (define nct (char->integer chTxt)) ;Numero de Char Texto;ASCI
  (define ncl (char->integer (char-upcase chLlave))) ;Numero de Char Llave;ASCII
  (set! ncl (- ncl 65)) ;ASCII A AUX
  (define aux 0) ;JUGAR CON 0-25, POSICION DEL LAS LETRAS DEL ACBDARIO
  (cond [(and (>= nct 65)(<= nct 90))(set! nct (- nct 65))(set! aux 65)] ;MAYUSCULA ;ASCII A AUX
        [(and (>= nct 97)(<= nct 122))(set! nct (- nct 97))(set! aux 97)] ;minuscula ;ASCII A AUX
        )
  ;algoritmo Vigenère  ;https://es.wikipedia.org/wiki/Cifrado_de_Vigen%C3%A8re
  (define modi (modulo (+ ncl nct) L))
  (integer->char (+ modi aux)) ;RETURN
  ) ;TERMINADO

; encodeString codifica un string utilizando la llave parametrizada
(define (encodeString txt llave)
   ;su implementación
  (define llaves "")
  (define text-co "")
  (define i-fors 0) ;contador de for
  (define j-fors 0) ;contaodr del ajuste de llave
  (define nct 0)
  (define (fors)
    (if(< i-fors (string-length txt)) ; if ( i < tama;o texto)
       (begin
         (if(= j-fors (string-length llave))(begin (set! j-fors 0)))
         (set! llaves (string-append llaves (string (string-ref llave j-fors)))) ; AJUSTANDO LA LLAVEA AL TAMA;O DEL TEXTO
         (set! nct (char->integer (string-ref txt i-fors)))
         (if (or (and (>= nct 65)(<= nct 90)) (and (>= nct 97)(<= nct 122)))
             (begin (set! text-co (string-append text-co (string (encodeChar (string-ref txt i-fors)(string-ref llave j-fors))))))
             (begin (set! text-co (string-append text-co (string (integer->char nct)))))
             )
         (set! j-fors (+ j-fors 1)) ; j++  
         (set! i-fors (+ i-fors 1)) ; i++
         (fors)
         )
       )
    )
  (fors)
  text-co ;
  )

; decodeChar decode un caracter en base a otro caracter de llave
(define (decodeChar chTxt chLlave)
  ;su implementación
  (define nct (char->integer chTxt))
  (define ncl (char->integer (char-upcase chLlave)))
  (set! ncl (- ncl 65))
  (define aux 0)
  (cond [(and (>= nct 65)(<= nct 90))(set! nct (- nct 65))(set! aux 65)] ;MAYUSCULA
        [(and (>= nct 97)(<= nct 122))(set! nct (- nct 97))(set! aux 97)] ;minuscula
        )
  ;algoritmo Vigenère Inversa ;https://es.wikipedia.org/wiki/Cifrado_de_Vigen%C3%A8re
  (define n (- nct ncl))
  (if (>= n 0) ;condicion inicial
      (begin (integer->char (+ (modulo n L) aux))) ;Cuando (Ci - Ki) >= 0 
      (begin (integer->char (+ (modulo (+ n L) L) aux))) ;Cuando (Ci - Ki) < 0 
      )
  ;#\null
)

; decodeString decodifica un strgin utilizando la llave parametrizada
(define (decodeString txt llave)
   ;su implementación
   ""
)
(encodeString "Hola" "lemon") ; "Ssxo"
(encodeString "Hola Mundo! Tengo 5 MayúsCulas" "lemon") ;"Ssxo Zfrpc! Gprsc 5 Zlc\\ggPfpmg"
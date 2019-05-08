(require racket/string)
;mensaje de bienvenida
(display "Hola Soy Josue la mera verga")
;variables grobales
(define palabras "")
(define llave "pass-dafault")
;definiendo funciones secundarias 
(define (salida)(display "Saliendo...\nGracias por usar nuestro codificador."))
(define (texto-no-valido)(display "resultado >> ERROR! Expresión no valida\n"))
(define (llavecita)())
;funcion principal
(define (main)
  (define commandline "")
  (define flag "")
  (define arg1 "")
  (define arg2 "")
  (newline)
  ;FOR
  (define (for-ciclo)
    (display "encoder>> ")(set! commandline (read-line))
    ;Generalizando parametros y al mismo tiempo validando entradas
    (set! palabras (length(string-split commandline)))
    (cond[(= palabras 1)
          (set! flag (list-ref (string-split commandline)0))]
         [(= palabras 2)
          (set! flag (list-ref (string-split commandline)0))
          (set! arg1 (list-ref (string-split commandline)1))]
         [(= palabras 3)
          (set! flag (list-ref (string-split commandline)0))
          (set! arg1 (list-ref (string-split commandline)1))
          (set! arg2 (list-ref (string-split commandline)2))]
         [else (texto-no-valido)(for-ciclo)])
    ;COND - para validad banderas
    (cond[(equal? flag "quit")(if(= palabras 1)
                                 (begin(salida))
                                 (begin(texto-no-valido)(for-ciclo)))]
         [(equal? flag "setkey")(if(= palabras 2)
                                   (begin(set! llave arg1)
                                         (display "resultado >> nueva llave aceptada\n")
                                         (display llave)(for-ciclo))
                                   (begin (texto-no-valido)(for-ciclo)))]
         [(equal? flag "encode-text")()]
         [(equal? flag "encode-text-key")()]
         [(equal? flag "encode-file")()]
         [(equal? flag "decode-text")()]
         [(equal? flag "decode-text-key")()]
         [(equal? flag "decode-file")()]
         [else (texto-no-valido)(for-ciclo)]
     );Finalizando COND
    );FINALIZANDO FOR
  (for-ciclo)
)(main)
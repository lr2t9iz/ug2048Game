(require racket/list)
(require racket/string)
(include "funciones.scm") ;importanto libreria funciones hecho por mi
; programa principal
;mensaje de bienvenida
(display "Codificador/Decodificar\n@autor: Josue Aguilar\nCarné: 12003857\nSección: AN")
;variables grobales
(define palabras 0)
(define llave "")
(define texto-a-cifrar "")
(define texto-codificado "")
;definiendo funciones secundarias 
(define (salida)(display "Saliendo...\nGracias por usar nuestro codificador."))
(define (texto-no-valido)(display "resultado >> ERROR! Expresión no valida\n"))
(define (llavecita)())
;funcion principal
(define (main)
    ; su implementación acá
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
    (define i 1)
    (define in "")
    (cond[(= palabras 1)
          (set! flag (list-ref (string-split commandline)0))]
         [(= palabras 2)
          (set! flag (list-ref (string-split commandline)0))
          (set! arg1 (list-ref (string-split commandline)1))]
         [(= palabras 3)
          (set! flag (list-ref (string-split commandline)0))
          (set! arg1 (list-ref (string-split commandline)1))
          (set! arg2 (list-ref (string-split commandline)2))])
    ;COND - para validad banderas
    (cond[(equal? flag "quit")(if(= palabras 1)
                                 (begin(salida))
                                 (begin(texto-no-valido)(for-ciclo)))]
         [(equal? flag "setkey")(if(= palabras 2)
                                   (begin(set! llave arg1)
                                         (display "resultado >> Nueva llave aceptada\n")
                                         (display llave)(for-ciclo))
                                   (begin (texto-no-valido)(for-ciclo)))]
         [(equal? flag "encode-text")(if(string=? llave "")
                                         (begin (display "resultado >> Llave no encotrado\n"))
                                         (begin
                                           ( if (not (= palabras 1))
                                                (begin
                                                  (set! texto-a-cifrar (substring (string-replace commandline flag "")1))
                                                  (display (string-append "\nresultado >> " (encodeString texto-a-cifrar llave) "\n"))
                                                  )
                                                (begin (texto-no-valido))
                                                )
                                           )
                                      )
                                     (for-ciclo)]
         [(equal? flag "encode-text-key")(if (string? arg1)
                                             (begin
                                             (if (not (= palabras 2))
                                                 (begin
                                                   (set! texto-a-cifrar (substring (string-replace commandline arg1 "")1))
                                                   (display (string-append "\nresultado >> " (encodeString texto-a-cifrar arg1) "\n"))
                                                   )
                                                 (begin (texto-no-valido))
                                                 )
                                             )
                                             )
                                             (for-ciclo)]
         [(equal? flag "encode-file")(if(string=? llave "")
                                        (begin (display "resultado >> Llave no encotrado\n"))
                                        (begin
                                          ( if (not (= palabras 1))
                                               (begin
                                                  (if (file-exist? arg1)
                                                      (begin
                                                        ;open input port
                                                        (set! in (open-input-file arg1))
                                                        (diplay (read-line in))
                                                        (close-input-port in);cerrando puerta
                                                      (begin (texto-no-valido))
                                                      )
                                                      )
                                                  )
                                               )
                                          )
                                        )
                                     ]
         [(equal? flag "decode-text")()]
         [(equal? flag "decode-text-key")()]
         [(equal? flag "decode-file")()]
[else (texto-no-valido)(for-ciclo)]

     );Finalizando COND
);FINALIZANDO FOR
(for-ciclo)
)
(main)
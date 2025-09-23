#lang racket 


(require "TDAusuario.rkt"
         "TDALibro.rkt"
         "TDAPrestamo.rkt"
         "TDABiblioteca.rkt")

(provide (all-from-out "TDAusuario.rkt"
                       "TDALibro.rkt"
                       "TDAPrestamo.rkt"
                       "TDABiblioteca.rkt"))

;;modificadores
;RF06: agregar-libro
(define (agregar-libro biblioteca libro)
  (if (busco-libro? biblioteca (get-libro-id libro))
      biblioteca
      (list (cons libro (get-biblioteca-libros biblioteca))
            (get-biblioteca-usuarios biblioteca)
            (get-biblioteca-prestamos biblioteca)
            (get-biblioteca-max-libros biblioteca)
            (get-biblioteca-dias-max biblioteca)
            (get-biblioteca-tasa-multa biblioteca)
            (get-biblioteca-limite-deuda biblioteca)
            (get-biblioteca-dias-retraso biblioteca)
            (get-biblioteca-fecha-inicial biblioteca))))

;RF07: registrar-usuario
;;Descripcion: Registra usuario en el sistema,al final de la lista, usuario inicia con deuda 0 y estado activo.
;;Dom: biblioteca (Biblioteca) X usuario (Usuario)
;;Rec: Biblioteca
;;Tipo de Recursion: recursion natural
(define (registrar-usuario biblioteca usuario)
  (define (registrar usuarios) ;;wrapper
    (cond [(null? usuarios) (list usuario)]                   ;;caso base lista vacia
          [(= (get-usuario-id (car usuarios)) (get-usuario-id usuario)) usuarios]   
          [else (cons (car usuarios) (registrar (cdr usuarios)))])) ; llamada recursiva natural
  (list (get-biblioteca-libros biblioteca)
        (registrar (get-biblioteca-usuarios biblioteca))
        (get-biblioteca-prestamos biblioteca)
        (get-biblioteca-max-libros biblioteca)
        (get-biblioteca-dias-max biblioteca)
        (get-biblioteca-tasa-multa biblioteca)
        (get-biblioteca-limite-deuda biblioteca)
        (get-biblioteca-dias-retraso biblioteca)
        (get-biblioteca-fecha-inicial biblioteca)))

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
  (if (libro-disponible? biblioteca (get-libro-id l))
      biblioteca
      (list (cons libro (get-biblioteca-libros b))
            (get-biblioteca-usuarios b)
            (get-biblioteca-prestamos b)
            (get-biblioteca-max-libros b)
            (get-biblioteca-dias-max b)
            (get-biblioteca-tasa-multa b)
            (get-biblioteca-limite-deuda b)
            (get-biblioteca-dias-retraso b)
            (get-biblioteca-fecha-inicial b))))

;RF07: registrar-usuario
(define (registrar-usuario biblioteca usuario)
  (define (registrar usuarios) ;;wrapper
    (cond [(null? usuarios) (list usuario)]                   ;;caso base lista vacia
          [(= (get-usuario-id (car usuarios)) (get-usuario-id u)) usuarios]    ; si ya existe no cambia
          [else (cons (car usuarios) (registrar (cdr usuarios)))])) ; llamada recursiva natural
  (list (get-biblioteca-libros b)
        (registrar (get-biblioteca-usuarios b))
        (get-biblioteca-prestamos b)
        (get-biblioteca-max-libros b)
        (get-biblioteca-dias-max b)
        (get-biblioteca-tasa-multa b)
        (get-biblioteca-limite-deuda b)
        (get-biblioteca-dias-retraso b)
        (get-biblioteca-fecha-inicial b)))

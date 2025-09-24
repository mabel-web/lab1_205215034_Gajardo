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

;;rf17 tomar-prestamo
(define (tomar-prestamo biblioteca id-usuario id-libro dias fecha-actual)
  (cond [(or (null? (buscar-libro biblioteca "id" id-libro))
        (not (get-libro-disponible (buscar-libro b "id" id-libro)))) biblioteca])
        [(or (null? (obtener-usuario biblioteca id-usuario))(usuario-suspendido? (obtener-usuario b id-usuario))) biblioteca]
        [(>= (length (get-usuario-prestamos (obtener-usuario b id-usuario)))(get-biblioteca-max-libros b)) biblioteca]
        [(> dias (get-biblioteca-dias-max b)) biblioteca]
        [(>= (get-usuario-deuda (obtener-usuario biblioteca id-usuario))(get-biblioteca-limite-deuda b)) biblioteca]
        [else ;no hay restricciones
          (list  (map (lambda (libros);; marca como no disponible el q coincide el id
                 (if (= (get-libro-id libros) id-libro)
                     (set-libro-disponible libros #f) libros))
               (get-biblioteca-libros b))
                (map (lambda (usuarios) ; agrego el nuevo prestamo a la lista del usuario
                 (if (= (get-usuario-id usuarios) id-usuario)
                     (set-prestamo u (crear-prestamo id-libro id-usuario id-libro fecha-actual dias))
                     usuarios)) (get-biblioteca-usuarios b))
          (cons (crear-prestamo id-libro id-usuario id-libro fecha-actual dias)(get-biblioteca-prestamos b));agrega el nuevo prestamo al registro global
          (get-biblioteca-max-libros b)
          (get-biblioteca-dias-max b)
          (get-biblioteca-tasa-multa b)
          (get-biblioteca-limite-deuda b)
          (get-biblioteca-dias-retraso b)
          (get-biblioteca-fecha-inicial b))])) 


;;tengo q arreglar los nombres s he estado usando abreviaciones en los tdas y despues puse mas descriptivosREVISAR
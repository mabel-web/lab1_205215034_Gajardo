#lang racket

(provide (all-defined-out))

;;TDA BIBLIOTECA
;;REPRESENTACION
;;list (libros usuarios prestamos max-libros-usuario dias-max-prestamo tasa-multa-diaria limite-deuda-max dias-max-retraso fecha-inicial)

;;CONSTRUCTORA
; Dom: id (int) X libros (List libro)
; Rec: Biblioteca
(define (biblioteca libros usuarios prestamos max-libros dias-max tasa-multa limite-deuda dias-retraso fecha-inicial)
  (list '() '()  '() 2 3 100 1000 1 "01/01"))

;;PERTENENCIA
;;Descripcion: esta funcion permite validar los parametros que pertenecen al TDA biblioteca, devuelve true si la entrada es valida, sino false
;;dom: libros (Lista Libro) X usuarios (Lista Usuario) X prestamos (Lista Prestamo)
;;X max-libros-usuario (int) X dias-max-prestamo (int) X
;;tasa-multa-diaria (int) X limite-deuda-max (int) X dias-max-retraso (int) X fecha-inicial (string)
;;rec: biblioteca
(define (biblioteca? b)
  (and (list? b)
       (= (length b) 9)           ; largo de la lista
       (list? (car b))            ; libros
       (list? (cadr b))           ; usuarios
       (list? (caddr b))          ; prestamos
       (integer? (cadddr b))      ; max libros x usuario
       (integer? (list-ref b 4))  ; dias max prestamo
       (integer? (list-ref b 5))   ; tasa multa 
       (integer? (list-ref b 6))   ; limite deuda 
       (integer? (list-ref b 7))   ;dias max retraso
       (string? (list-ref b 8))))  ; fecha inicial

      
;;SELECTORES
;;Descripción: esta funcion entrega la lista de libros disponibles de la biblioteca
;;dom: libros (Lista Libro) X usuarios (Lista Usuario) X prestamos (Lista Prestamo) X max-libros-usuario (int) X dias-max-prestamo (int) X
;;tasa-multa-diaria (int) X limite-deuda-max (int) X dias-max-retraso (int) X fecha-inicial (string)
;;rec: biblioteca
(define (get-biblioteca-libros b)(car b))

;;Descripcion: esta funcion entrega la lista de usuaruios de la biblioteca
;;dom: libros (Lista Libro) X usuarios (Lista Usuario) X prestamos (Lista Prestamo) X max-libros-usuario (int) X dias-max-prestamo (int) X
;;tasa-multa-diaria (int) X limite-deuda-max (int) X dias-max-retraso (int) X fecha-inicial (string)
;;rec: biblioteca
(define (get-biblioteca-usuarios b)(cadr b))

;;Descripción: esta funcion entrega la lista de prestamos de libros de la biblioteca
;;dom: libros (Lista Libro) X usuarios (Lista Usuario) X prestamos (Lista Prestamo) X max-libros-usuario (int) X dias-max-prestamo (int) X
;;tasa-multa-diaria (int) X limite-deuda-max (int) X dias-max-retraso (int) X fecha-inicial (string)
;;rec: biblioteca
(define (get-biblioteca-prestamos b)(caddr b))

;;Descripción: esta funcion entrega el numero maximo de libros a prestar
;;dom: libros (Lista Libro) X usuarios (Lista Usuario) X prestamos (Lista Prestamo) X max-libros-usuario (int) X dias-max-prestamo (int) X
;;tasa-multa-diaria (int) X limite-deuda-max (int) X dias-max-retraso (int) X fecha-inicial (string)
;;rec: biblioteca
(define (get-biblioteca-max-libros b)(cadddr b))

;;Descripción: esta funcion entrega el numero maximo de dias que puede tener un prestamo
;;dom: libros (Lista Libro) X usuarios (Lista Usuario) X prestamos (Lista Prestamo) X max-libros-usuario (int) X dias-max-prestamo (int) X
;;tasa-multa-diaria (int) X limite-deuda-max (int) X dias-max-retraso (int) X fecha-inicial (string)
;;rec: biblioteca
(define (get-biblioteca-dias-max-prestamo b)(list-ref b 4))

;;Descripción: esta funcion entrega el monto por día de retraso.
;;dom: libros (lista Libro) X usuarios (Lista Usuario) X prestamos (Lista Prestamo) X max-libros-usuario (int) X dias-max-prestamo (int) X
;;tasa-multa-diaria (int) X limite-deuda-max (int) X dias-max-retraso (int) X fecha-inicial (string)
;;rec: biblioteca
(define (get-biblioteca-tasa-multa-diaria b)(list-ref b 5))

;;Descripción: esta funcion entrega el numero de la deuda máxima antes de suspensión
;;dom: libros (lista Libro) X usuarios (Lista Usuario) X prestamos (Lista Prestamo) X max-libros-usuario (int) X dias-max-prestamo (int) X
;;tasa-multa-diaria (int) X limite-deuda-max (int) X dias-max-retraso (int) X fecha-inicial (string)
;;rec: biblioteca
(define (get-biblioteca-limite-deuda b)(list-ref b 6))

;;Descripción: esta funcion entrega los días de retraso antes de suspensión
;;dom: libros (Lista Libro) X usuarios (Lista Usuario) X prestamos (Lista Prestamo) X max-libros-usuario (int) X dias-max-prestamo (int) X
;;tasa-multa-diaria (int) X limite-deuda-max (int) X dias-max-retraso (int) X fecha-inicial (string)
;;rec: biblioteca
(define (get-biblioteca-dias-max-retraso b)(list-ref b 7))

;;Descripción: esta funcion entrega la fecha de inicio del sistema
;;Dom: usuario consultado (list)
;;Rec: estado del usuario consutado (bool)
(define (get-biblioteca-fecha-inicial b)(list-ref b 8))

;;MODIFICADORES
;;Los modificadores correspondientes a max-libros-usuario (int) X dias-max-prestamo (int) X
;;tasa-multa-diaria (int) X limite-deuda-max (int) X dias-max-retraso (int) X fecha-inicial (string)
;;no estan porque son reglas de la biblioteca que no deben cambiar 

;;Descripción: esta funcion permite modificar la lista de libros de la biblioteca
;;Dom:
;;Rec: 
(define (set-biblioteca-libros b nueva-biblioteca-libros)
  (list nueva-biblioteca-libros
        (get-biblioteca-usuarios b)
        (get-biblioteca-prestamos b)
        (get-biblioteca-max-libros b)
        (get-biblioteca-dias-max-prestamo b)
        (get-biblioteca-tasa-multa-diaria b)
        (get-biblioteca-limite-deuda b)
        (get-biblioteca-limite-deuda b)
        (get-biblioteca-fecha-inicial b)))

;;Descripción: esta funcion permite modificar la lista de usuarios de la biblioteca
(define (set-biblioteca-usuarios b nueva-biblioteca-usuarios)
  (list (get-biblioteca-libros b)
        nueva-biblioteca-usuarios
        (get-biblioteca-prestamos b)
        (get-biblioteca-max-libros b)
        (get-biblioteca-dias-max-prestamo b)
        (get-biblioteca-tasa-multa-diaria b)
        (get-biblioteca-limite-deuda b)
        (get-biblioteca-limite-deuda b)
        (get-biblioteca-fecha-inicial b)))


;;Descripción: esta funcion permite modificar la lista de prestamos de la biblioteca
(define (set-biblioteca-prestamos b nueva-biblioteca-prestamos)
  (list (get-biblioteca-libros b)
        (get-biblioteca-usuarios b)
        nueva-biblioteca-prestamos
        (get-biblioteca-max-libros b)
        (get-biblioteca-dias-max-prestamo b)
        (get-biblioteca-tasa-multa-diaria b)
        (get-biblioteca-limite-deuda b)
        (get-biblioteca-limite-deuda b)
        (get-biblioteca-fecha-inicial b)))

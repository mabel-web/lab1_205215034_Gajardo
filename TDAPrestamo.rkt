#lang racket

(provide (all-defined-out))

;;TDA Prestamo
;;REPRESENTACION
;;lista (id id-usuario id-libro fecha-prestamo dias-solicitados)

;;CONSTRUCTORA
;;Descripcion: esta funcion permite definir la reprensentacion de tda prestamo con los parametros de entrada y condiciones iniciales requeridas.
;;Dom: id (int) X id-usuario (int) X id-libro (int) X fecha-prestamo (string) X dias-solicitados (int)
;;Rec: prestamo (lista con estructura (id id-usuario id-libro fecha-prestamo dias-solicitados))
(define (crear-prestamo id id-usuario id-libro fecha-prestamo dias-solicitados)
  (list id id-usuario id-libro fecha-prestamo dias-solicitados))

;;PERTENENCIA
;;Descripcion: esta funcion permite validar los parametros que pertenecen a prestamos, devuelve true si la entrada es valida, sino false
;;Dom: id (int) X id-usuario (int) X id-libro (int) X fecha-prestamo (string) X dias-solicitados (int)
;;Rec: prestamo (lista con estructura (id id-usuario id-libro fecha-prestamo dias-solicitados))
(define (prestamo? p)
  (and (list? p)
       (= (length p) 5)        ; largo de la lista
       (integer? (car p))       ; id int
       (integer? (cadr p))       ; id-usuario int
       (integer? (caddr p))     ; id-libro int
       (string? (cadddr p))    ; fecha string < a 30 dias
       (integer? (list-ref p 4)))) ; dias int

;;SELECTORES
;;Descripción: esta funcion entrega el id del prestamo
;;Dom: prestamo consultado (list)
;;Rec: id del prestamo (int)
(define (get-prestamo-id p)(car p))

;;Descripción: esta funcion entrega el id-usuario del prestamo
;;Dom: prestamo consultado (list)
;;Rec: id-usuario del prestamo (int)
(define (get-prestamo-id-usuario p)(cadr p))

;;Descripción: esta funcion entrega el id-libro del prestamo
;;Dom: prestamo consultado (list)
;;Rec: id-libro del prestamo (int)
(define (get-prestamo-id-libro p)(caddr p))

;;Descripción: esta funcion entrega la fecha del prestamo
;;Dom: prestamo consultado (list)
;;Rec: fecha del prestamo (int)
(define (get-prestamo-fecha p)(cadddr p))

;;Descripción: esta funcion entrega los dias solicitados del prestamo
;;Dom: prestamo consultado (list)
;;Rec: dias-solicitados del prestamo (int)
(define (get-prestamo-dias-solicitados p)(list-ref p 4))

;;MODIFICADORES
;;Descripción: esta funcion permite modificar los días solicitados de un libro
;;Dom: id (int) X id-usuario (int) X id-libro (int) X fecha-prestamo (string) X dias-solicitados (int)
;;Rec: prestamo (nueva lista con estructura (id id-usuario id-libro fecha-prestamo nuevo-dias-solicitados))
(define (set-prestamo-dias-solicitados p nuevos-dias-solicitados)
  (list (get-prestamo-id p)
        (get-prestamo-id-usuario p)
        (get-prestamo-id-libro p)
        (get-prestamo-fecha p)
        nuevos-dias-solicitados))

;;Descripción: esta funcion permite modificar la fecha de un libro solicitado
;;Dom: id (int) X id-usuario (int) X id-libro (int) X fecha-prestamo (string) X dias-solicitados (int)
;;Rec: prestamo (nueva lista con estructura (id id-usuario id-libro nueva-fecha-prestamo dias-solicitados))
(define (set-prestamo-fecha p nueva-fecha-prestamo)
  (list (get-prestamo-id p)
        (get-prestamo-id-usuario p)
        (get-prestamo-id-libro p)
        nueva-fecha-prestamo
        (get-prestamo-dias-solicitados p)))

;;pendiente rf18
;;rf19
;;rf22
;;rf 24
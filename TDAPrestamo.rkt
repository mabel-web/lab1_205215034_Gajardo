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

;;rf15;;;;;; pendiente;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (obtener-fecha-vencimiento prestamo)
(define (avanzar dia mes dias-restantes) ;hago un wraper
    (cond [(= dias-restantes 0)(string-append 
           (if (< dia 10) (string-append "0" (number->string dia))(number->string dia)) "/"
           (if (< mes 10) (string-append "0" (number->string mes)) (number->string mes)))]
          [(= dia 30)(avanzar 1 (+ mes 1) (- dias-restantes 1))]
          [else (avanzar (+ dia 1) mes (- dias-restantes 1))])))(avanzar (string->number (substring (get-prestamo-fecha p) 0 2))
          (string->number (substring (get-prestamo-fecha p) 3 5)) (get-prestamo-dias p)))

;;rf16 
(define (calcular-dias-retraso fecha-vencimiento fecha-actual)
  (define (fecha-a-dias fecha)
    (define partes (string-split fecha "/"))
    (+ (string-a-number (car partes))(* (string-a-number (cadr partes)) 30)))
  (max 0 (- (fecha-a-dias fecha-actual) (fecha-a-dias fecha-vencimiento))));max evita num negativo



;;;Rf17
(define (calcular-multa prestamo fecha-actual tasa)
  (* (calcular-dias-retraso (obtener-fecha-vencimiento prestamo) fecha-actual) tasa))


;(calcular-multa p1 "18/04" 100) 



;;pendiente rf18
;;rf19
;;rf22
;;rf 24
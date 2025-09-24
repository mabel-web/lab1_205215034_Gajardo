#lang racket

(provide (all-defined-out))

;;TDA Usuario

;;REPRESENTACIÓN
;;lista (id nombre deuda prestamos estado)
;;id: int
;;nombre: string
;;deuda: int
;;prestamos: list
;;estado: booleano, suspendido (#f), activo (#t)


;;CONSTRUCTORES
;;Descripcion: esta funcion permite definir la reprensentacion de usuario con los parametros de entrada y condiciones iniciales requeridas.
;;Dom: id (int) X nombre (string)
;;Rec: Usuario (lista con estructura (id nombre deuda prestamos estado))
(define (crear-usuario id nombre)
   (list id
         nombre
         0 ;;deuda inicial
         null ;;sin prestamos
         #t)) ;;estado inicial activo

;;PERTENENCIA
;;Descripcion: esta funcion permite validad los parametros que pertenecen a usuario, devuelve true si la entrada es valida
;;Dom: id (int) X nombre (string)
;;Rec: Usuario (lista con estructura (id nombre deuda prestamos estado))
(define (usuario? u)
  (and (list? u)
       (= (length u) 5)        ; largo de la lista
       (integer? (car u))       ; id entero
       (string? (cadr u))       ; nombre string
       (integer? (caddr u))     ; deuda entero
       (list? (cadddr u))    ; lista prestamos
       (boolean? (list-ref u 4)))) ; estado activo o inactivo


;;SELECTORES
;;Descripción: esta funcion entrega el id del usuario
;;Dom: usuario consultado (list)
;;Rec: id del usuario consutado (int)
(define (get-usuario-id u)(car u))

;;Descripción: esta funcion entrega el nombre del usuario
;;Dom: usuario consultado (list)
;;Rec: nombre del usuario consutado (int)
(define (get-usuario-nombre u)(cadr u))

;;rf14?
;;Descripción: esta funcion entrega la deuda del usuario
;;Dom: usuario consultado (list)
;;Rec: deuda del usuario consutado (int)
(define (get-usuario-deuda u)(caddr u))

;;Descripción: esta funcion entrega los libros prestados del usuario
;;Dom: usuario consultado (list)
;;Rec: prestamos del usuario consutado (list)
(define (get-usuario-prestamo u)(cadddr u))

;;Descripción: esta funcion entrega el estado del usuario
;;Dom: usuario consultado (list)
;;Rec: estado del usuario consutado (bool)
(define (get-usuario-estado u)(list-ref u 4))

;; MODIFICADORES
;;El id y el nombre del usuario no debe cambiar por lo que no hare modificadores de estos

;;Descripción: esta funcion permite modificar la deuda de un usuario especifico
;;Dom: id (int) X nombre (string)
;;Rec: Usuario (nueva lista con estructura (id nombre nueva-deuda prestamos estado))
(define (set-deuda u nuevo-deuda)
  (list (get-usuario-id u)
        (get-usuario-nombre u)
        nuevo-deuda
        (get-usuario-prestamo u)
        (get-usuario-estado u)))
        

;;Descripción: esta funcion permite modificar el estado de un usuario especifico
;;Dom: id (int) X nombre (string)
;;Rec: Usuario (nueva lista con estructura (id nombre deuda prestamos nuevo-estado))
(define (set-estado u nuevo-estado)
  (list (get-usuario-id u)
        (get-usuario-nombre u)
        (get-usuario-deuda u)
        (get-usuario-prestamo u)
        nuevo-estado))

;;Descripción: esta funcion permite modificar los prestamos de un usuario especifico
;;Dom: id (int) X nombre (string)
;;Rec: Usuario (nueva lista con estructura (id nombre deuda nuevo-prestamos estado))
(define (set-prestamo u nuevo-prestamo)
  (list (get-usuario-id u)
        (get-usuario-nombre u)
        (get-usuario-deuda u)
        (cons nuevo-prestamo(get-usuario-prestamo u))
        (get-usuario-estado u)))




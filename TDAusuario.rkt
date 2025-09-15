#lang racket

;;TDA Usuario;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Capas del TDA

;;REPRESENTACIÓN

;;lista(id nombre deuda libros estado)

;;deuda = entero
;;libros = entero del numero de libros o ids??
;;estado = suspendido (#f), activo (#t)

;;CONSTRUCTORA
;;Dom: id (int) X nombre (string)
;;REc: usuario
(define (crear-usuario id nombre)
   (list id
         nombre
         0;;deuda
         #t;;estado
         null));;prestamo


;;VALIDACION
         
(display "===== BIBLIOTECA VIRTUAL - Script de Pruebas =====\n\n")

;; Crear usuarios
(define u1 (crear-usuario 1 "Jose"))
(define u (crear-usuario 2 "Carlos"))
(define u2 (crear-usuario "test" "random"))


(display u1)
(newline)
(display (car u1))
(newline)
(display (cadr u1))
(newline)
(display (caddr u1))
(newline)
(display (cadddr u1))
(newline)
(display (car (cdr (cdr (cdr (cdr u1)))))) (newline)

;;PERTENENCIA

;; devuelve true si la entrada es valida
(define (usuario? u)
  (and (list? u)
       (= (length u) 5)        ; largo de la lista
       (integer? (car u))       ; id entero
       (string? (cadr u))       ; nombre string
       (integer? (caddr u))     ; deuda entero
       (boolean? (cadddr u))    ; estado booleano
       (list? (list-ref u 4)))) ; prestamos lista


(if (usuario? u1)
    "usuario valido"
    "no es usuario") (newline)

(usuario? u1) (newline)
(usuario? u) (newline)
(usuario? u2) (newline)

;;SELECTORES

(define (usuario-id u)(car u))

(define (usuario-nombre u)(cadr u))

(define (usuario-deuda u)(caddr u))

(define (usuario-estado u)(cadddr u))

(define (usuario-prestamo u)(car u))

(usuario-nombre u) (newline)

;; MODIFICADORA
;; cambiar deuda
(define (set-deuda u nuevo-deuda)
  (crear-usuario (usuario-deuda u) nuevo-deuda))

(set-deuda u -200)

;;cambiar estado
(define (set-estado u nuevo-estado)
  (crear-usuario (usuario-estado u) nuevo-estado))

(set-estado u #f)


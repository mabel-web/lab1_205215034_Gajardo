#lang racket

;;TDA Crear Usuario
;;Capas del TDA
;;REPRESENTACIÓN
;;lista = (id, nombre, deuda, libros, estado)
;;id = entero
;;nombre = string
;;deuda = entero
;; libros = entero del numero de libros o ids??
;;estado = booleano

;;CONSTRUCTORA
;;Dom: id X nombre
;;REc: usuario
(define (crear-usuario id nombre)
   (list ========================================

;;PERTENENCIA


;;SELECTORES
(define (usuario-id u)(car u))

(define (usuario-nombre u)(cadr u))

(define (usuario-deuda u)(caddr u))

(define (usuario-deuda u)(cadddr u))

(define (usuario-libros u)(car u))

(define (usuario-estado u)(car u))

;;MODIFICADORES
;;estado del usuario, booleano
(define(usuario-suspendido u estado)
  (list (usuario-id)
        (usuario-nombre u)
        (usuario-deuda u)
        (usuario-libros u)
        (estado)))





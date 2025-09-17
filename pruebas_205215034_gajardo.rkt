#lang racket

(require "main.rkt")

;; Crear usuarios
(define u1 (crear-usuario 1 "Jose"))
(define u2 (crear-usuario 2 "Carlos"))


;; Crear libros (solo 2)
;(define l1 (crear-libro 101 "El Hobbit" "J.R.R. Tolkien"))
;(define l2 (crear-libro 102 "1984" "George Orwell"))
;
;
;;; Crear biblioteca con fecha inicial 01/01
;;; max-dias: 3, tasa-multa: 100/día, dias-max-retraso: 1
;(define b1 (crear-biblioteca '()    ; sin libros inicialmente
;                            '()    ; sin usuarios inicialmente
;                            '()    ; sin préstamos inicialmente
;                            2      ; max libros por usuario
;                            3      ; max días por préstamo
;                            100    ; tasa multa por día
;                            1000   ; deuda máxima
;                            1      ; días máx de retraso antes de suspensión
;                            "01/01")) ; fecha inicial del sistema
;
;;; Agregar libros
;(define b2 (agregar-libro b1 l1))
;(define b3 (agregar-libro b2 l2))
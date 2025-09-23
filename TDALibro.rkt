#lang racket

(provide (all-defined-out))

;;TDA Libro
;;REPRESENTACION
;;lista (id titulo autor)
;;id: int
;;titulo: string
;;autor: string

;;CONSTRUCTORES
;;Descripcion: esta funcion permite construir la reprensentacion del tda libro con los parametros de entrada y condiciones iniciales requeridas.
;;Dom: id (int) X titulo (string) X autor(string)
;;Rec: libro Usuario (lista con estructura (id titulo autor))
;;Tipo de recursion: no aplica
(define (crear-libro id titulo autor)
  (list id 
        (string-downcase titulo)
        (string-downcase autor)))

;;PERTENENCIA
;;Descripcion: esta funcion permite validar los parametros que pertenecen a libro, devuelve true si la entrada es valida, sino false
;;Dom: id (int) X titulo (string) X autor (string)
;;Rec: Usuario (lista con estructura (id nombre deuda prestamos estado))
(define (libro? l)
  (and (list? l)
       (= (length l) 3)        ; largo de la lista
       (integer? (car l))       ; id entero
       (string? (cadr l))       ; titulo string
       (string? (caddr l))))     ; autor string
      
;;SELECTORES
;;Descripción: esta funcion entrega el id del libro
;;Dom: libro consultado (list)
;;Rec: id del libro consutado (int)
;;RF 10 get-libro-id
(define (get-libro-id l)(car l))

;;Descripción: esta funcion entrega el titulo del libro
;;Dom: libro consultado (list)
;;Rec: titulo del libro consutado (int)
(define (get-libro-titulo l)(cadr l))

;;pendiente?????????????
;;Descripción: esta funcion entrega el autor del libro
;;Dom: libro consultado (list)
;;Rec: autor del libro consutado (int)
(define (get-libro-autor l)(caddr l))

;; MODIFICADORES
;;El id, titulo y autor de un libro no debe cambiar por lo que no hare modificadores de estos

(define l1 (crear-libro 100 "demian" "ANONIMO"))
(get-libro-titulo l1)
(get-libro-autor l1)
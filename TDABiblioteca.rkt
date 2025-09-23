#lang racket

(provide (all-defined-out))

;;TDA BIBLIOTECA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;REPRESENTACION
;;list (libros usuarios prestamos max-libros-usuario dias-max-prestamo tasa-multa-diaria limite-deuda-max dias-max-retraso fecha-inicial)


;;CONSTRUCTORA
;;dom: libros (Lista Libro) X usuarios (Lista Usuario) X prestamos (Lista Prestamo)
;;X max-libros-usuario (int) X dias-max-prestamo (int) X
;;tasa-multa-diaria (int) X limite-deuda-max (int) X dias-max-retraso (int) X fecha-inicial (string)
;;rec: biblioteca
(define (crear-biblioteca libros usuarios prestamos max-libros dias-max tasa-multa limite-deuda dias-retraso fecha-inicial)
  (list null
       null  
       null
       2
       3
       100 
       1000
       1
       "01/01"))

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

;;Descripción: esta funcion entrega la lista de usuaruios de la biblioteca
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

(define b1 (crear-biblioteca '() '() '() 2 3 100 1000 1 "01/01" ))


(get-biblioteca-fecha-inicial b1)
(get-biblioteca-libros b1)

;;MODIFICADORES
;;Descripcion: Busca usuario por ID. **Retorna null (lista vacía '()) si no encuentra el usuario**.
;;Dom: biblioteca (Biblioteca) X id (int)
;;rec: usuario o null
;;RF 8 obtener-usuario
(define (obtener-usuario biblioteca id)
  (define (obtener usuarios)
    (cond [(null? usuarios) #f] 
          [(= (get-usuario-id (car usuarios)) id) (car usuarios)] 
          [else (obtener (cdr usuarios))])) ; sigue buscando
  (buscar (get-biblioteca-usuarios biblioteca)))
      
;;rf 9
;;buscar-libro
;;Descripcion: Busca libro por "id", "titulo" o "autor". **Para "titulo"/"autor": búsqueda parcial case-insensitive (todo en minúsculas)**. **Para "id": búsqueda exacta**. **Si hay múltiples resultados, retorna el primero. Retorna '() si no encuentra**. **Debe usar funciones de orden superior**.
;;Dom: biblioteca (Biblioteca) X criterio (string) X valor (string/int)
;rec: libro o null
;;tipo de recursion: recursion natural
(define (buscar-libro biblioteca criterio valor)
  (define libros (get-biblioteca-libros b)) 
  (define (existe? texto parcial) ;aux que da t si el parcial esta en texto y f sino
    (define (buscar i)
      (cond [(> i (- (string-length texto) (string-length parcial))) #f] ;; caso base: si ya no hay espacio suficiente para comparar
            [(string=? (substring texto i (+ i (string-length parcial))) parcial) #t] ;substring
            [else (buscar (+ i 1))]));; llamada recursiva
    (buscar 0)) ;

  (define (igual-id? l)(= (get-libro-id l) valor))
  (define (igual-titulo? l)
    (existe? (string-downcase (get-libro-titulo l))
               (string-downcase valor)))
  (define (igual-autor? l)
    (existe? (string-downcase (get-libro-autor l))
               (string-downcase valor)))

  (cond [(equal? criterio "id")
         (if (null? (filter igual-id? libros))'()(car (filter igual-id? libros)))]
        [(equal? criterio "titulo")
         (if (null? (filter igual-titulo? libros))'()(car (filter igual-titulo? libros)))]
        [(equal? criterio "autor")(if (null? (filter igual-autor? libros))'()(car (filter igual-autor? libros)))]
        [else '()])

;;Descripción: obtiene la fecha actual del sistema desde la lista de la biblioteca
;;Dom: biblioteca
;;Rec: string
;;RF 11
;;get fecha actual
(define (get-fecha b)(get-biblioteca-fecha-inicial b))

;;RF12-libro-disponible
;;Descripción: verifica si un libro está disponible. **Retorna #f si el libro no existe o está prestado**.
;;Dom: biblioteca (Biblioteca) X id-libro (int)
;;Rec: booleano
;;tipo de recursion: natural
(define (libro-disponible? b id)
  (define (verificar libros)
    (cond [(null? libros) #f] 
          [(= (get-libro-id (car libros)) id)(get-libro-disponible (car libros))] ;encuentra id
          [else (buscar (cdr libros))]))
  (buscar (get-biblioteca-libros b)))


  ;;rf20
(debe-suspenderse? biblioteca id-usuario fecha-actual)


;;rf 21
(define (suspender-usuario b id-usuario)
  (define (recorrer usuarios)
    (cond [(null? usuarios) '()] 
          [(= (get-usuario-id (car usuarios)) id-usuario)
           (if (usuario-suspendido? (car usuarios));fr13
               (cons (car usuarios) (cdr usuarios)) ;;ya esta suspendido no cambio
               (cons (set-estado (car usuarios) #f) (cdr usuarios)))] ; suspender
          [else (cons (car usuarios) (recorrer (cdr usuarios)))])) 
  (list (get-biblioteca-libros b)
        (recorrer (get-biblioteca-usuarios b))
        (get-biblioteca-prestamos b)
        (get-biblioteca-max-libros b)
        (get-biblioteca-dias-max b)
        (get-biblioteca-tasa-multa b)
        (get-biblioteca-limite-deuda b)
        (get-biblioteca-dias-retraso b)
        (get-biblioteca-fecha-inicial b)))
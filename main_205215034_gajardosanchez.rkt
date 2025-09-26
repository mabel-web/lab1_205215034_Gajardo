#lang racket 


(require "TDAusuario_205215034_gajardosanchez.rkt"
         "TDALibro_205215034_gajardosanchez.rkt"
         "TDAPrestamo_205215034_gajardosanchez.rkt"
         "TDABiblioteca_205215034_gajardosanchez.rkt")

(provide (all-from-out "TDAusuario_205215034_gajardosanchez.rkt"
                       "TDALibro_205215034_gajardosanchez.rkt"
                       "TDAPrestamo_205215034_gajardosanchez.rkt"
                       "TDABiblioteca_205215034_gajardosanchez.rkt"))

(provide (all-defined-out))
;;RF 02 crear-usuario
;;Descripcion: esta funcion permite definir la reprensentacion de usuario con los parametros de entrada y condiciones iniciales requeridas.
;;Dom: id (int) X nombre (string)
;;Rec: Usuario (lista con estructura (id nombre deuda prestamos estado))
(define (crear-usuario id nombre)
   (list id
         nombre
         0 ;;deuda inicial
         null ;;sin prestamos
         #t)) ;;estado inicial activo

;;RF 03 crear-libro
;;Descripcion: esta funcion permite construir la reprensentacion del tda libro con los parametros de entrada y condiciones iniciales requeridas.
;;Dom: id (int) X titulo (string) X autor(string)
;;Rec: Libro (lista con (id titulo autor))
;;Tipo de recursion: no aplica
(define (crear-libro id titulo autor)
  (list id 
        (string-downcase titulo)
        (string-downcase autor)))

;;RF04 crear-prestamo
;;Descripcion: esta funcion permite definir la reprensentacion de tda prestamo con los parametros de entrada y condiciones iniciales requeridas.
;;Dom: id (int) X id-usuario (int) X id-libro (int) X fecha-prestamo (string) X dias-solicitados (int)
;;Rec: prestamo (lista con estructura (id id-usuario id-libro fecha-prestamo dias-solicitados))
(define (crear-prestamo id id-usuario id-libro fecha-prestamo dias-solicitados)
  (list id id-usuario id-libro fecha-prestamo dias-solicitados))

;;RF05 crear-biblioteca
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

;;RF06: agregar-libro
;;Descripcion: Agrega libro a la biblioteca. Si el ID ya existe, no se agrega y retorna la
;;biblioteca sin cambios. Se permiten libros con mismo título/autor pero diferente ID.
;;Dom: biblioteca (Biblioteca) X libro (Libro)
;;Rec: Biblioteca
;;Tipo de algoritmo/estrategia empleado: Recursion natural para recorrer la lista y 
;uso de wrapper para acceder un parametro mas especifico al buscar
(define (agregar-libro biblioteca libro)
 (define (recorrer-libros? lista-libros)
  (if (null? lista-libros)#f
      (if (= (get-libro-id (car lista-libros)) (get-libro-id libro))#t
          (recorrer-libros? (cdr lista-libros)))))
   (if (recorrer-libros? (get-biblioteca-libros biblioteca))
      biblioteca
      (list (cons libro (get-biblioteca-libros biblioteca)) ; agrega al inicio
            (get-biblioteca-usuarios biblioteca)
            (get-biblioteca-prestamos biblioteca)
            (get-biblioteca-max-libros biblioteca)
            (get-biblioteca-dias-max-prestamo biblioteca)
            (get-biblioteca-tasa-multa-diaria biblioteca)
            (get-biblioteca-limite-deuda biblioteca)
            (get-biblioteca-dias-max-retraso biblioteca)
            (get-biblioteca-fecha-inicial biblioteca))))


;RF07: registrar-usuario
;;Descripcion: Registra usuario en el sistema,al final de la lista, usuario inicia con deuda 0 y estado activo.
;;Dom: biblioteca (Biblioteca) X usuario (Usuario)
;Rec: Biblioteca
;;Tipo de Recursion: recursion natural, wrapper de la funcion para acceder a los usuarios y compararlos en la lista con uso de cond
(define (registrar-usuario biblioteca usuario)
  (define (registrar usuarios) ;;wrapper
    (cond [(null? usuarios) (list usuario)]                  
          [(= (get-usuario-id (car usuarios)) (get-usuario-id usuario)) usuarios]   
          [else (cons (car usuarios) (registrar (cdr usuarios)))])) ; llamada recursiva natural
  (list (get-biblioteca-libros biblioteca)
        (registrar (get-biblioteca-usuarios biblioteca))
        (get-biblioteca-prestamos biblioteca)
        (get-biblioteca-max-libros biblioteca)
        (get-biblioteca-dias-max-prestamo biblioteca)
        (get-biblioteca-tasa-multa-diaria biblioteca)
        (get-biblioteca-limite-deuda biblioteca)
        (get-biblioteca-dias-max-retraso biblioteca)
        (get-biblioteca-fecha-inicial biblioteca)))

;;RF 8 obtener-usuario
;;Descripcion: Busca usuario por ID. **Retorna null (lista vacía '()) si no encuentra el usuario**.
;;Dom: biblioteca (Biblioteca) X id (int)
;;rec: usuario o null
;;Estrategia: ocupo recursion natural y busco por fuerza bruta los elementos de la lista de usuarios (wrapper)
(define (obtener-usuario biblioteca id)
  (define (obtener usuarios)
    (cond [(null? usuarios) #f] 
          [(= (get-usuario-id (car usuarios)) id) (car usuarios)] 
          [else (obtener (cdr usuarios))])) ; sigue buscando
  (obtener (get-biblioteca-usuarios biblioteca)))

;;RF 9 buscar-libro
;;Descripcion: Busca libro por "id", "titulo" o "autor". **Para "titulo"/"autor": búsqueda parcial todo en minúsculas. Para "id": búsqueda exacta. **Si hay múltiples resultados, retorna el primero. Retorna '() si no encuentra**. **Debe usar funciones de orden superior**.
;;Dom: biblioteca (Biblioteca) X criterio (string) X valor (string/int)
;;Rec: libro o null
;;tipo de recursion: bisqueda lineal con uso de funcion de orden superior
(define (buscar-libro biblioteca criterio valor)
 ((lambda (libros-coincidentes) 
  (if (null? libros-coincidentes) 
      '()
      (car libros-coincidentes)));devuelvo el primero
  (filter (lambda (libro);recorro la lista y co el filter busco el valor en lambda q es libro
  (cond [(string-ci=? criterio "id")(= (get-libro-id libro) valor)];string-ci compara strings
        [(string-ci=? criterio "titulo")(string-contains? (string-downcase (get-libro-titulo libro))(string-downcase valor))]
        [(string-ci=? criterio "autor")(string-contains?(string-downcase (get-libro-autor libro))(string-downcase valor))]
        [else #f]))(get-biblioteca-libros biblioteca))))


;;RF 10 get-libro-id
;;Descripcion:
;;Dom: libro (Libro)
;;Rec: int
(define (get-libro-id l)(car l))

;;RF 11 get-fecha-actual
;;Descripción: obtiene la fecha actual del sistema desde la lista de la biblioteca
;;Dom: biblioteca (Biblioteca)
;;Rec: string
(define (get-fecha b)(get-biblioteca-fecha-inicial b))

;;RF12 libro-disponible
;;Descripción: verifica si un libro está disponible. **Retorna #f si el libro no existe o está prestado**.
;;Dom: biblioteca (Biblioteca) X id-libro (int)
;;Rec: booleano
;;tipo de recursion: natural
;;Tipo de algoritmo: uso de recursion natural pata buscar dentro de lista (busqueda lineal)
(define (libro-disponible? biblioteca id-libro)
  (define (verificar-disponibilidad prestamos) 
    (cond [(null? prestamos) #t] 
          [(= (get-prestamo-id-libro (car prestamos)) id-libro) #f] ;encuentra id
          [else (verificar-disponibilidad (cdr prestamos))]))
    (verificar-disponibilidad (get-biblioteca-prestamos biblioteca)))

;;RF 13 usuario-suspendido?
;;Descripción Verifica estado de suspensión del usuario. **Un usuario está suspendido si su campo suspendido es #t**.
;;Dom: usuario (Usuario)
;;Rec: boolean
(define (usuario-suspendido? usuario)
  (if (get-usuario-estado usuario)#f #t)) ;f si esta activo osea no suspendido y t si esta suspendido (en#f)
  
;;RF 14 obtener-deuda
;;Descripción: retorna la deuda acumulada del usuario
;;Dom: usuario (Usuario)
;;Rec: int
(define (obtener-deuda usuario)(get-usuario-deuda usuario))

;;RF 15 obtener-fecha-vencimiento
;;Descripcion:Calcula fecha de vencimiento sumando días solicitados a fecha de prestamo.
;;Dom: prestamo (Prestamo)
;;Rec: string
;;Tipo de algoritmo/estrategia: uso de wrapper para inicializar los argumentos de la fecha y recursion natural hasta el caso base
(define (obtener-fecha-vencimiento prestamo)
  (define (avanzar dia mes dias-restantes);wraper
    (cond [(= dias-restantes 0)(string-append
           (if (< dia 10) (string-append "0" (number->string dia)) (number->string dia)) "/"
           (if (< mes 10) (string-append "0" (number->string mes)) (number->string mes)))]
          [(= dia 30) (avanzar 1 (+ mes 1) (- dias-restantes 1))]
          [else (avanzar (+ dia 1) mes (- dias-restantes 1))]))
  (avanzar
   (string->number (substring (get-prestamo-fecha prestamo) 0 2))
   (string->number (substring (get-prestamo-fecha prestamo) 3 5))
   (get-prestamo-dias-solicitados prestamo)))

;;RF16 calcular-dias-retraso
;;Descripcion: Calcula días de retraso entre fecha vencimiento y fecha actual, retorna 0 si no hay retraso.
;;Dom: fecha-vencimiento (string) X fecha-actual (string)
;;Rec: int
;;Tipo de algoritmo/estrategia: wrapper para separar los datos de la fecha y operaciones aritmeticas
(define (calcular-dias-retraso fecha-vencimiento fecha-actual)
  (define (fecha-a-dias fecha)
    (define partes (string-split fecha "/"))
    (+ (string->number (car partes))(* (string->number (cadr partes)) 30)))
  (max 0 (- (fecha-a-dias fecha-actual) (fecha-a-dias fecha-vencimiento))));max evita num negativo

;;;RF17 calcular-multa
;;Descripcion: Calcula multa multiplicando días de retraso por tasa diaria, si no hay retraso, multa es 0
;;Dom: prestamo (Prestamo) X fecha-actual (string) X tasa-multa-diaria (int)
;;Rec: int
;;Tipo de algoritmo/estrategia: reutilizacion de funciones anteriores y calcular con aritmetica
(define (calcular-multa prestamo fecha-actual tasa)
  (* (calcular-dias-retraso (obtener-fecha-vencimiento prestamo) fecha-actual) tasa))

;;RF18 tomar-prestamo
;;Descripcion: Permite que el usuario tome un libro prestado, verifica si cumple con las condiciones de libro disponible, usuario no suspendido,
;;no excede límite de libros, días solicitados no exceden máximo, deuda no excede límite. Si la verificacion falla, retorna biblioteca sin cambios
;;Dom: biblioteca (Biblioteca) X id-usuario (int) X id-libro (int) X dias-solicitados (int) X fecha-actual (string)
;;Rec: Biblioteca
;;Tipo de algoritmo/estrategia: uso de cond para validar condiciones, funciones de orden superior donde se intenta llamar recursivamente (natural)
(define (tomar-prestamo biblioteca id-usuario id-libro dias fecha-actual)
  (cond [(or (null? (buscar-libro biblioteca "id" id-libro))
        (not (libro-disponible? biblioteca id-libro))) biblioteca]
        [(or (null? (obtener-usuario biblioteca id-usuario))(usuario-suspendido? (obtener-usuario biblioteca id-usuario))) biblioteca]
        [(>= (length (get-usuario-prestamo (obtener-usuario biblioteca id-usuario)))(get-biblioteca-max-libros biblioteca)) biblioteca]
        [(> dias (get-biblioteca-dias-max-prestamo biblioteca)) biblioteca]
        [(>= (get-usuario-deuda (obtener-usuario biblioteca id-usuario))(get-biblioteca-limite-deuda biblioteca)) biblioteca]
        [else ;no hay restricciones
            (list (get-biblioteca-libros biblioteca) ;ka lista no cambia
            (map (lambda (usuarios) ; agrego el nuevo prestamo a la lista del usuario
                 (if (= (get-usuario-id usuarios) id-usuario)
                     (set-prestamo usuarios (crear-prestamo id-libro id-usuario id-libro fecha-actual dias))
                     usuarios)) 
                     (get-biblioteca-usuarios biblioteca))
                (cons (crear-prestamo id-libro id-usuario id-libro fecha-actual dias)(get-biblioteca-prestamos biblioteca));agrega el nuevo prestamo al registro global
                (get-biblioteca-max-libros biblioteca)
                (get-biblioteca-dias-max-prestamo biblioteca)
                (get-biblioteca-tasa-multa-diaria biblioteca)
                (get-biblioteca-limite-deuda biblioteca)
                (get-biblioteca-dias-max-retraso biblioteca)
                (get-biblioteca-fecha-inicial biblioteca))]))

;;rf19
;;Descripcion: Devuelve libro y procesa multas. Calcula multa por retraso, suma a deuda del
;;usuario, marca libro disponible. Si deuda total excede limite, suspende usuario al
;;automaticamente. Usuario suspendido SÍ puede devolver**.
;;Dom:biblioteca (Biblioteca) X id-usuario (int) X id-libro (int) X fecha-actual (string)
;;Rec: Biblioteca
;;Tipo de algoritmo/estrategia
;define (devolver-libro biblioteca id-usuario id-libro fecha-actual)
;(define prestamos (get-biblioteca-prestamos biblioteca))
;(define usuarios (get-biblioteca-usuarios biblioteca))
;(define (actualizar-usuario usuario)
;         (if (= (get-usuario-id usuario) id-usuario)
;           (set-prestamo (set-deuda usuario 
;           (+ (get-usuario-deuda usuario)


;;RF 20 debe-suspenderse?
;;Descripcion: Verifica si usuario debe suspenderse automaticamente, segun criterios deuda > límite máximo o
;;tiene algún libro con retraso > días máximos permitidos. Si el
;;usuario debe ser suspendido retorna #t caso contrario #f
;;Dom:biblioteca (Biblioteca) X id-usuario (int) X fecha-actual (string)
;;Rec: Boolean
;;Tipo de algoritmo/estrategia: recursion natural dentro de funcion de orden susperior, validaciones logicas
(define (debe-suspenderse? biblioteca id-usuario fecha-actual)
  (or (> (get-usuario-deuda (obtener-usuario biblioteca id-usuario))
      (get-biblioteca-limite-deuda biblioteca));deuda supera al limite
   (not (null? (filter (lambda (prestamo);prestamo vencido
                   (and (= (get-prestamo-id-usuario prestamo) id-usuario)
                        (> (calcular-dias-retraso (obtener-fecha-vencimiento prestamo) fecha-actual)
                           (get-biblioteca-dias-max-retraso biblioteca))))
                  (get-biblioteca-prestamos biblioteca))))))
        

;;RF 21
;;Descripcion: Suspende usuario manualmente cambiando su estado a #t. Si el usuario se encuentra
;;suspendido entonces el usuario se mantiene intacto
;;Dom: biblioteca (Biblioteca) X id-usuario (int)
;;Rec: Biblioteca
;;Tipo de algoritmo/estrategia: recursion natural para recorrer la lista
(define (suspender-usuario biblioteca id-usuario)
  (define (recorrer usuarios)
    (cond [(null? usuarios) '()] 
          [(= (get-usuario-id (car usuarios)) id-usuario)
           (if (usuario-suspendido? (car usuarios));fr13
               (cons (car usuarios) (cdr usuarios)) ;;ya esta suspendido no cambio
               (cons (set-estado (car usuarios) #f) (cdr usuarios)))] ; suspender
          [else (cons (car usuarios) (recorrer (cdr usuarios)))])) 
  (list (get-biblioteca-libros biblioteca)
        (recorrer (get-biblioteca-usuarios biblioteca))
        (get-biblioteca-prestamos biblioteca)
        (get-biblioteca-max-libros biblioteca)
        (get-biblioteca-dias-max-prestamo biblioteca)
        (get-biblioteca-tasa-multa-diaria biblioteca)
        (get-biblioteca-limite-deuda biblioteca)
        (get-biblioteca-dias-max-retraso biblioteca)
        (get-biblioteca-fecha-inicial biblioteca)))


;;(define (procesar-dia biblioteca)
;; (define (avanzar-dia fecha))
;;    (cond [(string-append
;;           (if (< dia 10) (string-append "0" (number->string dia)) (number->string dia)) "/"
;;           (if (< mes 10) (string-append "0" (number->string mes)) (number->string mes)))]
;;          [(= dia 30) (avanzar-dia 1 (+ mes 1) (- dias-restantes 1))]
;;          [else (avanzar (+ dia 1) mes (- dias-restantes 1))]))
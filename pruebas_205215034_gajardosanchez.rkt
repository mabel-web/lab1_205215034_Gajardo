#lang racket

(require "main_205215034_gajardosanchez.rkt")

(display "===== BIBLIOTECA VIRTUAL - Script de Pruebas =====\n\n")
;; Crear usuarios
(define u1 (crear-usuario 1 "Jose"))
(define u2 (crear-usuario 2 "Carlos"))


;Crear libros (solo 2)
(define l1 (crear-libro 101 "El Hobbit" "J.R.R. Tolkien"))
(define l2 (crear-libro 102 "1984" "George Orwell"))

(display "\n===== Libros:  =====\n\n")
(display l1)(newline)
(display l2)(newline)

;Crear biblioteca con fecha inicial 01/01
;max-dias: 3, tasa-multa: 100/día, dias-max-retraso: 1
(define b1 (crear-biblioteca '()    ; sin libros inicialmente
                            '()    ; sin usuarios inicialmente
                            '()    ; sin préstamos inicialmente
                            2      ; max libros por usuario
                            3      ; max días por préstamo
                            100    ; tasa multa por día
                            1000   ; deuda máxima
                            1      ; días máx de retraso antes de suspensión
                            "01/01")) ; fecha inicial del sistema


(display "\n===== Biblioteca:  =====\n\n")
(display b1)(newline)


;;; Agregar libros
(define b2 (agregar-libro b1 l1))
(define b3 (agregar-libro b2 l2))


(display "\n===== Libros agregados:  =====\n\n")
(display b2)(newline)
(display b3)(newline)

(display "\n===== Registrar usuarios:  =====\n")
;;; Registrar usuarios
(define b4 (registrar-usuario b3 u1))
(define b5 (registrar-usuario b4 u2))
(display "\nUsuarios registrados:\n")
(get-biblioteca-usuarios b5) (newline)

(display "Sistema configurado: 2 libros, 2 usuarios\n")
(display "Configuración: máx 3 días prestamo, multa $100/día, suspensión tras 1 día retraso\n")
(display "Fecha inicial: ") (display (get-fecha b5)) (newline)
(newline)

(display "\n===== Tomar prestamos:  =====\n")

(display "=== DÍA 01/01 ===\n")
(display "Jose toma prestado 'El Hobbit' por 3 días\n")
(define b6 (tomar-prestamo b5 1 101 3 "01/01"))
(display "\nPréstamos en la biblioteca:\n")
(display (get-biblioteca-prestamos b6))(newline)
(display "Préstamo realizado - debe devolver el 04/01\n\n")

(display "\n===== Fechas de vencimiento y multas:  =====\n")
(define prestamo-jose (car (get-biblioteca-prestamos b6)))

(display "fecha de vencimiento del prestamo de Jose:\n")
(display (obtener-fecha-vencimiento prestamo-jose)) (newline)

(display "multa si se devuelve en 07/01 (tasa 100/día):\n")
(display (calcular-multa prestamo-jose "07/01" 100)) (newline)

(display "multa si se devuelve en 04/02 (tasa 100/día):\n")
(display (calcular-multa prestamo-jose "04/02" 100)) (newline)
(newline)

(display "\n===== Suspensiones:  =====\n")

(display "jose debe suspenderse el 05/01?\n")
(display (debe-suspenderse? b6 1 "05/01")) (newline) ;no, el 4

;voy a probar otras funciones
;; RF08: obtener-usuario
(display "obtener-usuario:\n")
(display (obtener-usuario b5 1)) (newline)
(display (obtener-usuario b5 2)) (newline)

(display "buscar-libro por id=101:\n")
(display (buscar-libro b5 "id" 101)) (newline)
(display " buscar-libro por id que no existe:\n")
(display (buscar-libro b5 "id" 110)) (newline)
(display "buscar-libro por titulo y busqqueda parcial:\n")
(display (buscar-libro b5 "titulo" "hob")) (newline)
(display "prueba buscar-libro por busqueda parcial del autor\n")
(display (buscar-libro b5 "autor" "j")) (newline)
(display "prueba buscar-libro por busqueda parcial del autor no registrado\n")
(display (buscar-libro b5 "autor" "f")) (newline)

;; RF18: usuario-suspendido?
(display "prueba usuario-suspendido?=================== :\n")
(display (usuario-suspendido? u1)) (newline); f es no, t es si

;; RF14: obtener-deuda
(display "Prueba obtener-deuda:\n")
(display (obtener-deuda u1)) (newline)

(display "calcular-dias-retraso entre 04/01 y 06/01:\n")
(display (calcular-dias-retraso "04/01" "06/01")) (newline)

(display "Prueba calcular-dias-retraso entre 04/01 y 02/01:\n")
(display (calcular-dias-retraso "04/01" "02/01")) (newline)



;; ====== Simulo devolución ============= ;;;;;;;;;;devolver-libro no funciono
;(display "jose devuelve el hobbit el 04/01 ===")
;(define b7 (devolver-libro b6 1 101 "04/01"))

;(display "prestamos después de devolución:\n");
;(display (get-biblioteca-prestamos b6)) (newline)

;(display "usuarios después de devolución (deuda actualizada):\n")
;(display (get-biblioteca-usuarios b6)) (newline)

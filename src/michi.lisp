
;CONSTANTES

(defvar *pc_simbolo* "X")
(defvar *oponente_simbolo* "O")

;las 8 maneras de completar una linea (3 horizontalmente, 3 verticalmente, y 2 en la diagonal)
(defvar *tripletas* '(  (1 2 3) (4 5 6) (7 8 9) (1 4 7)
                        (2 5 8) (3 6 9) (1 5 9) (3 5 7) )
)

;instanciamos las contantes para el valor correpondiente a la pc y el oponente
(defvar *pc* 10)
(defvar *oponente* 1)

;VARIABLE GLOBALES

;FUNCIONES

;crea un tablero vacio
(defun crear-tablero ()
  (list 'tablero 0 0 0 0 0 0 0 0 0)
)

;convierte la numeracion del tablero a los simbolos del juego
(defun convertir-a-letras (v)
  (cond 
    ((equal v 1) *oponente_simbolo*)
    ((equal v 10) *pc_simbolo*)
    (t " ")
  )
)

(defun imprimir-fila (x y z)
  (format t "~&  ~A | ~A | ~A " (convertir-a-letras x) (convertir-a-letras y) (convertir-a-letras z))
)

(defun imprimir-tablero (tablero)
  (format t "~%")
  (imprimir-fila (nth 1 tablero) (nth 2 tablero) (nth 3 tablero))
  (format t "~& ---+---+---")
  (imprimir-fila (nth 4 tablero) (nth 5 tablero) (nth 6 tablero))
  (format t "~& ---+---+---")
  (imprimir-fila (nth 7 tablero) (nth 8 tablero) (nth 9 tablero))
  (format t "~%~%")
)

;realiza el movimiento de un jugador en la posicion indicada del tablero
(defun efectuar-movimiento (jugador pos tablero)
  (setf (nth pos tablero) jugador)
  tablero
)

;devuelve la suma de los numeros contenidos por una tripleta
(defun suma-de-tripleta (tablero tripleta)
  (+ (nth (car tripleta) tablero) (nth (cadr tripleta) tablero) (nth (caddr tripleta) tablero))
)

;calcula la suma de cada tripleta y las devuelve en una lista
(defun calcula-sumas (tablero)
  (mapcar #'(lambda (tripleta) (suma-de-tripleta tablero tripleta)) *tripletas*)
)

;verifica si en tablero actual hay un ganador, condicion de parada cuando se completa una linea
(defun ganador-p (tablero)
  (let ((sumas (calcula-sumas tablero)))
    (or (member (* 3 *pc*) sumas) (member (* 3 *oponente*) sumas))
  )
)

;DEFINICION DEL JUEGO

;definimos los estados del juego
'menu 'turno-pc 'turno-op 'ganador-pc 'ganador-op 'empate 'fin

;variable globales del juego

; q: es el estado actual del juego
(setq q nil)
; ta: es el tablero actual
(setq ta nil)
; op: opciones del menu (0: sin seleccionar, 1: facil, 2:intermedio, 3:avanzado)
(setq op nil)

;funciones del juego

(defun init ()
  (setq q 'menu)
  (setq ta (crear-tablero))
  (setq op 0)
)

(defun mostrar-menu ()
  (format t "~%~%")
  (format t "+----------------+~%")
  (format t "|      MICHI     |~%")
  (format t "+----------------+~%")
  (format t " [1] Facil        ~%")
  (format t " [2] Intermedio   ~%")
  (format t " [3] Avanzado     ~%")
  (format t " [0] Salir        ~%")
  (format t "+----------------+~%")
)
(defun opcion()
  (format t " Opcion > ")
  (setq op (read))
  (if (or (= op 0) (= op 1) (= op 2) (= op 3))
    op
    (opcion)
  )
)

(defun movimiento-op()
  (format t " Tu movimiento > ")
  (read)
)

(defun limpiar-pantalla()
  (format t "~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~T  ")
)

(defun pintar ()
  (limpiar-pantalla)
  (cond 
    ((eq q 'menu) (mostrar-menu))
    ((eq q 'turno-op) (imprimir-tablero ta))
  )
)

(defun actualizar ()  
  (cond 
    ((eq q 'menu) 
      (opcion)
      (cond
        ((equal op 1) (setq q 'turno-op))
        ((equal op 0) (setq q 'fin))
      )
    )
    ((eq q 'turno-op)
      (movimiento-op)
    )
  )
)

(defun juego ()
  (init)
  (do ()
    ((eq q 'fin) nil)
    (pintar)
    (actualizar)
  )
)

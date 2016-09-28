
;CONSTANTES

(defvar *pc_simbolo* "X")
(defvar *oponente_simbolo* "O")

;las 8 maneras de completar una linea (3 horizontalmente, 3 verticalmente, y 2 en la diagonal)
(defvar *tripletas* '(  (1 2 3) (4 5 6) (7 8 9) (1 4 7)
                        (2 5 8) (3 6 9) (1 5 9) (3 5 7) )
)

(defvar *pc* 10)
(defvar *oponente* 1)

(defun juego ()
  (init)
  (do ()
    ((eq turno 'fin) nil)
    (imprimir-tablero ta)
    (actualizar)
    (cambiar-turno)
    (ganador-p ta)
  )
  (imprimir-tablero ta)
  (print resultado)
)

(defun init ()
  (setq ta (crear-tablero))
  (setq turno 'turno-op)
  (setq resultado 'empate)
  (setq mov-disp '(1 2 3 4 5 6 7 8 9))
  (mostrar-menu)
  (opcion)
)

(defun crear-tablero ()
  (list 'tablero 0 0 0 0 0 0 0 0 0)
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

(defun limpiar-pantalla()
  (format t "~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~T  ")
)

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
  (limpiar-pantalla)
  (format t "~%")
  (imprimir-fila (nth 1 tablero) (nth 2 tablero) (nth 3 tablero))
  (format t "~& ---+---+---")
  (imprimir-fila (nth 4 tablero) (nth 5 tablero) (nth 6 tablero))
  (format t "~& ---+---+---")
  (imprimir-fila (nth 7 tablero) (nth 8 tablero) (nth 9 tablero))
  (format t "~%~%")
)

(defun actualizar ()  
  (cond 
    ((eq turno 'turno-op)
      (efectuar-movimiento 10 (movimiento-op) ta)
    )
    ((eq turno 'turno-pc)
      (efectuar-movimiento 1 (movimiento-pc) ta)
    )
  )
)

(defun cambiar-turno()
  (if (eq turno 'turno-op)
    (setq turno 'turno-pc)
    (setq turno 'turno-op)
  )
)

(defun movimiento-op()
  (format t " Tu movimiento > ")
  (setq mov (read))
  (if (member mov mov-disp)
      (setq mov-disp (remove mov mov-disp))
      (movimiento-op)
  )
  mov
)

(defun movimiento-pc()
  (format t " Tu movimiento > ")
  (setq mov (random 10))
  (if (AND (member mov mov-disp) (not (eq mov 0)))
      (setq mov-disp (remove mov mov-disp))
      (movimiento-pc)
  )
  mov
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
    (if (member (* 3 *pc*) sumas)
        (setq turno 'fin resultado 'ganador-maquina)
        NIL    
    )
    (if (member (* 3 *oponente*) sumas)
        (setq turno 'fin resultado 'ganador-humano)
        NIL    
    )
    (if (NULL mov-disp)
        (setq turno 'fin resultado 'empate)
        NIL    
    )
  )
)
;AUTOR: JHONATAN JOSE MARIA IBAÑEZ TACCSI 

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
  (format t "~&    ~A | ~A | ~A " (convertir-a-letras x) (convertir-a-letras y) (convertir-a-letras z))
)

(defun imprimir-tablero (tablero)
  (format t "~%")
  (imprimir-fila (nth 1 tablero) (nth 2 tablero) (nth 3 tablero))
  (format t "~&   ---+---+---")
  (imprimir-fila (nth 4 tablero) (nth 5 tablero) (nth 6 tablero))
  (format t "~&   ---+---+---")
  (imprimir-fila (nth 7 tablero) (nth 8 tablero) (nth 9 tablero))
  (format t "~%~%")
)

;realiza el movimiento de un jugador en la posicion indicada del tablero
(defun efectuar-movimiento (jugador pos tablero)
  (let ((aux (copy-list tablero)))
    (when (and (>= pos 1) (<= pos 9) (zerop (nth pos aux)))
      (setf (nth pos aux) jugador)
      aux
    )
  )
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

(defun ganador-pc (tablero)
  (let ((sumas (calcula-sumas tablero)))
    (member (* 3 *pc*) sumas)
  )
)

(defun ganador-oponente (tablero)
  (let ((sumas (calcula-sumas tablero)))
    (member (* 3 *oponente*) sumas)
  )
)

;valida si el tablero esta lleno
(defun tablero-total-p (tablero)
  (not (member 0  tablero))
)

;ESTRATEGIA ALEATORIA
(defun estrategia-aleatoria (tablero)
  (let ( (pos (+ 1 (random 9))) )
    (if (zerop (nth pos tablero)) 
      pos 
      (estrategia-aleatoria tablero)
    )
  )
)

;ESTRATEGIA PRIMERO EL MEJOR

;verifica si casilla esta vacia
;genera c/ sucesor con la fc efectuar-movimiento
;forma una lista con resultado
(defun generar-sucesores(tablero jugador)
  (mapcan
    #'(lambda (m) 
        (if (zerop (nth m tablero)) 
          (list (efectuar-movimiento jugador m tablero)) 
          nil
        )
      )
      '(1 2 3 4 5 6 7 8 9)
  )
)

(defun lineas-abiertas (pos)
  (let (
      (sumas (calcula-sumas pos))
      (abiertasX 0)
      (abiertasO 0)
    )
    (dolist (s sumas)
      (cond 
        ((equal 0 s)  (setf abiertasX (+ abiertasX 1)) 		
                      (setf abiertasO (+ abiertasO 1)))
        ((equal 1 s)  (setf abiertasO (+ abiertasO 1)))
        ((equal 2 s)  (setf abiertasO (+ abiertasO 1)))
        ((equal 10 s) (setf abiertasX (+ abiertasX 1)))
        ((equal 20 s) (setf abiertasX (+ abiertasX 1)))
      )
    )
    (setf res (- abiertasX abiertasO))
  )
)

(defun feval-primero-mejor (tablero)
  (cond 
    ((ganador-pc tablero) 1000)
    ((ganador-oponente tablero) -1000)
    (t (lineas-abiertas tablero))
  )
)
;Pasos q se efectuan en esta fc:
; -aplica feval para evaluar c/ sucesor  
; -forma sublistas con cada sucesor y su valor de evaluación
(defun sucesores-con-evaluacion (sucesores)
  (mapcar #'(lambda (n) (list n (feval-primero-mejor n))) sucesores)
)

;Pasos q se efectuan en esta fc:
; -genera los sucesores con gen-mov()
; -evalua los sucesores con sucesores-con-evaluacion()
; -ordena los sucesores
; -obtiene la posicion i del 10 insertado en el sucesor seleccionado (mejor)
; El i retornado sera pos en fc movimiento-pc
(defun estrategia-primero-el-mejor (tablero jugador)
  (let* (
      (sucesores (generar-sucesores tablero jugador)) 
      (sucesores-peso (sucesores-con-evaluacion sucesores))
    )
    (sort sucesores-peso #'(lambda (x y) (>= (second x) (second y))))
    (do ((i 1 (+ i 1))) 
      ((/= (nth i tablero) (nth i (caar sucesores-peso))) i)
    )
  )
)

;ESTRATEGIA MINIMAX
(defun estrategia-minimax (tablero jugador)
  (let* (
	      (sucesores (generar-sucesores-minimax-nivel-2 tablero jugador 0)) 
	      (sucesores-peso (sucesores-con-evaluacion-minimax sucesores))
	    )
  		
		(format t "~%~%NIVEL MIN: ~S" sucesores-peso )
	    (setq jugada (algoritmo-minimax sucesores-peso 1))
	   (do ((i 1 (+ i 1))) 
	      ((/= (nth i tablero) (nth i jugada)) i)
               ;(format t "~%~S:~S" (nth i tablero) (nth i jugada))
	    )
  )
)

(defun generar-sucesores-minimax-nivel-2 (nodos_x_nivel jugador nivel)
  (cond ( (= nivel 0)   ;en el nivel 0, solo estara el tablero inicial
           (  generar-sucesores-minimax-nivel-2    
              (generar-sucesores nodos_x_nivel jugador) 
              *oponente*
              (+ nivel 1)
           )
        )
        (  (/= nivel 2)
           ( generar-sucesores-minimax-nivel-2   
             ( mapcar               ;por cada nodo de un nivel se generan los sucesores
               #'(  lambda (nodo_0)
                    (cons nodo_0 (generar-sucesores nodo_0 jugador))
                 )
               nodos_x_nivel
             )
             jugador 
             (+ nivel 1)
           )
        )
        (  t nodos_x_nivel
        )
  )
)

( defun sucesores-con-evaluacion-minimax (sucesores)
  ( mapcar               ;a cada nodo del nivel 2 se le asocia un valor minimax 
               #'(  lambda (nodos_1) ;nodos -> (n1_1 a b ) , (n1_2 c d) , (n1_3 e f)
                    (cons (car nodos_1) (sucesores-con-evaluacion (cdr nodos_1)))
                 )
               sucesores ; sucesores -> ((n1_1 a b ) (n1_2 c d) (n1_3 e f))
  )
)

( defun algoritmo-minimax (sucesores nivel)
   (cond (  (= nivel 1)   ;NIVEL MIN
            (   algoritmo-minimax 
                (mapcar ; -> ((n1_1 2) (n1_2 1) (n1_3 0))
                    #'(     lambda (nodos_1)  ; nodos_1 -> (n1_1 (a 2)(b 3)) 
                             (setq v_min (cadadr nodos_1) n_min (cadr nodos_1)) ;v_min=2 , n_min=(a 2) 
                             (mapcan
                                  #'( lambda (nodos_0)  ; nodos_0 -> (b 3)
                                        (if (< (nth 1 nodos_0) v_min) 
                                            (setq v_min (nth 1 nodos_0) n_min nodos_0)
                                        )
                                    )
                                    (cddr nodos_1)
                             )
                             (list (car nodos_1) v_min) ; nodos_1->(n1_1 2)
                      )
                     sucesores ;sucesores -> ( (n1_1 (a 2)(b 3)) (n1_2 (c 3)(d 1)) (n1_3 (e 0)(f 1)) )
                )
                (- nivel 1)
            )
        )
         (  t  ;NIVEL MAX (NIVEL 0)
             (format t "~%~%NIVEL MAX: ~S" sucesores )
              (setq v_max (cadar sucesores) n_max (car sucesores)) ;v_max=2 , n_max=n1_1 
               (mapcan
                    #'( lambda (nodos_0)  ; ->  nodos_0 -> (n1_2 1), (n1_3 0)
			;(format t "~%~S:~S" (nth 1 nodos_0) v_max)
                          (if (> (nth 1 nodos_0) v_max) 
                              (setq v_max (nth 1 nodos_0) n_max nodos_0)
                          )
                      )
                      (cdr sucesores)  
               )
               (format t "~%~%JUGADA: ~S  -> " (car n_max))
               (car n_max) ; ->n1_1
        )
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
  (limpiar-pantalla)
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

;utilizamos la estrategia elegida en el menu para el movimiento de la pc
(defun estrategia-elegida()
  (cond
    ((= op 1) (estrategia-aleatoria ta))
    ((= op 2) (estrategia-primero-el-mejor ta *pc*))
    ((= op 3) (estrategia-minimax ta *pc*))
  )
)

;movimiento del oponente
(defun movimiento-op()
  (format t " Tu movimiento > ")
  (do ( (aux (efectuar-movimiento *oponente* (read) ta)) )
    (aux aux)
    (format t " Re-ingresar movimiento > ")
    (setq aux (efectuar-movimiento *oponente* (read) ta))
  )
)

;movimiento de la pc
(defun movimiento-pc()
  (format t " Maquina > ")
  (let ( (pos (estrategia-elegida)) )
    (format t "~A " pos)
    (sleep 1)
    (efectuar-movimiento *pc* pos ta)
  )
)

;continuar o salir del juego
(defun continuar-salir()
  (format t " Constinuar o salir (1: Menu, 0: Salir) > ")
  (read)
)

(defun limpiar-pantalla()
  (format t "~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~T  ")
)

(defun pintar ()
  (limpiar-pantalla)
  (cond 
    ((eq q 'menu) (mostrar-menu))
    ((or (eq q 'turno-op) (eq q 'turno-pc)) (imprimir-tablero ta))
    ((eq q 'ganador-op) (imprimir-tablero ta) (format t "  >> GANASTE << ~%~%"))
    ((eq q 'ganador-pc) (imprimir-tablero ta) (format t "  << PERDISTE >> ~%~%"))
    ((eq q 'empate) (imprimir-tablero ta) (format t "   -- EMPATE -- ~%~%"))
  )
)

(defun actualizar ()  
  (cond 
    ((eq q 'menu) 
      (opcion)
      (cond
        ((or (= op 1) (= op 2) (= op 3)) (setq q 'turno-op))
        ((= op 0) (setq q 'fin))
      )
    )
    ((or (eq q 'turno-op) (eq q 'turno-pc))
      (let ( (gana (ganador-p ta)) (lleno (tablero-total-p ta)) )
        (if gana
          (setq q (if (= (car gana) (* 3 *oponente*)) 'ganador-op 'ganador-pc))
          (if lleno
            (setq q 'empate)            
            (cond          
              ((eq q 'turno-op)
                (setq ta (movimiento-op))
                (setq q 'turno-pc)
              )
              ((eq q 'turno-pc)
                (setq ta (movimiento-pc))
                (setq q 'turno-op)
              )
            )
          )
        )
      )
    )
    ((or (eq q 'ganador-op) (eq q 'ganador-pc) (eq q 'empate))      
      (if (zerop (continuar-salir))
        (setq q 'fin)
        (init)
      )
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
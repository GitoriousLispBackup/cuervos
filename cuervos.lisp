;;;;;;;;;;;;;;;;
;;; Interfaz ;;;
;;;;;;;;;;;;;;;;

(defvar *estado-inicial* '(0 0 0 0 0 0 0 0 0 0 0))
(defvar *estado-actual* *estado-inicial*)
(defvar *nodo-j-inicial*)
(defvar *ultimo-movimiento*)
;max es la maquina
;min es el humano
(defvar *jugador-inicial* 'max)
(defvar *jugador-actual* 'max)

(defvar *max-humano* nil)
(defvar *min-humano* t)

(defvar *contador-turnos* 1) ;impar, cuervos; par, buitre

(defvar *cuervos-jugados* 0)
(defvar *cuervos-comidos* 0)
(defvar *funcion-ia*)

;; Devuelve el jugador contrario al dado
(defun contrario (jugador)
  (if (eq jugador 'max) 'min 'max))

(defun menu ()
  (let ((salir nil) (opcion 0))
    (loop until salir do
	 (format t "~%*** El buitre y los cuervos ***~%~%")
	 (format t "Elige una opción:~%")
	 (format t " 1) Humano contra Humano~%")
	 (format t " 2) Humano contra Máquina~%")
	 (format t " 3) Máquina contra Máquina~%")
	 (format t " 4) Salir~%~%Opción:")
	 (setf opcion (read))
	 (cond
	   ((= opcion 1) nil)
	   ((= opcion 2)
	    (let ((salir2 nil) (opcion2 0))
	      (loop until salir2 do
		   (format t "~%¿Con quién jugará la máquina?~%")
		   (format t " 1) El buitre~%")
		   (format t " 2) Los cuervos~%")
		   (format t " 3) Atrás~%~%Opción:")
		   (setf opcion2 (read))
		   (cond
		     ((= opcion2 1)
		      (format t "La máquina jugará con el buitre~%")
		      (setf *jugador-inicial* 'min))
		     ((= opcion2 2)
		      (format t "La máquina jugará con los cuervos~%"))
		     ((= opcion2 3)
		      (setf salir2 t))))))
	   ((= opcion 3) nil)
	   ((= opcion 4)
	    (setf salir t))))
    (format t "~%Adiós.~%")))

(defun quien-okupa (n estado)
  (let ((a (nth n estado)))
    (cond ((equal a 0) n)
	  (t a))))

(defun imprimir-tablero (&optional (estado *estado-actual*) (canal t))
  (format canal "          ~a~%~%" (quien-okupa 0 estado))
  (format canal " ~a     ~a     ~a     ~a~%~%"
	  (quien-okupa 1 estado) (quien-okupa 2 estado)
	  (quien-okupa 3 estado) (quien-okupa 4 estado))
  (format canal "     ~a         ~a~%" (quien-okupa 5 estado) (quien-okupa 6 estado))
  (format canal "          ~a~%~%" (quien-okupa 7 estado))
  (format canal "  ~a                ~a~%" (quien-okupa 8 estado) (quien-okupa 9 estado))
  (format canal "Jugador siguiente: ~a~%" *jugador-actual*))

;;;;;;;;;;;;;;;;;;;;
;;; Estados, etc :::
;;;;;;;;;;;;;;;;;;;;

;el tablero se representa como un array de tamaño 10, el numero de casillas que hay con C o B o nil, más una posicion más con el numero de cuervos comidos
;         0
;
; 1    2     3    4
;
;     5       6
;         7
;   8            9

;para saber movimientos legales, se define una matriz 10x10 de t/nil
;   1 2 3 4 5 6 7 8 9 10
; 1     t t
; 2     t     t
; 3 t t   t   t
; 4 t   t   t   t
; 5       t     t
; 6   t t         t t
; 7       t t     t   t
; 8           t t   t t
; 9           t   t
;10             t t
(defvar *matriz-movimientos*
  (make-array '(10 10)
	      :initial-contents '((nil nil t t nil nil nil nil nil nil)
				  (nil nil t nil nil t nil nil nil nil)
				  (t t nil t nil t nil nil nil nil)
				  (t nil t nil t nil t nil nil nil)
				  (nil nil nil t nil nil t nil nil nil)
				  (nil t t nil nil nil nil t t nil)
				  (nil nil nil t t nil nil t nil t)
				  (nil nil nil nil nil t t nil t t)
				  (nil nil nil nil nil t nil t nil nil)
				  (nil nil nil nil nil nil t t nil nil))))
;a donde nos podemos mover en forma de lista, indice en 0
(defvar *lista-movimientos*)
(setf *lista-movimientos* '((2 3) (2 5) (0 2 3 5) (0 2 6 4) (3 6) (1 2 7 8) (3 4 7 9) (5 6 8 9) (5 7) (6 7)))
;para saber saltos legales, se define una matriz 10x10 de nil y el numero de la casilla que pasa por encima
;   1 2 3 4 5 6 7 8 9 10
; 1           t t
; 2       t       t
; 3         t       t
; 4   t               t
; 5     t         t
; 6 t                 t
; 7 t               t
; 8   t     t
; 9     t       t
;10       t   t
(defvar *matriz-saltos*
  (make-array '(10 10)
	      :initial-contents '((nil nil nil nil nil 2 3 nil nil nil)
				  (nil nil nil 2 nil nil nil 5 nil nil)
				  (nil nil nil nil 3 nil nil nil 5 nil)
				  (nil 2 nil nil nil nil nil nil nil 6)
				  (nil nil 3 nil nil nil nil 6 nil nil)
				  (2 nil nil nil nil nil nil nil nil 7)
				  (3 nil nil nil nil nil nil nil 7 nil)
				  (nil 5 nil nil 6 nil nil nil nil nil)
				  (nil nil 5 nil nil nil 7 nil nil nil)
				  (nil nil nil 6 nil 7 nil nil nil nil))))

(defvar *lista-saltos* '((5 6) (3 7) (4 8) (1 9) (2 7) (0 9) (0 8) (1 4) (2 6) (3 5)))


;; Estructura que representa un nodo del arbol de busqueda
(defstruct (nodo-j (:constructor crea-nodo-j)
                   (:conc-name nodo-)
                   (:print-function escribe-nodo-j))
  estado 		;; Tablero modificado
  jugador
  valor) 		;; Valor heuristico de la nueva jugada

;; Funcion que muestra por pantalla (u otro canal) el nodo dado
(defun escribe-nodo-j (nodo-j &optional (canal t))
  (format canal "~%Estado :~%")
  (imprimir-tablero (nodo-estado nodo-j) canal)
  (format canal "~%ultimo movimiento : ~a" *ultimo-movimiento*))
;; 	(format canal "~%Jugador : ~a" (jugador nodo-j)))

;; Funcion que inicializa *nodo-j-inicial*
(defun crea-nodo-j-inicial (jugador)
  (setf *estado-inicial* (make-array '(6 7)))
  (setf *nodo-j-inicial*
        (crea-nodo-j :estado *estado-inicial*
                     :jugador jugador)))

(defun comidos (estado) (first (last estado)))

(defun juegan-cuervos ()
  (oddp *contador-turnos*))

(defun juega-buitre ()
  (evenp *contador-turnos*))

(defun faltan-cuervos ()
  (< *cuervos-jugados* 14))

(defun buscar-buitre (estado)
  (let ((resultado -1))
    (loop for i from 0 to 9
       do
	 (cond
	   ((eq (nth i estado) 'B)
	    (setf resultado i))
	   (t nil)))
    resultado))

(defun se-puede-mover (estado i)
  (loop for x in (nth i *lista-movimientos*)
     when (equal (nth x estado) 0)
       collect x))
 (defun puede-saltar (estado i)
   (loop for x in (nth i *lista-saltos*)
	when (equal (nth x estado) 0)
	collect x))
(defun es-estado-final (estado)
  (let ((resultado nil))
    ;quedan 3 cuervos
    (cond
      ((>= (comidos estado) 4) (setf resultado t))
      (t
       ;el buitre no se puede mover
       ;donde esta el buitre
       (let ((i (buscar-buitre estado)))
	 (cond ((> (length (se-puede-mover estado i)) 0)
		(setf resultado t))
	       ((> (length (puede-saltar estado i)) 0)
		(setf resultado t))
	       (t nil)))))
    resultado))

(defun es-estado-ganador (estado turno jugador)
  (let ((resultado nil))
    (if (and (es-estado-final estado) (equal turno jugador))
	(setf resultado t))
  resultado))

;*movimientos*
(defvar *movimientos* '(origen destino))
;aplica-movimiento(movimiento,estado)
(defun aplica-movimiento (movimiento estado)
  (let ((estado-temporal estado))
    (cond ((and
	    (juega-buitre)
	    (or (member (nth 1 movimiento) (se-puede-mover estado-temporal (buscar-buitre estado)))
		(member (nth 1 movimiento) (puede-saltar estado (buscar-buitre estado)))))
	   (setf (nth (buscar-buitre estado) estado-temporal) 0)
	   (setf (nth (nth 1 movimiento) estado-temporal) 'B))
	  ((and
	    (juegan-cuervos)
	    (or
	     (member (nth 1 movimiento) (se-puede-mover estado-temporal (nth 0 movimiento))))
	    (= (nth 0 movimiento) -2))
	   (cond ((not(= (nth 0 movimiento) -2))
		  (setf (nth (nth 0 movimiento) estado-temporal) 0)))
	   (setf (nth (nth 1 movimiento) estado-temporal) 'C))
	  (t (setf estado-temporal nil)))
    estado-temporal))

;;;;;;;;;;;;;
;;; Juego ;;;
;;;;;;;;;;;;;

(defun jugar-humano ()
  (let ((movimiento nil))
    (cond ((and (juegan-cuervos) (faltan-cuervos))
	   ;decir que tiene que poner un cuervo en juego
	   (format t "Tienes que poner un cuervo en juego~%")
	   (format t "Elige una posicion del tablero donde ponerlo:~%")
	   (setf movimiento (list -2 (read))))
	  ((juegan-cuervos)
	   ;ofrecer mover un cuervo
	   (format t "Tienes que mover un cuervo. ¿Origen?~%")
	   (setf movimiento (list (read) -1))
	   (format t "¿Destino?~%")
	   (setf movimiento (list (first movimiento) (read))))
	  ((juega-buitre)
	   ;ofrecer mover o saltar el buitre
	   (format t "¿Dónde mueves el buitre?~%")
	   (setf movimiento (list -1 (read)))))
    movimiento))

;TODO
(defun jugar-maquina ()
)

(defun jugar ()
  ;imprimir el estado actual
  (imprimir-tablero)
  (let ((movimiento nil) (nuevo-estado nil))
    ;hacer la eleccion del movimiento
    ;  -si el jugador actual es máquina -> minimax
    ;  -si el jugador es humano -> menu de opciones
    (cond ((or (and (equal *jugador-actual* 'max) *max-humano*)
	       (and (equal *jugador-actual* 'min) *min-humano*))
	   (loop until nuevo-estado do
		(setf movimiento (jugar-humano))
		(setf nuevo-estado (aplica-movimiento movimiento *estado-actual*))))
	  (t
	   (setf movimiento (jugar-maquina))
	   (setf nuevo-estado (aplica-movimiento movimiento *estado-actual*))))
    ;ejecutar el movimiento, lo hacemos arriba ya
    ;(setf nuevo-estado (aplica-movimiento movimiento *estado-actual*))
    ;actualizar jugador actual, estado actual y contador de turnos
    (setf *contador-turnos* (1+ *contador-turnos*))
    (setf *jugador-actual* (contrario *jugador-actual*))
    (setf *estado-actual* nuevo-estado)))

(defun imprimir-fin-juego ()
  (format t "*** ¡El juego ha terminado! ***")
  (imprimir-tablero)
  (setf *contador-turnos* (1- *contador-turnos*))
  (if (juegan-cuervos)
      (format t "¡Los cuervos ganan!")
      (format t "¡El buitre gana!")))

(defun juego ()
  (let ((fin-juego nil))
    (loop until fin-juego do
	 (jugar)
	 (if (es-estado-final *estado-actual*)
	     (setf fin-juego t)))
    (imprimir-fin-juego)))

;(load "minimax.lisp")

(defun cuervos ()
  (compile-file "cuervos.lisp")
  (load "cuervos")
  (menu))

(defun inicio ()
  (format t "Escribe (cuervos) para empezar.~%"))

(inicio)

;;;;;;;;;;;;;;;;
;;; Interfaz ;;;
;;;;;;;;;;;;;;;;

(defvar *estado-inicial* '(0 0 0 0 0 0 0 0 0 0))
(defvar *estado-actual* *estado-inicial*)
;max es la maquina
;min es el humano
(defvar *jugador-inicial* 'max)
(defvar *jugador-actual* 'max)
(defvar *max-humano* nil)
(defvar *min-humano* t)
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
		      (format t "La máquina jugará con el buitre~%"))
		     ((= opcion2 2)
		      (format t "La máquina jugará con los cuervos~%"))
		     ((= opcion2 3)
		      (setf salir2 t))))))
	   ((= opcion 3) nil)
	   ((= opcion 4)
	    (setf salir t))))
    (format t "~%Adiós.~%")))


(defun imprimir-tablero ()
  (format t "          ~a~%~%" (nth 0 *estado-actual*))
  (format t " ~a     ~a     ~a     ~a~%~%"
	  (nth 1 *estado-actual*) (nth 2 *estado-actual*)
	  (nth 3 *estado-actual*) (nth 4 *estado-actual*))
  (format t "     ~a         ~a~%" (nth 5 *estado-actual*) (nth 6 *estado-actual*))
  (format t "          ~a~%~%" (nth 7 *estado-actual*))
  (format t "  ~a                ~a~%" (nth 8 *estado-actual*) (nth 9 *estado-actual*))
  (format t "Jugador siguiente: ~a~%" *jugador-actual*))
	  
;el tablero se representa como un array de tamaño 10, el numero de casillas que hay con C o B o nil
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
  (make-array '(10 10)	      :initial-contents '((nil nil t t nil nil nil nil nil nil)
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
;         0
;
; 1    2     3    4
;
;     5       6
;         7
;   8            9
(defvar *lista-saltos* '((5 6) (3 7) (4 8) (1 9) (2 7) (0 9) (0 8) (1 4) (2 6) (3 5)))

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
      ((>= *cuervos-comidos* 4) (setf resultado t))
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

(defun jugar ()
  ;imprimir el estado actual
  (imprimir-tablero)
  ;TODO hacer la eleccion del movimiento
  ;  -si el jugador actual es máquina -> minimax
  ;  -si el jugaador es humnao -> menu de opciones
  ;TODO ejecutar el movimiento
  ;TODO actualizar jugador actual y estado actual
  ;TODO devolver el nuevo estado
)

(defun juego ()
  (let ((fin-juego nil)
	(un-estado nil))
    (loop until fin-juego
	 (setf un-estado (jugar))
	 (if (es-estado-final estado)
	     (setf fin-juego t)))
    (imprimir-fin-juego un-estado)))
	

;es-estado-ganador(estado,turno,jugador)
;*movimientos*
;aplica-movimiento(movimiento,estado)
;f-utilidad(estado,turno)


;(load "minimax.lisp")

(defun cuervos ()
  (compile-file "cuervos.lisp")
  (load "cuervos")
  (menu))

(defun inicio ()
  (format t "Escribe (cuervos) para empezar.~%"))

(inicio)
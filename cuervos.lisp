;;;;;;;;;;;;;;;;
;;; Interfaz ;;;
;;;;;;;;;;;;;;;;

(defun menu ()
  (let ((salir nil) (opcion 0))
    (loop until salir do
	 (format t "El buitre y los cuervos~%~%")
	 (format t "Elige una opción:~%")
	 (format t " 1) Humano contra Humano~%")
	 (format t " 2) Humano contra Máquina~%")
	 (format t " 3) Máquina contra Máquina~%")
	 (format t " 4) Opciones~%")
	 (format t " 5) Salir~%~%Opción:")
	 (setf opcion (read))
	 (cond
	   ((= opcion 1) nil)
	   ((= opcion 2) nil)
	   ((= opcion 3) nil)
	   ((= opcion 4)
	    (let ((saliro nil) (opciono 0))
	      (loop until saliro do
		   (format t "Opciones:~%")
		   (format t " 1) Usar primera función estática~%")
		   (format t " 2) Usar segunda función estática~%")
		   (if (equal *jugador-inicial* 'max)
		       (format t " 3) Quien empieza: Máquina~%")
		       (format t " 3) Quien empieza: Humano~%"))
		   (format t " 4) Atrás~%~%Opción:")
		   (setf opciono (read))
		   (cond
		     ((= opciono 1) nil)
		     ((= opciono 2) nil)
		     ((= opciono 3)
		      (setf *jugador-inicial* (contrario *jugador-inicial*)))
		     ((= opciono 4)
		      (setf saliro t))))))
	   ((= opcion 5)
	    (setf salir t))))
    (format t "Adiós.~%")))
	   


;el tablero se representa como un array de tamaño 10, el numero de casillas que hay
;         1
;
; 2    3     4    5
;
;     6       7
;         8
;   9           10

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
(defvar *matriz-movimientos*)
(setf *matriz-movimientos*
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
;para saber saltos legales, se define una matriz 10x10 de t/nil
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
(defvar *matriz-saltos*)
(setf *matriz-movimientos*
      (make-array '(10 10)
		  :initial-contents '((nil nil nil nil nil 3 4 nil nil nil)
				      (nil nil nil t nil nil nil t nil nil)
				      (nil nil nil nil t nil nil nil t nil)
				      (nil t nil nil nil nil nil nil nil t)
				      (nil nil t nil nil nil nil t nil nil)
				      (t nil nil nil nil nil nil nil nil t)
				      (t nil nil nil nil nil nil nil t nil)
				      (nil t nil nil t nil nil nil nil nil)
				      (nil nil t nil nil nil t nil nil nil)
				      (nil nil nil t nil t nil nil nil nil))))

(defvar *estado-inicial*)
;max es la maquina
;min es el humano
(defvar *jugador-inicial* 'max)
;es-estado-final(estado)
;es-estado-ganador(estado,turno,jugador)
;*movimientos*
;aplica-movimiento(movimiento,estado)
;f-utilidad(estado,turno)


(load "minimax.lisp")

(defun cuervos ()
  (compile-file "cuervos.lisp")
  (load "cuervos")
  (menu))

(defun inicio ()
  (format t "Escribe (cuervos) para empezar.~%"))

(inicio)
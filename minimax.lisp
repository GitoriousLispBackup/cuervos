;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALGORITMO MINIMAX ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Algoritmo MINIMAX
;(defun minimax (nodo-j profundidad)
;  (if (or (es-estado-final (estado nodo-j))
;          (= profundidad 0))
;      (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
;                                        (jugador nodo-j)))
;      (let ((sucesores (sucesores nodo-j)))
;        (if (null sucesores)
;            (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
;                                              (jugador nodo-j)))
;            (if (eq (jugador nodo-j) 'max)
;                (maximizador sucesores profundidad)
;                (minimizador sucesores profundidad))))))

;; Funcion que busca maximizar (MAX) la puntuacion
;(defun maximizador (sucesores profundidad)
;  (let ((mejor-sucesor (first sucesores))
;        (mejor-valor *minimo-valor*))
;    (loop for sucesor in sucesores do
;	 (setf valor (valor (minimax sucesor (1- profundidad))))
;	 (when (> valor mejor-valor)
;	   (setf mejor-valor valor)
;	   (setf mejor-sucesor sucesor)))
;    (setf (valor mejor-sucesor) mejor-valor)
;    mejor-sucesor))

;; Funcion que busca minimizar (MIN) la puntuacion
;(defun minimizador (sucesores profundidad)
;  (let ((mejor-sucesor (first sucesores))
;        (mejor-valor *maximo-valor*))
;    (loop for sucesor in sucesores do
;	 (setf valor (valor (minimax sucesor (1- profundidad))))
;	 (when (< valor mejor-valor)
;	   (setf mejor-valor valor)
;	   (setf mejor-sucesor sucesor)))
;    (setf (valor mejor-sucesor) mejor-valor)
;    mejor-sucesor))

;; Algoritmo MINIMAX con poda ALFA-BETA
(defun minimax-a-b (nodo profundidad
		    &optional (alfa *minimo-valor*)
		    (beta *maximo-valor*))
  (dbg-der)
  (let ((nodo-elegido nil))
    (if (or
	 (es-estado-final (nodo-estado nodo))
	 (= profundidad 0))
	(progn
;	  (dbg "Final o profundidad limite~%")
	  (setf nodo-elegido (crea-nodo :valor (f-e-estatica (nodo-estado nodo) (nodo-jugador nodo))
					:jugador (nodo-jugador nodo))))
	(let ((lsucesores (sucesores nodo)))
	  (if (endp lsucesores)
	      (progn
		(format t "Sucesores vacío~%")
		(setf nodo-elegido (crea-nodo :valor (f-e-estatica (nodo-estado nodo) (nodo-jugador nodo))
					      :jugador (nodo-jugador nodo))))
	      (if (eq (nodo-jugador nodo) 'max)
		  (setf nodo-elegido
			(maximizador-a-b
			 (sort lsucesores #'> :key (lambda (x) (f-e-estatica (nodo-estado x) 'min)))
			 profundidad alfa beta))
		  (setf nodo-elegido
			(minimizador-a-b
			 (sort lsucesores #'< :key (lambda (x) (f-e-estatica (nodo-estado x) 'max)))
			 profundidad alfa beta))))))
    (dbg "Nodo ~a valor ~a~%" (nodo-jugador nodo-elegido) (nodo-valor nodo-elegido))
    (dbg-izq)
    nodo-elegido))

(defun elegir-aleatoriamente (lista)
;  (dbg "~a nodos candidatos~%" (length lista))
  (if (= 1 (length lista))
      (first lista)
      (nth (random (length lista)) lista)))

;; Funcion que busca maximizar (MAX) la puntuacion con ALFA-BETA
(defun maximizador-a-b (sucesores profundidad alfa beta)
;  (dbg "maximizar~%")
  (let ((mejores-sucesores (list (first sucesores)))
        (valor 0)
	(mejor nil))
    (loop for sucesor in sucesores do
	 (setf valor
	       (nodo-valor (minimax-a-b sucesor (1- profundidad) alfa beta)))
	 (when (= valor alfa)
	   ;mismo valor mejor, añadir a la lista
	   (setf mejores-sucesores (cons sucesor mejores-sucesores)))
	 (when (> valor alfa)
	   ;mejor valor, nueva lista con este nodo
	   (setf alfa valor)
	   (setf mejores-sucesores (list sucesor)))
	 (when (>= alfa beta) ;podar
	   (return))
)
    (setf mejor (elegir-aleatoriamente mejores-sucesores))
    (setf (nodo-valor mejor) alfa)
    mejor))

;; Funcion que busca minimizar (MIN) la puntuacion con ALFA-BETA
(defun minimizador-a-b (sucesores profundidad alfa beta)
;  (dbg "minimizar~%")
  (let ((mejores-sucesores (list (first sucesores)))
        (valor 0)
	(mejor nil))
    (loop for sucesor in sucesores do
	 (setf valor
	       (nodo-valor (minimax-a-b sucesor (1- profundidad) alfa beta)))
	 (when (= valor beta)
	   (setf mejores-sucesores (cons sucesor mejores-sucesores)))
	 (when (< valor beta)
	   (setf beta valor)
	   (setf mejores-sucesores (list sucesor)))
	 (when (>= alfa beta)
	   (return))
	 )
    (setf mejor (elegir-aleatoriamente mejores-sucesores))
    (setf (nodo-valor mejor) beta)
    mejor))

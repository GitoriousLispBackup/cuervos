;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALGORITMO MINIMAX ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Valores maximos y minimos para las variables alfa y beta
(defvar *minimo-valor* -10080)
(defvar *maximo-valor* 10080)
(defvar *medio-valor* 720) ;;porque son valores facilmente divisible 2*3*4*5*6

;; Para un posible nodo del arbol devuelve sus hijos
(defun sucesores (nodo-j)
  (let ((resultado ()))
    (loop for movimiento in *movimientos* do
	 (let ((siguiente
		(aplica-movimiento movimiento
				   (estado nodo-j) (if (equal (jugador nodo-j) 'max)
						       *color-maquina*
						       *color-humano*))))
	   (when siguiente
	     (push
	      (crea-nodo-j
	       :estado siguiente
	       :jugador (contrario (jugador nodo-j)))
	      resultado))))
    (nreverse resultado)))

;; Devuelve el jugador contrario al dado
(defun contrario (jugador)
  (if (eq jugador 'max) 'min 'max))

;; Algoritmo MINIMAX
(defun minimax (nodo-j profundidad)
  (if (or (es-estado-final (estado nodo-j))
          (= profundidad 0))
      (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
                                        (jugador nodo-j)))
      (let ((sucesores (sucesores nodo-j)))
        (if (null sucesores)
            (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
                                              (jugador nodo-j)))
            (if (eq (jugador nodo-j) 'max)
                (maximizador sucesores profundidad)
                (minimizador sucesores profundidad))))))

;; Funcion que busca maximizar (MAX) la puntuacion
(defun maximizador (sucesores profundidad)
  (let ((mejor-sucesor (first sucesores))
        (mejor-valor *minimo-valor*))
    (loop for sucesor in sucesores do
	 (setf valor (valor (minimax sucesor (1- profundidad))))
	 (when (> valor mejor-valor)
	   (setf mejor-valor valor)
	   (setf mejor-sucesor sucesor)))
    (setf (valor mejor-sucesor) mejor-valor)
    mejor-sucesor))

;; Funcion que busca minimizar (MIN) la puntuacion
(defun minimizador (sucesores profundidad)
  (let ((mejor-sucesor (first sucesores))
        (mejor-valor *maximo-valor*))
    (loop for sucesor in sucesores do
	 (setf valor (valor (minimax sucesor (1- profundidad))))
	 (when (< valor mejor-valor)
	   (setf mejor-valor valor)
	   (setf mejor-sucesor sucesor)))
    (setf (valor mejor-sucesor) mejor-valor)
    mejor-sucesor))

;; Algoritmo MINIMAX con poda ALFA-BETA
(defun minimax-a-b (nodo-j profundidad
		    &optional (alfa *minimo-valor*)
		    (beta *maximo-valor*))
  (if (or (es-estado-final (estado nodo-j)) (= profundidad 0))
      (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
                                        (jugador nodo-j)))
      (let ((sucesores (sucesores nodo-j)))
        (if (null sucesores)
            (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
                                              (jugador nodo-j)))
            (if (eq (jugador nodo-j) 'max)
                (maximizador-a-b
                 (sort sucesores #'> :key (lambda (nodo) (f-e-estatica (estado nodo) 'min)))
                 profundidad alfa beta)
                (minimizador-a-b
                 (sort sucesores #'< :key (lambda (nodo) (f-e-estatica (estado nodo) 'max)))
                 profundidad alfa beta))))))

;; Funcion que busca maximizar (MAX) la puntuacion con ALFA-BETA
(defun maximizador-a-b (sucesores profundidad alfa beta)
  (let ((mejor-sucesor (first sucesores))
        (valor 0))
    (loop for sucesor in sucesores do
	 (setf valor
	       (valor (minimax-a-b sucesor (1- profundidad) alfa beta)))
	 (when (> valor alfa)
	   (setf alfa valor)
	   (setf mejor-sucesor sucesor))
	 (when (>= alfa beta)
	   (return)))
    (setf (valor mejor-sucesor) alfa)
    mejor-sucesor))

;; Funcion que busca minimizar (MIN) la puntuacion con ALFA-BETA
(defun minimizador-a-b (sucesores profundidad alfa beta)
  (let ((mejor-sucesor (first sucesores))
        (valor 0))
    (loop for sucesor in sucesores do
	 (setf valor
	       (valor (minimax-a-b sucesor (1- profundidad) alfa beta)))
	 (when (< valor beta)
	   (setf beta valor)
	   (setf mejor-sucesor sucesor))
	 (when (>= alfa beta)
	   (return)))
    (setf (valor mejor-sucesor) beta)
    mejor-sucesor))

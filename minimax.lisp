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
(defun minimax-a-b (nodo-j profundidad
		    &optional (alfa *minimo-valor*)
		    (beta *maximo-valor*))
  (if (or (es-estado-final (nodo-estado nodo-j)) (= profundidad 0))
      (crea-nodo-j :valor (f-e-estatica (nodo-estado nodo-j)
                                        (nodo-jugador nodo-j)))
      (let ((sucesores (sucesores nodo-j)))
        (if (null sucesores)
            (crea-nodo-j :valor (f-e-estatica (nodo-estado nodo-j)
                                              (nodo-jugador nodo-j)))
            (if (eq (nodo-jugador nodo-j) 'max)
                (maximizador-a-b
                 (sort sucesores #'> :key (lambda (nodo) (f-e-estatica (nodo-estado nodo) 'min)))
                 profundidad alfa beta)
                (minimizador-a-b
                 (sort sucesores #'< :key (lambda (nodo) (f-e-estatica (nodo-estado nodo) 'max)))
                 profundidad alfa beta))))))

;; Funcion que busca maximizar (MAX) la puntuacion con ALFA-BETA
(defun maximizador-a-b (sucesores profundidad alfa beta)
  (let ((mejor-sucesor (first sucesores))
        (valor 0))
    (loop for sucesor in sucesores do
	 (setf valor
	       (nodo-valor (minimax-a-b sucesor (1- profundidad) alfa beta)))
	 (when (> valor alfa)
	   (setf alfa valor)
	   (setf mejor-sucesor sucesor))
	 (when (>= alfa beta)
	   (return)))
    (setf (nodo-valor mejor-sucesor) alfa)
    mejor-sucesor))

;; Funcion que busca minimizar (MIN) la puntuacion con ALFA-BETA
(defun minimizador-a-b (sucesores profundidad alfa beta)
  (let ((mejor-sucesor (first sucesores))
        (valor 0))
    (loop for sucesor in sucesores do
	 (setf valor
	       (nodo-valor (minimax-a-b sucesor (1- profundidad) alfa beta)))
	 (when (< valor beta)
	   (setf beta valor)
	   (setf mejor-sucesor sucesor))
	 (when (>= alfa beta)
	   (return)))
    (setf (nodo-valor mejor-sucesor) beta)
    mejor-sucesor))

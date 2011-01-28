;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables globales ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *nodo-actual*)
;max es la maquina
;min es el humano
(defvar *jugador-inicial* 'max)

(defvar *max-humano* nil)
(defvar *min-humano* t)

;el tablero se representa como un array de tamaño 10, el numero de casillas que hay con C o B o nil
; más una posicion más con el numero de cuervos comidos
; más el contador de turnos, es decir, al final es una lista de longitud 12
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
(setf *lista-movimientos* '((2 3) (2 5) (0 1 3 5) (0 2 4 6) (3 6) (1 2 7 8) (3 4 7 9) (5 6 8 9) (5 7) (6 7)))
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
(defstruct (nodo (:constructor crea-nodo)
		 (:conc-name nodo-)
		 (:print-function escribe-nodo))
  estado ;tablero
  jugador ;jugador actual
  ;contador-turnos ;este es el turno X, si es impar, juegan cuervos; si es par, el buitre
  valor) ;valor de la funcion estatica

;; Funcion que muestra por pantalla (u otro canal) el nodo dado
(defun escribe-nodo (nodo &optional (canal t) profundidad)
  (format canal "Estado del nodo: ~a~%" (nodo-estado nodo))
  (format canal "Jugador del nodo: ~a~%" (nodo-jugador nodo))
  (format canal "Turnos del nodo: ~a~%" (nodo-contador-turnos nodo))
  (imprimir-tablero nodo canal))

;; Funcion que inicializa *nodo-inicial*
(defun crea-nodo-inicial ()
  (setf *nodo-actual*
        (crea-nodo :estado '(0 0 0 0 0 0 0 0 0 0 0 1)
		   :jugador *jugador-inicial*)))

(defun nodo-contador-turnos (&optional (nodo *nodo-actual*))
  (first (last (nodo-estado nodo))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funciones auxiliares ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Devuelve el jugador contrario al dado
(defun contrario (jugador)
  (if (eq jugador 'max) 'min 'max))

(defun comidos (estado)
  (nth 10 estado))

(defun jugador-actual ()
  (nodo-jugador *nodo-actual*))

(defun juegan-cuervos (&optional (nodo *nodo-actual*) (estado nil))
  (oddp (if estado
	    (nth 11 estado)
	    (nodo-contador-turnos nodo))))

(defun juega-buitre (&optional (nodo *nodo-actual*) (estado nil))
  (evenp (if estado
	     (nth 11 estado)
	     (nodo-contador-turnos nodo))))

(defun faltan-cuervos (&optional (nodo *nodo-actual*) (estado nil))
  (< (if estado
	 (nth 11 estado)
	 (nodo-contador-turnos nodo)) 14))

(defun buscar-buitre (estado)
  (let ((resultado -1))
    (loop for i from 0 to 9
       do
	 (cond
	   ((eq (nth i estado) 'B)
	    (setf resultado i))
	   (t nil)))
    resultado))

(defun busca-cuervos (estado)
  (loop for x from 0 to 9
     when (equal (nth x estado) 'C)
     collect x))

(defun se-puede-mover (estado i)
  (when (< i 0)
    (return-from se-puede-mover))
  (loop for x in (nth i *lista-movimientos*)
     when (equal (nth x estado) 0)
     collect x))

(defun puede-saltar (estado i)
  (when (< i 0)
    (return-from puede-saltar))
  (loop for x in (nth i *lista-saltos*)
     when (and
	   (equal (nth x estado) 0) ;destino vacio
	   (equal (nth (aref *matriz-saltos* i x) estado) 'C)) ;hay un cuervo en el salto
     collect x))

(defun salto-sencillo (estado i j)
  (let ((nuevo (loop for a in estado collect a)))
    (setf (nth i nuevo) 0) ;origen a 0
    (setf (nth j nuevo) 'B) ;destino B
    (setf (nth (aref *matriz-saltos* i j) nuevo) 0) ;sobre el que saltamos a 0
    (setf (nth 10 nuevo) (1+ (nth 10 nuevo))) ;aumentar contador de saltos
    nuevo))

(defun saltos (&optional (estado (nodo-estado *nodo-actual*)))
  ;las defino dentro porque no tienen sentido fuera de esta funcion
  (defun saltos-r (estado i)
    (let* ((saltos-desde-aqui (puede-saltar estado i))
	   (resultado (list)))
      (cond ((endp saltos-desde-aqui) (list))
	    (t (loop for destino in saltos-desde-aqui do
		    (let* ((nuevo-estado (salto-sencillo estado i destino))
			   (dests (saltos-r nuevo-estado destino)))
		      (setf resultado (append resultado (list (append (list destino) dests))))))))
      resultado))
  (defun saltos-r2 (lista)
    (cond ((eq (length lista) 1) (cons (first lista) '()))
	  (t (cons (first lista) (saltos-r2 (second lista))))))

  (mapcar #'saltos-r2 (saltos-r estado (buscar-buitre estado))))

(defun mejor-salto (&optional (estado (nodo-estado *nodo-actual*)))
  (let ((long 0)
	(camino nil))
    (loop for un-camino in (saltos estado)
	 when (> (length un-camino) long)
	 do (setf camino un-camino)
	 (setf long (length un-camino)))
    (list long camino)))

(defun salto-multiple (estado i j)
  (let* ((saltos-posibles (saltos estado))
	 (este-salto (find-if #'(lambda(x) (member j x)) saltos-posibles)))
    (if este-salto
	(let ((fin nil))
	  (loop
	     for dest in este-salto
	     when (not fin)
	     do
	       (if (= i j)
		 (setf fin t)
		 (setf estado (salto-sencillo estado i dest)))
	       (setf i dest))
	  estado)
	nil)))

(salto-multiple '(0 B C 0 0 0 C C 0 0 0 6) 1 9)

(defun puede-saltar-multiple (estado i)
  (let ((resultado (list)))
    (loop for salto in (saltos estado)
       do (setf resultado (append resultado salto)))
    resultado))

(puede-saltar-multiple '(0 0 C 0 0 C C B 0 0 0 6) 1)
(saltos '(0 0 C 0 0 C C B 0 0 0 6))
(mejor-salto '(0 0 C 0 0 C C B 0 0 0 6))

(defun quien-okupa (n estado)
  (let ((a (nth n estado)))
    (cond ((equal a 0) n)
	  (t a))))

(defun cuervos-to-string (comidos &optional (cuervos nil))
  (let ((limit comidos)
	(lista (list)))
    (when cuervos
      (setf limit (- 7 comidos)))
    (setf lista (loop for a from 1 to limit collect 'C))
    (format nil "~{~a~}" lista)))
      
(defun imprimir-tablero (&optional (nodo *nodo-actual*) (canal t))
  (let ((estado (nodo-estado nodo)))
    (format canal "~%~%Turno ~a~%" (nodo-contador-turnos nodo))
    (format canal "~%          ~a~%~%" (quien-okupa 0 estado))
    (format canal " ~a     ~a     ~a     ~a~%~%" (quien-okupa 1 estado) (quien-okupa 2 estado) (quien-okupa 3 estado) (quien-okupa 4 estado))
    (format canal "     ~a         ~a~%" (quien-okupa 5 estado) (quien-okupa 6 estado))
    (format canal "          ~a~%~%" (quien-okupa 7 estado))
    (format canal "  ~a                ~a~%~%" (quien-okupa 8 estado) (quien-okupa 9 estado))
    (format canal "Cuervos: ~10a Buitre:~a~%" (cuervos-to-string (comidos estado) t) (cuervos-to-string (comidos estado)))
    (if (juega-buitre nodo)
	(format canal "Juega el buitre~%")
	(format canal "Juegan los cuervos~%"))))

(defun es-subseq (seq seq2)
  (cond ((endp seq) t)
	((endp seq2) nil)
	(t (and (equal (first seq) (first seq2)) (es-subseq (rest seq) (rest seq2))))))

;;;;;;;;;;;;;;;;;;;;
;;; Estados, etc :::
;;;;;;;;;;;;;;;;;;;;

(defun es-estado-final (estado)
  (let ((resultado nil))
    (cond
      ;quedan 3 cuervos
      ((>= (comidos estado) 4)
       (setf resultado t))
      (t
       ;el buitre no se puede mover
       ;donde esta el buitre
       (let ((i (buscar-buitre estado)))
	 (cond ((= i -1) nil)
	       ((and
		 (= (length (se-puede-mover estado i)) 0)
		 (= (length (puede-saltar estado i)) 0))
		(setf resultado t))
	       (t nil)))))
    resultado))

(defun es-estado-ganador (estado turno jugador)
  (let ((resultado nil))
    (if (and (es-estado-final estado) (equal turno jugador))
	(setf resultado t))
  resultado))

(defun aplica-movimiento (movimiento estado)
  (let ((estado-temporal (copy-seq estado)))
    (cond ((or
	    (and ;si el movimiento es una lista
	     (listp (second movimiento))
	     (not (equal (nth (first (second movimiento)) estado) 0)))
	    (and ;si es un movimiento simple
	     (not (listp (second movimiento)))
	     (not (equal (nth (second movimiento) estado) 0))))
	   ;si el destino esta ocupado no vale
	   (setf estado-temporal nil))
	  ((= (first movimiento) -3) ;poner el buitre
	   (setf (nth (second movimiento) estado-temporal) 'B))
	  ((= (first movimiento) -2) ;mover el buitre
	   (let* ((buitre (buscar-buitre estado))
		  (movimientos (se-puede-mover estado buitre))
		  (saltos-sencillos (puede-saltar estado buitre))
		  (destino (first (second movimiento)))
		  (cuervo nil)
		  (ok nil))
	     (cond ((equal (length (second movimiento)) 1)
		    (cond ((member destino movimientos) ;mover el buitre
			   (setf ok t))
			  ((member destino saltos-sencillos)
			   (setf estado-temporal (salto-sencillo estado-temporal buitre destino)))
			  (t
			   (setf estado-temporal nil))))
		   ((and
		     (> (length (second movimiento)) 1)
		     (find-if #'(lambda(x) (es-subseq (second movimiento) x)) (saltos estado-temporal)))
		    (setf estado-temporal (salto-multiple estado-temporal buitre (first (last (second movimiento))))))
		   (t  (setf estado-temporal nil)))
	     (when ok
	       (setf (nth destino estado-temporal) 'B)
	       (setf (nth buitre estado-temporal) 0))))
	  ((= (first movimiento) -1) ;poner cuervo
	   (setf (nth (second movimiento) estado-temporal) 'C))
	  (t ;mover cuervo
	   (setf (nth (second movimiento) estado-temporal) 'C)
	   (setf (nth (first movimiento) estado-temporal) 0)))
    estado-temporal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funciones estaticas ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;ideas para funcion de evaluacion estatica:
;	1.
;		-para el buitre, devolvera el numero de casillas a las que puede mover o saltar desde su posicion actual (buitre defensivo)
;		-para los cuervos, devolvera el negado del numero de casillas a las que se puede mover el buitre (cuervos agresivos)
;	2.
;		-para el buitre, devolvera el numero de cuervos que puede comer desde su posicion actual (buitre agresivo)
;		-para los cuervos, devolvera el numero de cuervos que estan a salvo del buitre (cuervos defensivos)
;	3.
;		-para el buitre, devolvera el numero de cuervos que puede comer desde su posicion actual (buitre agresivo)
;		-para los cuervos, devolvera el negado del numero de casillas a las que se puede mover el buitre (cuervos agresivos)
;	4.
;		-para el buitre, devolvera el numero de casillas a las que puede mover o saltar desde su posicion actual (buitre defensivo)
;		-para los cuervos, devolvera el numero de cuervos que estan a salvo del buitre (cuervos defensivos)


;; Valores maximos y minimos para las variables alfa y beta
(defvar *minimo-valor* -1000)
(defvar *maximo-valor* 1000)
(defvar *medio-valor* 0)

(defun f-e-estatica (estado turno))
(defun f-e-estatica-max (estado turno))
(defun f-e-estatica-min (estado turno))

; da un valor aleatorio al nodo
(defun funcion-estatica-aleatoria (estado turno)
  (- (random 2001) 1000))
  
(defun funcion-estatica1 (estado turno)
  (let ((resultado 0))
    (cond ((es-estado-ganador estado turno 'MAX) (return-from funcion-estatica1 *maximo-valor*))
	  ((es-estado-ganador estado turno 'MIN) (return-from funcion-estatica1 *minimo-valor*))
	  ((equal turno 'MAX) ;el resultado tiene que ser el mayor posible para el jugador actual
	   (cond ((juega-buitre :estado estado)
		  (setf resultado (+ resultado
				     (length (se-puede-mover estado (buscar-buitre estado)))
				     (* 2 (length (puede-saltar estado (buscar-buitre estado))))))) ;damos prioridad a los saltos frente a los movimientos
		 (t (setf resultado (- resultado
				       (+ (length (se-puede-mover estado (buscar-buitre estado)))
					  (* 2 (length (puede-saltar estado (buscar-buitre estado))))))))))
	  
	  (t ; el turno es MIN, asi que se tiene que devolver el valor menor para el jugador actual
	   (cond ((juegan-cuervos :estado estado)
		  (setf resultado (+ resultado
				     (length (se-puede-mover estado (buscar-buitre estado)))
				     (* 2 (length (puede-saltar estado (buscar-buitre estado)))))))
		 (t (setf resultado (- resultado
				       (+ (length (se-puede-mover estado (buscar-buitre estado)))
					  (* 2 (length (puede-saltar estado (buscar-buitre estado)))))))))))
    resultado))

(defun funcion-estatica2 (estado turno)
  (let ((resultado 0))
    (cond ((es-estado-ganador estado turno 'MAX) (return-from funcion-estatica2 *maximo-valor*))
	  ((es-estado-ganador estado turno 'MIN) (return-from funcion-estatica2 *minimo-valor*))
	  ((equal turno 'MAX)
	   (cond ((juega-buitre :estado estado)
		  (setf resultado (+ resultado (length (puede-saltar estado (buscar-buitre estado))))))
		 (t (+ resultado (loop for x in (busca-cuervos estado) summing (length (se-puede-mover estado x)))))))
	  (t
	   (cond ((juegan-cuervos :estado estado)
		  (setf resultado (+ resultado (length (puede-saltar estado (buscar-buitre estado))))))
		 (t (setf resultado (+ resultado (loop for x in (busca-cuervos estado) summing (length (se-puede-mover estado x)))))))))
    resultado))

(defun funcion-estatica3 (estado turno)
  (let ((resultado 0))
    (cond ((es-estado-ganador estado turno 'MAX) (return-from funcion-estatica3 *maximo-valor*))
	  ((es-estado-ganador estado turno 'MIN) (return-from funcion-estatica3 *minimo-valor*))
	  ((equal turno 'MAX)
	   (cond ((juega-buitre :estado estado)
		  (setf resultado (+ resultado (length (puede-saltar estado (buscar-buitre estado))))))
		 (t (setf resultado (- resultado (+ (length (se-puede-mover estado (buscar-buitre estado)))
						    (length (puede-saltar estado (buscar-buitre estado)))))))))
	  (t
	   (cond ((juegan-cuervos :estado estado)
		  (setf resultado (+ resultado (length (puede-saltar estado (buscar-buitre estado))))))
		 (t
		  (setf resultado (- resultado (+ (length (se-puede-mover estado (buscar-buitre estado)))
						  (length (puede-saltar estado (buscar-buitre estado))))))))))
    resultado))
	
(defun funcion-estatica4 (estado turno)
  (let ((resultado 0))
    (cond ((es-estado-ganador estado turno 'MAX) (return-from funcion-estatica4 *maximo-valor*))
	  ((es-estado-ganador estado turno 'MIN) (return-from funcion-estatica4 *minimo-valor*))
	  ((equal turno 'MAX)
	   (cond ((juega-buitre :estado estado)
		  (setf resultado (+ resultado
				     (length (se-puede-mover estado (buscar-buitre estado)))
				     (* 2 (length (puede-saltar estado (buscar-buitre estado)))))))
		 (t (setf resultado (+ resultado (loop for x in (busca-cuervos estado) summing (length (se-puede-mover estado x))))))))
	  (t (cond ((juegan-cuervos :estado estado)
		    (setf resultado (+ resultado
				       (length (se-puede-mover estado (buscar-buitre estado)))
				       (* 2 (length (puede-saltar estado (buscar-buitre estado)))))))
		   (t (setf resultado (+ resultado (loop for x in (busca-cuervos estado) summing (length (se-puede-mover estado x)))))))))
    resultado))

;funcion estatica para buitre agresivo
(defun funcion-estatica-b-agr (estado turno)
  (let ((resultado 0))
    (cond ((es-estado-ganador estado turno 'MAX) (return-from funcion-estatica-b-agr *maximo-valor*))
	  ((es-estado-ganador estado turno 'MIN) (return-from funcion-estatica-b-agr *minimo-valor*))
	  ((equal turno 'MAX) (setf resultado (+ resultado (length (puede-saltar estado (buscar-buitre estado))))))
	  (t (setf resultado (+ resultado (loop for x in (busca-cuervos estado) summing (length (se-puede-mover estado x)))))))
    resultado))

;funcion estatica para buitre defensivo
(defun funcion-estatica-b-def (estado turno)
  (let ((resultado 0))
    (cond ((es-estado-ganador estado turno 'MAX) (return-from funcion-estatica-b-def *maximo-valor*))
	  ((es-estado-ganador estado turno 'MIN) (return-from funcion-estatica-b-def *minimo-valor*))
	  ((equal turno 'MAX) (setf resultado (+ resultado
						 (length (se-puede-mover estado (buscar-buitre estado)))
						 (* 2 (length (puede-saltar estado (buscar-buitre estado)))))))
	  (t (setf resultado (+ resultado (loop for x in (busca-cuervos estado) summing (length (se-puede-mover estado x))))))) ;aqui tengo algunas dudas... revisar luego.
    resultado))

;funcion estatica para cuervo agresivo
(defun funcion-estatica-c-agr (estado turno)
	(let ((resultado 0))
		(cond ((es-estado-ganador estado turno 'MAX) (return-from funcion-estatica-c-agr *maximo-valor*))
			((es-estado-ganador estado turno 'MIN) (return-from funcion-estatica-c-agr *minimo-valor*)) 
			((equal turno 'MAX) (setf resultado (- resultado
				       (+ (length (se-puede-mover estado (buscar-buitre estado)))
					  (* 2 (length (puede-saltar estado (buscar-buitre estado))))))))
			(t (setf resultado (+ resultado
				     (length (se-puede-mover estado (buscar-buitre estado)))
				     (* 2 (length (puede-saltar estado (buscar-buitre estado)))))))) ; una vez mas, not sure. Hay que repasar esto :S
					 
	resultado))

			
;funcion estatica para cuervo defensivo
(defun funcion-estatica-c-agr (estado turno)
	(let ((resultado 0))
		(cond ((es-estado-ganador estado turno 'MAX) (return-from funcion-estatica-c-def *maximo-valor*))
			((es-estado-ganador estado turno 'MIN) (return-from funcion-estatica-c-def *minimo-valor*)) 
			((equal turno 'MAX) (setf resultado (+ resultado (loop for x in (busca-cuervos estado) summing (length (se-puede-mover estado x))))))
			(t (setf resultado (+ resultado (length (puede-saltar estado (buscar-buitre estado))))))) ; una vez mas, not sure. Hay que repasar esto :S
					 
	resultado))


	
(setf (symbol-function 'f-e-estatica) #'funcion-estatica-aleatoria)
(setf (symbol-function 'f-e-estatica-max) #'funcion-estatica-aleatoria)
(setf (symbol-function 'f-e-estatica-min) #'funcion-estatica-aleatoria)

;;;;;;;;;;;;;;;
;;; Minimax ;;;
;;;;;;;;;;;;;;;

;; Para un posible nodo del arbol devuelve sus hijos
;(defun sucesores (nodo-j)
;  (let ((resultado ()))
;    (loop for movimiento in *movimientos* do
;	 (let ((siguiente
;		(aplica-movimiento movimiento
;				   (estado nodo-j) (if (equal (jugador nodo-j) 'max)
;						       *color-maquina*
;						       *color-humano*))))
;	   (when siguiente
;	     (push
;	      (crea-nodo-j
;	       :estado siguiente
;	       :jugador (contrario (jugador nodo-j)))
;	      resultado))))
;    (nreverse resultado)))

;debe devolver una lista de nodos con los posibles movimientos que se puede hacer
(defun sucesores (nodo)
  (let ((estado (nodo-estado nodo))
	(resultado ())
	(movimientos ()))
    (cond ((juega-buitre nodo)
	   ;si es buitre
	   ;  si es el primer movimiento, buscar un sitio donde poner el buitre
	   ;  si no, se puede mover o incluso saltar
	   (if (equal (nodo-contador-turnos nodo) 2)
	       (loop for i from 0 to 9 do (push (list -3 i) movimientos))
	       (loop for i from 0 to 9 do (push (list -2 (list i)) movimientos))))
	  ((juegan-cuervos nodo)
	   ;si es cuervos
	   (cond ((faltan-cuervos nodo)
		  ;  si aún quedan cuervos por poner, buscar un sitio donde ponerlos
		  (loop for i from 0 to 9
		     do (push (list -1 i) movimientos)))
		 (t
		  ;  si no, buscar movimientos posibles de cada cuervo que haya en el tablero
		  ;buscar los cuervos del tablero
		  (let ((cuervos (busca-cuervos (nodo-estado nodo))))
		    (loop for c in cuervos do
			 (setf movimientos
			       (append movimientos (mapcar #'(lambda(x) (list c x)) (nth c *lista-movimientos*))))))))))
    (reverse movimientos)
    ;mirar qué movimientos son posibles con aplica-movimiento
    (loop for movimiento in movimientos do
	 (let ((siguiente
		(aplica-movimiento movimiento estado)))
	   (when siguiente
;	     (format t "Posible siguiente: ~a Mov:~a~%" siguiente movimiento)
	     (setf (nth 11 siguiente) (1+ (nodo-contador-turnos nodo)))
	     (push
	      (crea-nodo
	       :estado siguiente
	       :jugador (contrario (nodo-jugador nodo)))
	      resultado))))
;    (format t "Movimientos posibles: ~a~%" (length resultado))
    (reverse resultado)))

(load "minimax.lisp")

;;;;;;;;;;;;;
;;; Juego ;;;
;;;;;;;;;;;;;

(defun jugar-humano ()
  (let ((movimiento nil))
    (cond ((juegan-cuervos)
	   (cond ((faltan-cuervos)
		  ;decir que tiene que poner un cuervo en juego
		  (format t "Tienes que poner un cuervo en juego.~%")
		  (format t "Elige una posicion del tablero donde ponerlo:~%")
		  (setf movimiento (list -1 (read))))
		 (t
		  ;ofrecer mover un cuervo
		  (format t "Tienes que mover un cuervo. ¿Origen?~%")
		  (setf movimiento (list (read) -1))
		  (format t "¿Destino?~%")
		  (setf movimiento (list (first movimiento) (read))))))
	  ((juega-buitre)
	   (format t "Saltos posibles: ~a~%" (saltos))
	   (format t "Mejor salto: long ~a, camino ~a~%" (first (mejor-salto)) (second (mejor-salto)))
	   (cond ((= (nodo-contador-turnos) 2)
		  ;poner el buitre
		  (format t "Tienes que poner el buitre. ¿Dónde?~%")
		  (setf movimiento (list -3 (read))))
		 (t
		  ;mover o saltar el buitre
		  (format t "¿Dónde mueves el buitre?~%")
		  (setf movimiento (list -2 (eval (read-from-string (concatenate 'string "'(" (read-line) ")")))))))))
    movimiento))

;TODO
(defun jugar-maquina ()
  ;TODO crear un nodo con el estado actual y tal
  ;TODO llamar a minimax con el nodo creado
  ;TODO devolver el nuevo estado que devuelve minimax
  (if (equal (nodo-jugador *nodo-actual*) 'max)
      (setf (symbol-function 'f-e-estatica) #'f-e-estatica-max)
      (setf (symbol-function 'f-e-estatica) #'f-e-estatica-min))
  (let* ((profundidad (+ 3 (comidos (nodo-estado *nodo-actual*)))))
    (minimax-a-b *nodo-actual* profundidad)))

(defun jugar ()
  ;imprimir el estado actual
  (imprimir-tablero)
  (let ((movimiento nil) (nuevo-estado nil) (nuevo-nodo nil))
    ;hacer la eleccion del movimiento
    ;  -si el jugador actual es máquina -> minimax
    ;  -si el jugador es humano -> menu de opciones
    (cond ((or (and (equal (jugador-actual) 'max) *max-humano*)
	       (and (equal (jugador-actual) 'min) *min-humano*))
	   (loop until nuevo-estado do
		(setf movimiento (jugar-humano))
		(setf nuevo-estado (aplica-movimiento movimiento (nodo-estado *nodo-actual*)))
		(when nuevo-estado
		  (setf (nth 11 nuevo-estado) (1+ (nodo-contador-turnos)))
		  (setf nuevo-nodo
			(crea-nodo
			 :estado nuevo-estado
			 :jugador (contrario (nodo-jugador *nodo-actual*)))))))
	  (t
	   (setf nuevo-nodo (jugar-maquina))))
    (setf *nodo-actual* nuevo-nodo)))
    ;ejecutar el movimiento, lo hacemos arriba ya
    ;(setf nuevo-estado (aplica-movimiento movimiento *estado-actual*))
    ;actualizar jugador actual, estado actual y contador de turnos
;    (setf *ultimo-movimiento* movimiento)
;    (setf *contador-turnos* (1+ *contador-turnos*))
;    (setf *jugador-actual* (contrario *jugador-actual*))
;    (setf *estado-actual* nuevo-estado)))

(defun imprimir-fin-juego ()
  (format t "*** ¡El juego ha terminado! ***~%")
  (imprimir-tablero)
  (if (oddp (1- (nodo-contador-turnos)))
      (format t "¡Los cuervos ganan!~%~%")
      (format t "¡El buitre gana!~%~%")))

(defun juego ()
  (crea-nodo-inicial)
  (let ((fin-juego nil))
    (loop until fin-juego do
	 (jugar)
	 (if (es-estado-final (nodo-estado *nodo-actual*))
	     (setf fin-juego t)))
    (imprimir-fin-juego)))


;;;;;;;;;;;;;;;;
;;; Interfaz ;;;
;;;;;;;;;;;;;;;;

(defun menu ()
  (let ((salir nil) (opcion 0))
    (loop until salir do
	 (format t "~%*** El buitre y los cuervos ***~%~%")
	 (format t "Elige una opción:~%")
	 (format t " 1) Humano contra Humano~%")
	 (format t " 2) Humano contra Máquina~%")
	 (format t " 3) Máquina contra Máquina~%")
	 (format t " 4) Salir~%")
	 (setf opcion (read))
	 (cond
	   ((= opcion 1)
	    (setf *max-humano* t)
	    (setf *min-humano* t)
	    (juego))
	   ((= opcion 2)
	    (setf *max-humano* nil)
	    (setf *min-humano* t)
	    (let ((salir2 nil) (opcion2 0))
	      (loop until salir2 do
		   (format t "~%¿Con quién jugará la máquina?~%")
		   (format t " 1) El buitre~%")
		   (format t " 2) Los cuervos~%")
		   (format t " 3) Atrás~%")
		   (setf opcion2 (read))
		   (cond
		     ((= opcion2 1)
		      (format t "La máquina jugará con el buitre~%")
		      (setf *jugador-inicial* 'min)
		      (juego))
		     ((= opcion2 2)
		      (format t "La máquina jugará con los cuervos~%")
		      (setf *jugador-inicial* 'max)
		      (juego))
		     ((= opcion2 3)
		      (setf salir2 t))))))
	   ((= opcion 3)
	    (setf *max-humano* nil)
	    (setf *min-humano* nil)
	    (let ((salir2 nil) (opcion2 0) (ia1 0) (ia2 0))
	      (defun ia-a-texto (ia)
		(cond ((= ia 0) "aleatoria")
		      ((= ia 1) "agresiva")
		      ((= ia 2) "defensiva")))
	      (defun cambiar-ia1 ()
		(setf ia1 (mod (1+ ia1) 3))
		(cond ((= ia1 0)
		       (setf (symbol-function 'f-e-estatica-max) #'funcion-estatica-aleatoria))
		      ((= ia1 1)
		       (setf (symbol-function 'f-e-estatica-max) #'funcion-estatica1))
		      ((= ia1 2)
		       (setf (symbol-function 'f-e-estatica-max) #'funcion-estatica2))))
	      (defun cambiar-ia2 ()
		(setf ia2 (mod (1+ ia2) 3))
		(cond ((= ia2 0)
		       (setf (symbol-function 'f-e-estatica-min) #'funcion-estatica-aleatoria))
		      ((= ia2 1)
		       (setf (symbol-function 'f-e-estatica-min) #'funcion-estatica3))
		      ((= ia2 2)
		       (setf (symbol-function 'f-e-estatica-min) #'funcion-estatica4))))
	      (loop until salir2 do
		   (format t "** Partida Máquina contra Máquina **~%")
		   (format t " 1) IA Máquina de los cuervos (aleatoria, agresiva, defensiva): ~a~%" (ia-a-texto ia1))
		   (format t " 2) IA Máquina del buitre (aleatoria, agresiva, defensiva): ~a~%" (ia-a-texto ia2))
		   (format t " 3) ¡Jugar!~%~%")
		   (format t " 4) Atrás~%")
		   (setf opcion2 (read))
		   (cond
		     ((= opcion2 1) (cambiar-ia1))
		     ((= opcion2 2) (cambiar-ia2))
		     ((= opcion2 3) (juego))
		     ((= opcion2 4)
		      (setf salir2 t))))))
	   ((= opcion 4)
	    (setf salir t))))
    (format t "~%Adiós.~%")))

(defun cuervos ()
;  (compile-file "cuervos.lisp")
;  (load "cuervos")
  (menu))

(defun inicio ()
  (format t "Escribe (cuervos) para empezar.~%"))

(inicio)
(cuervos)
;(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))

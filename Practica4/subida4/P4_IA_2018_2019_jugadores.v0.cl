(use-package 'conecta4)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

;; -------------------------------------------------------------------------------
;; Funciones de evaluación
;; -------------------------------------------------------------------------------

(defun f-eval-bueno (estado)
  ; current player standpoint
  (let* ((tablero (estado-tablero estado))
	 (ficha-actual (estado-turno estado))
	 (ficha-oponente (siguiente-jugador ficha-actual)))
    (if (juego-terminado-p estado)
	(let ((ganador (ganador estado)))
	  (cond ((not ganador) 0)
		((eql ganador ficha-actual) +val-max+)
		(t +val-min+)))
      (let ((puntuacion-actual 0)
	    (puntuacion-oponente 0))
	(loop for columna from 0 below (tablero-ancho tablero) do
	      (let* ((altura (altura-columna tablero columna))
		     (fila (1- altura))
		     (abajo (contar-abajo tablero ficha-actual columna fila))
		     (der (contar-derecha tablero ficha-actual columna fila))
		     (izq (contar-izquierda tablero ficha-actual columna fila))
		     (abajo-der (contar-abajo-derecha tablero ficha-actual columna fila))
		     (arriba-izq (contar-arriba-izquierda tablero ficha-actual columna fila))
		     (abajo-izq (contar-abajo-izquierda tablero ficha-actual columna fila))
		     (arriba-der (contar-arriba-derecha tablero ficha-actual columna fila)))
		(setf puntuacion-actual
		      (+ puntuacion-actual
			 (cond ((= abajo 0) 0)
			       ((= abajo 1) 10)
			       ((= abajo 2) 100)
			       ((= abajo 3) 1000))
			 (cond ((= der 0) 0)
			       ((= der 1) 10)
			       ((= der 2) 100)
			       ((= der 3) 1000))
			 (cond ((= izq 0) 0)
			       ((= izq 1) 10)
			       ((= izq 2) 100)
			       ((= izq 3) 1000))
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 10)
			       ((= abajo-izq 2) 100)
			       ((= abajo-izq 3) 1000)))))
	      (let* ((altura (altura-columna tablero columna))
		     (fila (1- altura))
		     (abajo (contar-abajo tablero ficha-oponente columna fila))
		     (der (contar-derecha tablero ficha-oponente columna fila))
		     (izq (contar-izquierda tablero ficha-oponente columna fila))
		     (abajo-der (contar-abajo-derecha tablero ficha-oponente columna fila))
		     (arriba-izq (contar-arriba-izquierda tablero ficha-oponente columna fila))
		     (abajo-izq (contar-abajo-izquierda tablero ficha-oponente columna fila))
		     (arriba-der (contar-arriba-derecha tablero ficha-oponente columna fila)))
		(setf puntuacion-oponente
		      (+ puntuacion-oponente
				 (cond ((= abajo 0) 0)
					   ((= abajo 1) 10)
					   ((= abajo 2) 100)
					   ((= abajo 3) 1000))
				 (cond ((= der 0) 0)
					   ((= der 1) 10)
					   ((= der 2) 100)
					   ((= der 3) 1000))
				 (cond ((= izq 0) 0)
					   ((= izq 1) 10)
					   ((= izq 2) 100)
					   ((= izq 3) 1000))
				 (cond ((= abajo-izq 0) 0)
					   ((= abajo-izq 1) 10)
					   ((= abajo-izq 2) 100)
					   ((= abajo-izq 3) 1000))))))
	(- puntuacion-actual puntuacion-oponente)))))

;; ----------HEURISTICA---------------------------
(defun heuristica (estado)
  ; current player standpoint
  (let* ((tablero (estado-tablero estado))
	 (ficha-actual (estado-turno estado))
	 (ficha-oponente (siguiente-jugador ficha-actual)))
    (if (juego-terminado-p estado)
	(let ((ganador (ganador estado)))
	  (cond ((not ganador) 0)
		((eql ganador ficha-actual) +val-max+)
		(t +val-min+)))
      (let ((puntuacion-actual 0)
	    (puntuacion-oponente 0))
	(loop for columna from 0 below (tablero-ancho tablero) do
		(loop for fila from 0 below (tablero-alto tablero) do
	      (let* ((altura (altura-columna tablero columna))
		     ;(fila (1- altura))
		     (abajo (contar-abajo tablero ficha-actual columna fila))
		     (der (contar-derecha tablero ficha-actual columna fila))
		     (izq (contar-izquierda tablero ficha-actual columna fila))
		     (abajo-der (contar-abajo-derecha tablero ficha-actual columna fila))
		     (arriba-izq (contar-arriba-izquierda tablero ficha-actual columna fila))
		     (abajo-izq (contar-abajo-izquierda tablero ficha-actual columna fila))
		     (arriba-der (contar-arriba-derecha tablero ficha-actual columna fila)))
		(setf puntuacion-actual
		      (+ puntuacion-actual
			 ;Miramos si estan libres las columnas del medio de la primera fila
			 (if (and (= fila 0) (= columna 3) (null (obtener-ficha tablero columna fila))) 5000
				(if (and (= fila 0) (or (= columna 4)(= columna 2)) (null (obtener-ficha tablero columna fila))) 3000
			 
			 ; ;Miramos que no haya otra ficha encima
			 (if (< fila 5)
				(if (null (obtener-ficha tablero columna (+ 1 fila)))
				 (cond ((= abajo 0) 0)
					   ((= abajo 1) 10)
					   ((= abajo 2) 100)
					   ((= abajo 3) 1000)
					   (t 0))
					   0)
			  0)))
			  
			  ;Miramos si hay una ficha del adversario a la izquierda
			  (if (> columna 1) 
					(if (null (obtener-ficha tablero (- columna 2) fila))
						(cond ((= (+ der izq) 0) 0)
						 ((= (+ der izq) 1) 100)
					     ((= (+ der izq) 2) 100)
					     ((= (+ der izq) 3) 1000)
						 (t 0))
					0)
			  0)
			  
			  ;Miramos si hay una ficha del adversario a la derecha
			  (if (< columna 5) 
					(if (null (obtener-ficha tablero (+ columna 2) fila))
						(cond ((= (+ der izq) 0) 0)
						 ((= (+ der izq) 1) 100)
					     ((= (+ der izq) 2) 100)
					     ((= (+ der izq) 3) 1000)
						 (t 0))
					0)
			  0)
			  
			  
			  ;DIAGONAL ABAJO_DER-ARRIBA_IZQ
				;Miramos que no haya una ficha del adversario que impida la linea por la izquierda
				(if (and (> columna 1) (< fila 4))
					(if (null (obtener-ficha tablero (- columna 2) (+ fila 2)))
						(cond ((= (+ abajo-der arriba-izq) 0) 0)
						   ((= (+ abajo-der arriba-izq) 1) 100)
						   ((= (+ abajo-der arriba-izq) 2) 100)
						   ((= (+ abajo-der arriba-izq) 3) 1000)
						   (t 0))
					0)
				0)
				
			 ;Miramos que no haya una ficha del adversario que impida la linea por la derecha
				(if (and (< columna 4) (> fila 1))
					(if (null (obtener-ficha tablero (+ columna 2) (- fila 2))) 
						(cond ((= (+ abajo-der arriba-izq) 0) 0)
						   ((= (+ abajo-der arriba-izq) 1) 100)
						   ((= (+ abajo-der arriba-izq) 2) 100)
						   ((= (+ abajo-der arriba-izq) 3) 
								(if (= fila 1) 5000 
								1000))
							(t 0))
					0)
				0)
				
			 ;DIAGONAL ABAJO_IZQ-ARRIBA_DER
				;Miramos que no haya una ficha del adversario que impida la linea por la derecha
				(if (and (< columna 4) (< fila 4)) 
					(if (null (obtener-ficha tablero (+ columna 2) (+ 2 fila)))
						(cond ((= (+ abajo-izq arriba-der) 0) 0)
						((= (+ abajo-izq arriba-der) 1) 100)
						((= (+ abajo-izq arriba-der) 2) 100)
						((= (+ abajo-izq arriba-der) 3) 1000)
						(t 0))
					0)
				0)
				
				;Miramos que no haya una ficha del adversario que impida la linea por la izquierda
				(if (and (> columna 1) (> fila 1))
					(if (null (obtener-ficha tablero (- columna 2) (- fila 2)))
						(cond ((= (+ abajo-der arriba-izq) 0) 0)
						   ((= (+ abajo-izq arriba-der) 1) 100)
						   ((= (+ abajo-izq arriba-der) 2) 100)
						   ((= (+ abajo-izq arriba-der) 3) 
								(if (= fila 1) 5000 ;Solo tendria que colocar ahi la ficha para ganar
								1000))
							(t 0))
					0)
				0)
				)))
	      (let* ((altura (altura-columna tablero columna))
		     ;(fila (1- altura))
		     (abajo (contar-abajo tablero ficha-actual columna fila))
		     (der (contar-derecha tablero ficha-actual columna fila))
		     (izq (contar-izquierda tablero ficha-actual columna fila))
		     (abajo-der (contar-abajo-derecha tablero ficha-actual columna fila))
		     (arriba-izq (contar-arriba-izquierda tablero ficha-actual columna fila))
		     (abajo-izq (contar-abajo-izquierda tablero ficha-actual columna fila))
		     (arriba-der (contar-arriba-derecha tablero ficha-actual columna fila)))
		(setf puntuacion-actual
		      (+ puntuacion-actual
			  ;Miramos si estan libres las columnas del medio de la primera fila
			 (if (and (= fila 0) (= columna 3) (null (obtener-ficha tablero columna fila))) 5000
				(if (and (= fila 0) (or (= columna 4)(= columna 2)) (null (obtener-ficha tablero columna fila))) 3000
			 
			 ; ;Miramos que no haya otra ficha encima
			 (if (< fila 5)
				(if (null (obtener-ficha tablero columna (+ 1 fila)))
				 (cond ((= abajo 0) 0)
					   ((= abajo 1) 10)
					   ((= abajo 2) 100)
					   ((= abajo 3) 1000)
					   (t 0))
					   0)
			  0)))
			  
			  ;Miramos si hay una ficha del adversario a la izquierda
			  (if (> columna 1) 
					(if (null (obtener-ficha tablero (- columna 2) fila))
						(cond ((= (+ der izq) 0) 0)
						 ((= (+ der izq) 1) 100)
					     ((= (+ der izq) 2) 100)
					     ((= (+ der izq) 3) 1000)
						 (t 0))
					0)
			  0)
			  
			  ;Miramos si hay una ficha del adversario a la derecha
			  (if (< columna 5) 
					(if (null (obtener-ficha tablero (+ columna 2) fila))
						(cond ((= (+ der izq) 0) 0)
						 ((= (+ der izq) 1) 100)
					     ((= (+ der izq) 2) 100)
					     ((= (+ der izq) 3) 1000)
						 (t 0))
					0)
			  0)
			  
			  
			  ;DIAGONAL ABAJO_DER-ARRIBA_IZQ
				;Miramos que no haya una ficha del adversario que impida la linea por la izquierda
				(if (and (> columna 1) (< fila 4))
					(if (null (obtener-ficha tablero (- columna 2) (+ fila 2)))
						(cond ((= (+ abajo-der arriba-izq) 0) 0)
						   ((= (+ abajo-der arriba-izq) 1) 100)
						   ((= (+ abajo-der arriba-izq) 2) 100)
						   ((= (+ abajo-der arriba-izq) 3) 1000)
						   (t 0))
					0)
				0)
				
			 ;Miramos que no haya una ficha del adversario que impida la linea por la derecha
				(if (and (< columna 4) (> fila 1))
					(if (null (obtener-ficha tablero (+ columna 2) (- fila 2))) 
						(cond ((= (+ abajo-der arriba-izq) 0) 0)
						   ((= (+ abajo-der arriba-izq) 1) 100)
						   ((= (+ abajo-der arriba-izq) 2) 100)
						   ((= (+ abajo-der arriba-izq) 3) 
								(if (= fila 1) 5000 
								1000))
							(t 0))
					0)
				0)
				
			 ;DIAGONAL ABAJO_IZQ-ARRIBA_DER
				;Miramos que no haya una ficha del adversario que impida la linea por la derecha
				(if (and (< columna 4) (< fila 4)) 
					(if (null (obtener-ficha tablero (+ columna 2) (+ 2 fila)))
						(cond ((= (+ abajo-izq arriba-der) 0) 0)
						((= (+ abajo-izq arriba-der) 1) 100)
						((= (+ abajo-izq arriba-der) 2) 100)
						((= (+ abajo-izq arriba-der) 3) 1000)
						(t 0))
					0)
				0)
				
				;Miramos que no haya una ficha del adversario que impida la linea por la izquierda
				(if (and (> columna 1) (> fila 1))
					(if (null (obtener-ficha tablero (- columna 2) (- fila 2)))
						(cond ((= (+ abajo-der arriba-izq) 0) 0)
						   ((= (+ abajo-izq arriba-der) 1) 100)
						   ((= (+ abajo-izq arriba-der) 2) 100)
						   ((= (+ abajo-izq arriba-der) 3) 
								(if (= fila 1) 5000 ;Solo tendria que colocar ahi la ficha para ganar
								1000))
							(t 0))
					0)
				0))))))
	(- puntuacion-actual puntuacion-oponente)))))


(defun tito (estado)
  ; current player standpoint
  (let* ((tablero (estado-tablero estado))
	 (ficha-actual (estado-turno estado))
	 (ficha-oponente (siguiente-jugador ficha-actual))) 
    (if (juego-terminado-p estado)
	(let ((ganador (ganador estado)))
	  (cond ((not ganador) 0)
		((eql ganador ficha-actual) +val-max+)
		(t +val-min+)))
      (let ((puntuacion-actual 0)
	    (puntuacion-oponente 0))
	(loop for columna from 0 below (tablero-ancho tablero) do
	      (let* ((altura (altura-columna tablero columna))
		     (fila (1- altura))
		     (abajo (contar-abajo tablero ficha-actual columna fila))
		     (der (contar-derecha tablero ficha-actual columna fila))
		     (izq (contar-izquierda tablero ficha-actual columna fila))
		     (abajo-der (contar-abajo-derecha tablero ficha-actual columna fila))
		     (arriba-izq (contar-arriba-izquierda tablero ficha-actual columna fila))
		     (abajo-izq (contar-abajo-izquierda tablero ficha-actual columna fila))
		     (arriba-der (contar-arriba-derecha tablero ficha-actual columna fila)))
		(setf puntuacion-actual
		      (+ puntuacion-actual
			 (cond ((= abajo 0) 0)
			       ((= abajo 1) 10)
			       ((= abajo 2) 1000)
			       ((= abajo 3) 4000))
			 (cond ((= der 0) 0)
			       ((= der 1) 10)
			       ((= der 2) 1000)
			       ((= der 3) 4000))
			 (cond ((= izq 0) 0)
			       ((= izq 1) 10)
			       ((= izq 2) 1000)
			       ((= izq 3) 4000))
			 (cond ((= abajo-der 0) 0)
			       ((= abajo-der 1) 10)
			       ((= abajo-der 2) 1000)
			       ((= abajo-der 3) 4000))
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 10)
			       ((= abajo-izq 2) 1000)
			       ((= abajo-izq 3) 4000))
			 (cond ((= arriba-der 0) 0)
			       ((= arriba-der 1) 10)
			       ((= arriba-der 2) 1000)
			       ((= arriba-der 3) 4000))
			 (cond ((= arriba-izq 0) 0)
			       ((= arriba-izq 1) 10)
			       ((= arriba-izq 2) 1000)
			       ((= arriba-izq 3) 4000)))))
	      (let* ((altura (altura-columna tablero columna))
		     (fila (1- altura))
		     (abajo (contar-abajo tablero ficha-oponente columna fila))
		     (der (contar-derecha tablero ficha-oponente columna fila))
		     (izq (contar-izquierda tablero ficha-oponente columna fila))
		     (abajo-der (contar-abajo-derecha tablero ficha-oponente columna fila))
		     (arriba-izq (contar-arriba-izquierda tablero ficha-oponente columna fila))
		     (abajo-izq (contar-abajo-izquierda tablero ficha-oponente columna fila))
		     (arriba-der (contar-arriba-derecha tablero ficha-oponente columna fila)))
		(setf puntuacion-oponente
		      (+ puntuacion-oponente
			 (cond ((= abajo 0) 0)
			       ((= abajo 1) 1)
			       ((= abajo 2) 2000)
			       ((= abajo 3) 3000))
			 (cond ((= der 0) 0)
			       ((= der 1) 1)
			       ((= der 2) 2000)
			       ((= der 3) 3000))
			 (cond ((= izq 0) 0)
			       ((= izq 1) 1)
			       ((= izq 2) 2000)
			       ((= izq 3) 3000))
			 (cond ((= abajo-der 0) 0)
			       ((= abajo-der 1) 1)
			       ((= abajo-der 2) 2000)
			       ((= abajo-der 3) 3000))
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 1)
			       ((= abajo-izq 2) 2000)
			       ((= abajo-izq 3) 3000))
			 (cond ((= arriba-der 0) 0)
			       ((= arriba-der 1) 1)
			       ((= arriba-der 2) 2000)
			       ((= arriba-der 3) 3000))
			 (cond ((= arriba-izq 0) 0)
			       ((= arriba-izq 1) 1)
			       ((= arriba-izq 2) 2000)
			       ((= arriba-izq 3) 3000))))))
		(- puntuacion-actual puntuacion-oponente)))))
	

(defun bro (estado)
  ; current player standpoint
  (let* ((tablero (estado-tablero estado))
	 (ficha-actual (estado-turno estado))
	 (ficha-oponente (siguiente-jugador ficha-actual)))
    (if (juego-terminado-p estado)
	(let ((ganador (ganador estado)))
	  (cond ((not ganador) 0)
		((eql ganador ficha-actual) +val-max+)
		(t +val-min+)))
      (let ((puntuacion-actual 0)
	    (puntuacion-oponente 0))
	(loop for columna from 0 below (tablero-ancho tablero) do
		(loop for fila from 0 below (tablero-alto tablero) do
	      (let* ((altura (altura-columna tablero columna))
		     ;(fila (1- altura))
		     (abajo (contar-abajo tablero ficha-actual columna fila))
		     (der (contar-derecha tablero ficha-actual columna fila))
		     (izq (contar-izquierda tablero ficha-actual columna fila))
		     (abajo-der (contar-abajo-derecha tablero ficha-actual columna fila))
		     (arriba-izq (contar-arriba-izquierda tablero ficha-actual columna fila))
		     (abajo-izq (contar-abajo-izquierda tablero ficha-actual columna fila))
		     (arriba-der (contar-arriba-derecha tablero ficha-actual columna fila)))
		(setf puntuacion-actual
		      (+ puntuacion-actual
			 ;Miramos si estan libres las columnas del medio de la primera fila
			 (if (and (= fila 0) (= columna 3) (null (obtener-ficha tablero columna fila))) 5000
				(if (and (= fila 0) (or (= columna 4)(= columna 2)) (null (obtener-ficha tablero columna fila))) 3000
			 
			 ; ;Miramos que no haya otra ficha encima
			 (if (< fila 5)
				(if (null (obtener-ficha tablero columna (+ 1 fila)))
				 (cond ((= abajo 0) 0)
					   ((= abajo 1) 10)
					   ((= abajo 2) 100)
					   ((= abajo 3) 1000)
					   (t 0))
					   0)
			  0)))
			  
			  ;Miramos si hay una ficha del adversario a la izquierda
			  (if (> columna 1) 
					(if (null (obtener-ficha tablero (- columna 2) fila))
						(cond ((= (+ der izq) 0) 0)
						 ((= (+ der izq) 1) 100)
					     ((= (+ der izq) 2) 100)
					     ((= (+ der izq) 3) 1000)
						 (t 0))
					0)
			  0)
			  
			  ;Miramos si hay una ficha del adversario a la derecha
			  (if (< columna 5) 
					(if (null (obtener-ficha tablero (+ columna 2) fila))
						(cond ((= (+ der izq) 0) 0)
						 ((= (+ der izq) 1) 100)
					     ((= (+ der izq) 2) 100)
					     ((= (+ der izq) 3) 1000)
						 (t 0))
					0)
			  0)
			  
			  
			  ;DIAGONAL ABAJO_DER-ARRIBA_IZQ
				;Miramos que no haya una ficha del adversario que impida la linea por la izquierda
				(if (and (> columna 1) (< fila 4))
					(if (null (obtener-ficha tablero (- columna 2) (+ fila 2)))
						(cond ((= (+ abajo-der arriba-izq) 0) 0)
						   ((= (+ abajo-der arriba-izq) 1) 100)
						   ((= (+ abajo-der arriba-izq) 2) 100)
						   ((= (+ abajo-der arriba-izq) 3) 1000)
						   (t 0))
					0)
				0)
				
			 ;Miramos que no haya una ficha del adversario que impida la linea por la derecha
				(if (and (< columna 4) (> fila 1))
					(if (null (obtener-ficha tablero (+ columna 2) (- fila 2))) 
						(cond ((= (+ abajo-der arriba-izq) 0) 0)
						   ((= (+ abajo-der arriba-izq) 1) 100)
						   ((= (+ abajo-der arriba-izq) 2) 100)
						   ((= (+ abajo-der arriba-izq) 3) 
								(if (= fila 1) 5000 
								1000))
							(t 0))
					0)
				0)
				
			 ;DIAGONAL ABAJO_IZQ-ARRIBA_DER
				;Miramos que no haya una ficha del adversario que impida la linea por la derecha
				(if (and (< columna 4) (< fila 4)) 
					(if (null (obtener-ficha tablero (+ columna 2) (+ 2 fila)))
						(cond ((= (+ abajo-izq arriba-der) 0) 0)
						((= (+ abajo-izq arriba-der) 1) 100)
						((= (+ abajo-izq arriba-der) 2) 100)
						((= (+ abajo-izq arriba-der) 3) 1000)
						(t 0))
					0)
				0)
				
				;Miramos que no haya una ficha del adversario que impida la linea por la izquierda
				(if (and (> columna 1) (> fila 1))
					(if (null (obtener-ficha tablero (- columna 2) (- fila 2)))
						(cond ((= (+ abajo-der arriba-izq) 0) 0)
						   ((= (+ abajo-izq arriba-der) 1) 100)
						   ((= (+ abajo-izq arriba-der) 2) 100)
						   ((= (+ abajo-izq arriba-der) 3) 
								(if (= fila 1) 5000 ;Solo tendria que colocar ahi la ficha para ganar
								1000))
							(t 0))
					0)
				0)
				)))
	      (let* ((altura (altura-columna tablero columna))
		     ;(fila (1- altura))
		     (abajo (contar-abajo tablero ficha-actual columna fila))
		     (der (contar-derecha tablero ficha-actual columna fila))
		     (izq (contar-izquierda tablero ficha-actual columna fila))
		     (abajo-der (contar-abajo-derecha tablero ficha-actual columna fila))
		     (arriba-izq (contar-arriba-izquierda tablero ficha-actual columna fila))
		     (abajo-izq (contar-abajo-izquierda tablero ficha-actual columna fila))
		     (arriba-der (contar-arriba-derecha tablero ficha-actual columna fila)))
		(setf puntuacion-actual
		      (+ puntuacion-actual
			  ;Miramos si estan libres las columnas del medio de la primera fila
			 (if (and (= fila 0) (= columna 3) (null (obtener-ficha tablero columna fila))) 6000
				(if (and (= fila 0) (or (= columna 4)(= columna 2)) (null (obtener-ficha tablero columna fila))) 5000
			 
			 ; ;Miramos que no haya otra ficha encima
			 (if (< fila 5)
				(if (null (obtener-ficha tablero columna (+ 1 fila)))
				 (cond ((= abajo 0) 0)
					   ((= abajo 1) 10)
					   ((= abajo 2) 100)
					   ((= abajo 3) 6000)
					   (t 0))
					   0)
			  0)))
			  
			  ;Miramos si hay una ficha del adversario a la izquierda
			  (if (> columna 1) 
					(if (null (obtener-ficha tablero (- columna 2) fila))
						(cond ((= (+ der izq) 0) 0)
						 ((= (+ der izq) 1) 100)
					     ((= (+ der izq) 2) 100)
					     ((= (+ der izq) 3) 6000)
						 (t 0))
					0)
			  0)
			  
			  ;Miramos si hay una ficha del adversario a la derecha
			  (if (< columna 5) 
					(if (null (obtener-ficha tablero (+ columna 2) fila))
						(cond ((= (+ der izq) 0) 0)
						 ((= (+ der izq) 1) 100)
					     ((= (+ der izq) 2) 100)
					     ((= (+ der izq) 3) 6000)
						 (t 0))
					0)
			  0)
			  
			  
			  ;DIAGONAL ABAJO_DER-ARRIBA_IZQ
				;Miramos que no haya una ficha del adversario que impida la linea por la izquierda
				(if (and (> columna 1) (< fila 4))
					(if (null (obtener-ficha tablero (- columna 2) (+ fila 2)))
						(cond ((= (+ abajo-der arriba-izq) 0) 0)
						   ((= (+ abajo-der arriba-izq) 1) 100)
						   ((= (+ abajo-der arriba-izq) 2) 100)
						   ((= (+ abajo-der arriba-izq) 3) 2000)
						   (t 0))
					0)
				0)
				
			 ;Miramos que no haya una ficha del adversario que impida la linea por la derecha
				(if (and (< columna 4) (> fila 1))
					(if (null (obtener-ficha tablero (+ columna 2) (- fila 2))) 
						(cond ((= (+ abajo-der arriba-izq) 0) 0)
						   ((= (+ abajo-der arriba-izq) 1) 100)
						   ((= (+ abajo-der arriba-izq) 2) 100)
						   ((= (+ abajo-der arriba-izq) 3) 
								(if (= fila 1) 6000 
								1000))
							(t 0))
					0)
				0)
				
			 ;DIAGONAL ABAJO_IZQ-ARRIBA_DER
				;Miramos que no haya una ficha del adversario que impida la linea por la derecha
				(if (and (< columna 4) (< fila 4)) 
					(if (null (obtener-ficha tablero (+ columna 2) (+ 2 fila)))
						(cond ((= (+ abajo-izq arriba-der) 0) 0)
						((= (+ abajo-izq arriba-der) 1) 100)
						((= (+ abajo-izq arriba-der) 2) 100)
						((= (+ abajo-izq arriba-der) 3) 2000)
						(t 0))
					0)
				0)
				
				;Miramos que no haya una ficha del adversario que impida la linea por la izquierda
				(if (and (> columna 1) (> fila 1))
					(if (null (obtener-ficha tablero (- columna 2) (- fila 2)))
						(cond ((= (+ abajo-der arriba-izq) 0) 0)
						   ((= (+ abajo-izq arriba-der) 1) 100)
						   ((= (+ abajo-izq arriba-der) 2) 100)
						   ((= (+ abajo-izq arriba-der) 3) 
								(if (= fila 1) 6000 ;Solo tendria que colocar ahi la ficha para ganar
								1000))
							(t 0))
					0)
				0))))))
	(- puntuacion-actual puntuacion-oponente)))))
;; -------------------------------------------------------------------------------
;; Jugadores
;; -------------------------------------------------------------------------------

(defvar *jugador-aleatorio* (make-jugador :nombre 'Jugador-aleatorio
					  :f-jugador #'f-jugador-aleatorio
					  :f-eval  #'f-eval-aleatoria))

(defvar *jugador-bueno* (make-jugador :nombre 'Jugador-bueno
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'f-eval-bueno))

(defvar *jugador-humano* (make-jugador :nombre 'Jugador-humano
				       :f-jugador #'f-jugador-humano
				       :f-eval  #'f-no-eval))

(defvar *jugador-1* (make-jugador :nombre 'Jugador-bueno
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'heuristica))
					  
(defvar *jugador-tito* (make-jugador :nombre 'Jugador-bueno
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'tito))
					  
(defvar *jugador-bro* (make-jugador :nombre 'Jugador-bueno
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'bro))

;; -------------------------------------------------------------------------------
;; Algunas partidas de ejemplo:
;; -------------------------------------------------------------------------------

(setf *verbose* t)

;(print (partida *jugador-aleatorio* *jugador-aleatorio*))
;(print (partida *jugador-aleatorio* *jugador-bueno* 4))
;(print (partida *jugador-bueno* *jugador-aleatorio* 4))
;(print (partida *jugador-bueno* *jugador-bueno* 4))
;(print (partida *jugador-humano* *jugador-humano*))
;(print (partida *jugador-humano* *jugador-aleatorio* 4))
(print (partida *jugador-1* *jugador-bro* 4))
;(print (partida *jugador-aleatorio* *jugador-humano*))

;;

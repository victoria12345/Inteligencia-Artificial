(defpackage :2302_P08_5add0 
(:use :common-lisp :conecta4)
(:export :heuristica :*alias*))

(in-package 2302_P08_5add0)
(defvar *alias* '|CHIQUI3.0|) 

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
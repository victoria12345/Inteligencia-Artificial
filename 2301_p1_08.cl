;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scalar-product (x y)
;;; Calcula el producto escalar de dos vectores.
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT: x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: producto escalar entre x e y
;;;
(defun scalar-product (x y)
  (if (or(null x)(null y)) 0
    (+(* (car x) (car y))
    (scalar-product (cdr x) (cdr y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; formula-cos-dis
;;; Aplica la formula de la distancia coseno
;;;
;;; INPUT: xx: norma del vector x
;;;        yy: norma del vector y
;;;		   xy: producto escalar de x e y
;;; OUTPUT: coseno distancua entre x e y
;;;
(defun formula-cos-dis (xx yy xy)
	(- 1 (/ xy
    (* (sqrt xx)
    (sqrt yy)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-rec (x y)
;;; Calcula la distancia coseno de un vector de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;; Si uno de los dos es el vector 0, se devuelve NIL
;;;
;;; INPUT: x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;
(defun cosine-distance-rec (x y)
  (let ((xy  (scalar-product x y)) (xx (scalar-product x x)) (yy (scalar-product y y)))
    (if (or (= 0 xx) (= 0 yy))
    nil
    (formula-cos-dis xx yy xy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scalar-product-mapcar (x y)
;;; Calcula el producto escalar de dos vectores.
;;; En vez de recursivamente, utilizando "mapcar"
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT: x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: producto escalar entre x e y
;;;
(defun scalar-product-mapcar (x y)
	(if (or (null x) (null y)) 0
	(apply #'+ (mapcar #'* x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-mapcar
;;; Calcula la distancia coseno de un vector usando mapcar
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT:  x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;
(defun cosine-distance-mapcar (x y)
	(let((xy (scalar-product-mapcar x y)) (xx (scalar-product-mapcar x x)) (yy (scalar-product-mapcar y y)))
	(if (or (= 0 xx) (= 0 yy))
	nil
		(formula-cos-dis xx yy xy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simil-vector-lst
;;; Lista de vectores y su similaridad.
;;; Solo van a pertenecer a esta lista los que superen el niveld e confianza
;;; INPUT:  vector: vector que representa a una categoria,
;;;                 representado como una lista
;;;         lst-of-vectors vector de vectores
;;;         confidence: Nivel de confianza
;;; OUTPUT: Lista de tuplas (vector similitud)
;;;
(defun simil-vector-lst (x lst confidence)
	(mapcan #'(lambda(y)
				(let ((sim (cosine-distance-mapcar x y)))
				(if (>= (- 1 confidence) sim) (list (list y sim))))) lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; copia-lista
;;; Devuelve una copia de una lista
;;; INPUT:  lst: lista que queremos copiar
;;; OUTPUT: Una lista que es una copia de lst
;;;				
(defun copia-lista (lst)
  	(if (atom lst)
		lst
		(cons (car lst) (copia-lista (cdr lst)))))
				
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; order-vectors-cosine-distance
;;; Devuelve aquellos vectores similares a una categoria
;;; INPUT:  vector: vector que representa a una categoria,
;;;                 representado como una lista
;;;         lst-of-vectors vector de vectores
;;;         confidence-level: Nivel de confianza (parametro opcional)
;;; OUTPUT: Vectores cuya semejanza con respecto a la
;;;         categoria es superior al nivel de confianza ,
;;;         ordenados
;;;
(defun order-vectors-cosine-distance (vector lst-of-vectors &optional (confidence-level 0))
	(if (or (null vector) (null lst-of-vectors) (> confidence-level 1) (< confidence-level 0))  nil
		(mapcar #'first (sort (copia-lista (simil-vector-lst vector lst-of-vectors confidence-level)) #'< :key #'second))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cats-simil
;;; Calcula la distancia coseno de un vector y una lista de categorias
;;; INPUT:  vector: vector
;;;         categories: lista de categorias
;;;         distance-measure: funcion a usar para la distancia coseno
;;; OUTPUT: Lista de tuplas (categoria distancia)
;;;
(defun cats-simil (vector categories distance-measure)
	(mapcar #'(lambda(x) (list (car x) (funcall distance-measure (cdr vector) (cdr x)))) categories))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; best-cat
;;; Busca la mejor categoria para un vector
;;; INPUT:  vector: vector
;;;         categories: lista de categorias
;;;         distance-measure: funcion a usar para la distancia coseno
;;; OUTPUT: Tupla (numero-de-categoria distancia-coseno)
;;;
(defun best-cat (vector categories distance-measure)
	(car (sort (copia-lista (cats-simil vector categories distance-measure)) #'< :key #'second)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-vectors-category (categories vectors distance-measure)
;;; Clasifica a los textos en categorias .
;;;
;;; INPUT : categories: vector de vectores, representado como
;;;                     una lista de listas
;;;         texts:      vector de vectores, representado como
;;;                     una lista de listas
;;;         distance-measure: funcion de distancia
;;; OUTPUT: Pares formados por el vector que identifica la categoria
;;;         de menor distancia , junto con el valor de dicha distancia
;;;
( defun get-vectors-category (categories texts distance-measure)
	(unless (null (car texts))
		(mapcar #'(lambda(x) (best-cat x categories distance-measure)) texts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; newton
;;; Estima el cero de una funcion mediante Newton-Raphson
;;;
;;; INPUT : f: funcion cuyo cero se desea encontrar
;;;         df: derivada de f
;;;         max-iter: maximo numero de iteraciones
;;;         x0: estimacion inicial del cero (semilla)
;;;         tol: tolerancia para convergencia (parametro opcional)
;;; OUTPUT: estimacion del cero de f o NIL si no converge
;;;
(defun newton (f df max-iter x0 &optional (tol 0.001))
  (if (= max-iter 0)
    nil
      (if (< (abs (funcall f x0)) tol)
       x0
        (newton f df (- max-iter 1) (- x0 (/ (funcall f x0) (funcall df x0))) tol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; one-root-newton
;;; Prueba con distintas semillas iniciales hasta que Newton
;;; converge
;;;
;;; INPUT: f: funcion de la que se desea encontrar un cero
;;;        df: derivada de f
;;;        max-iter: maximo numero de iteraciones
;;;        semillas : semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: el primer cero de f que se encuentre, o NIL si se diverge
;;;         para todas las semillas
;;;
(defun one-root-newton (f df max-iter semillas &optional ( tol 0.001))
  (cond
    ((null semillas) nil)
    (t (newton f df max-iter (first semillas) tol))
    (t (one-root-newton f df max-iter (rest semillas) tol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all-roots-newton
;;; Prueba con distintas semillas iniciales y devuelve las raices
;;; encontradas por Newton para dichas semillas
;;;
;;; INPUT: f: funcion de la que se desea encontrar un cero
;;;        df: derivada de f
;;;        max-iter: maximo numero de iteraciones
;;;        semillas: semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: las raices que se encuentren para cada semilla o nil
;;;          si para esa semilla el metodo no converge
;;;
(defun all-roots-newton (f df max-iter semillas &optional ( tol 0.001))
     (mapcar #'(lambda(x) (newton f df max-iter x tol)) semillas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list-not-nil-roots-newton
;;; Elimina de la lista de salida de all-roots-newton los
;;; elementos que sean nil
;;;
;;; INPUT: f : funcion de la que se desea encontrar un cero
;;;        df : derivada de f
;;;        max-iter : maximo numero de iteraciones
;;;        semillas : semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: Las raices no nil que se encuentran para las semillas
;;;
(defun list-not-nil-roots-newton (f df max-iter semillas &optional (tol 0.001))
 (mapcan #'nconc (all-roots-newton f df max-iter semillas tol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst
;;; Combina un elemento dado con todos los elementos de una lista
;;;
;;; INPUT: elem: elemento a combinar
;;;        lst: lista con la que se quiere combinar el elemento
;;;
;;; OUTPUT: lista con las combinacion del elemento con cada uno de los
;;;         de la lista
(defun combine-elt-lst (elt lst)
	(if (null lst) '()
	(mapcar #'(lambda(x) (list elt x)) lst)))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; limpiar
;;; funcion auxiliar para evitar parentesis "extra"
;;;
;;; INPUT lst:lista quu queremos "limpiar"
;;; OUTPUT: lista de la manera pedida en el ejercicio 3.3
;;;
(defun limpiar (lst)
	(if (null (cdr lst)) 
		(car lst)
		(append (car lst) (limpiar (cdr lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst
;;; Calcula el producto cartesiano de dos listas
;;;
;;; INPUT: lst1: primera lista
;;;        lst2: segunda lista
;;;
;;; OUTPUT: producto cartesiano de las dos listas
(defun combine-lst-lst (lst1 lst2)
	(limpiar (mapcar #'(lambda(x) (combine-elt-lst x lst2)) lst1 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst-aux
;;; Combina un elemento dado con todos los elementos de una lista
;;; Igual a combine-elt-lst pero utilizamos la funcion "cons" para evitar
;;; problemas de parentesis
;;; Creaada como funcion auxiliar de combine-list-of-lsts
;;;
;;; INPUT: elem: elemento a combinar
;;;        lst: lista con la que se quiere combinar el elemento
;;;
;;; OUTPUT: lista con las combinacion del elemento con cada uno de los
;;;         de la lista
(defun combine-elt-lst-aux (elt lst)
	(if (null lst) '()
	(mapcar #'(lambda(x)(cons elt x)) lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts-aux
;;; Funcion auxiliar que combina todos los elementos de dos listas
;;; Utiliza la funcion combine-elt-lst-aux, creada especificamente para
;;; este ejercicio y que no haya "problemas de parentesis"
;;;
;;; INPUT: lst1: lista 1
;;;        lst2: lista 2
;;;
;;; OUTPUT: lista con todas las posibles combinaciones de elementos
(defun combine-list-of-lsts-aux (lst1 lst2)
	(cond ((null lst1) lst2)
	((null lst2) lst1)
	(T (limpiar (mapcar #'(lambda(x) (combine-elt-lst-aux x lst2)) lst1)))))
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts
;;; Calcula todas las posibles disposiciones de elementos
;;; pertenecientes a N listas de forma que en cada disposicion
;;; aparezca unicamente un elemento de cada lista
;;;
;;; INPUT: lstolsts: lista de listas
;;;
;;; OUTPUT: lista con todas las posibles combinaciones de elementos
(defun combine-list-of-lsts (lstolsts)
	(if (null lstolsts) '(())
	(combine-list-of-lsts-aux (car lstolsts) (combine-list-of-lsts (cdr lstolsts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; defino operadores logicos
(defconstant +bicond+ '<=>)
(defconstant +cond+   '=>)
(defconstant +and+    '^)
(defconstant +or+     'v)
(defconstant +not+    '!)

;; definiciones de valores de verdad, conectores y atomos
(defun truth-value-p (x)
  (or (eql x T) (eql x NIL)))

(defun unary-connector-p (x)
  (eql x +not+))

(defun binary-connector-p (x)
  (or (eql x +bicond+)
      (eql x +cond+)))

(defun n-ary-connector-p (x)
  (or (eql x +and+)
      (eql x +or+)))

(defun bicond-connector-p (x)
  (eql x +bicond+))

(defun cond-connector-p (x)
    (eql x +cond+))

(defun connector-p (x)
  (or (unary-connector-p  x)
      (binary-connector-p x)
      (n-ary-connector-p  x)))

(defun positive-literal-p (x)
  (and (atom x)
       (not (truth-value-p x))
       (not (connector-p x))))

(defun negative-literal-p (x)
  (and (listp x)
       (eql +not+ (first x))
       (null (rest (rest x)))
       (positive-literal-p (second x))))

(defun literal-p (x)
  (or (positive-literal-p x)
      (negative-literal-p x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expand-bicond
;;; Recibe una expresion con bicondicional y la transforma en una con
;;; and's y or's
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : fbf - Formula bien formada (FBF) sin bicondicional
;;;
(defun expand-bicond (fbf)
 (list +or+ (list +and+ (second fbf) (third fbf)) (list +and+ (list +not+ (second fbf) +not+ (third fbf)))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expand-cond
;;; Recibe una expresion con condicional y la transforma en una con
;;; and's y or's
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : fbf - Formula bien formada (FBF) sin condicional
;;;
(defun expand-cond (fbf)
  (list +or+ (list +not+ (second fbf)) (third fbf)))
  
  
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expand
;;; Recibe una expresion con condicional o bicondicional
;;; y la transforma en una con and's y or's
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) con condicionales
;;; OUTPUT : fbf - Formula bien formada (FBF) sin condicionales
;;;

(defun expand(fbf)
	(if (equal T (bicond-connector-p (first fbf)))
			(list +and+ (expand-bicond fbf))
	(list +and+ (expand-cond fbf))))

;;;
;;; create-childs
;;; va haciendo llamadas recursivas a expand-truth-tree-aux
;;; segun si es un and o un or
;;; 
;;; INPUT: lst lista literales hasta ahira
;;;			fbf - formula bien formada
;;; OUTPUT: lista no contradictoria de literales
;;;
(defun create-childs (lst fbf)
	(if (equal +or+ (first fbf))
		(mapcar #'(lambda(x) (expand-truth-tree-aux lst x)) (cdr fbf))
	
	(and_tree (expand-truth-tree-aux lst (cdr fbf)) lst)))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; and_tree
;;; Organiza la lista para cuando haya un and
;;;
;;; INPUT  : result : resultado de la funcion expand-truth-tree-aux (de los 
;;; 					operadores del and)
;;; 		 lst: lista de atomos
;;; OUTPUT : lista que debe ser evaluable su valor de verdad
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun and_tree (result lst)
	(if (null lst) result
	(mapcar #'(lambda(x)(append result (LIST x))) lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expand-truth-tree-aux
;;; Se encarga de ir ramificando la fbf de manera adecuada para finalmente
;;; poder comprobar si es verdad (en una funcion exterior)
;;;
;;; INPUT  : lst: lista de atomos
;;;			 fbf: formula bine formada
;;; OUTPUT : lista de listas de atomos.
;;;			 se puede ver como un arbol, en el que cada rama es una lista
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun expand-truth-tree-aux (lst fbf)
	(when (not (null fbf))

	(if (and (not (literal-p fbf)) (not (null fbf)))
		(cond ((unary-connector-p (first fbf)) (expand-truth-tree-aux lst (eliminar-not fbf)))
			( (binary-connector-p (first fbf)) (expand-truth-tree-aux lst (expand fbf)))
			( (n-ary-connector-p (first fbf))(create-childs lst fbf))
			( (literal-p (first fbf)) (and_tree (list (first fbf)) (expand-truth-tree-aux lst (cdr fbf))))
			(T (and_tree (expand-truth-tree-aux lst (first fbf)) (expand-truth-tree-aux lst (cdr fbf)))))
		(if (and (null lst)(literal-p fbf)) (and_tree (list fbf) lst)
			lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eliminar-not
;;; Reduce la expresion a un literal o a un literal negado
;;;
;;; INPUT  : x - expresion de un literal negado multiples veces
;;; OUTPUT : x - literal negado o literal positivo
;;;
(defun eliminar-not (x)
	(if (or (null x) (literal-p x)) x
	;;si no eliminamos...
	(let ((connector (first x)))
	;;Comprobamos si es una negacion
	(if (unary-connector-p connector)
		(let ((aux (second x)))
		;;si es un not volvemos a reducir
		(if (unary-connector-p (first aux)) (eliminar-not (second aux))
		;;si no modificamos la operacion
			(eliminar-not (cons (intercambio (first aux)) 
						(mapcar #'(lambda(x) (list +not+ x)) (cdr aux)))))
		)
	(cons connector (mapcar #'eliminar-not (cdr x)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eliminar-parentesis
;;; Elimina los parentesis necesarios de una lista para solo obtener atomos
;;;
;;; INPUT  : atomos : lista de atomos
;;;			 fbf: lista de atomos resultante de expand-truth-tree-aux
;;; 		 lst: lista de atomos
;;; OUTPUT : lista de atomos que ya puede ser evaluable por las funciones creadas
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminar-parentesis (atomos fbf)
	(cond
		((null fbf) nil)
		((equal T (literal-p fbf)) (cons fbf atomos))
		((equal T (literal-p (first fbf))) (append (list (first fbf)) (eliminar-parentesis atomos (cdr fbf))))
		(T (append (first fbf) (eliminar-parentesis atomos (cdr fbf))))))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; intercambio
;;; Intercambia un and por un or
;;;
;;; INPUT  : connector: conector que debe ser intercambiado
;;; 		 lst: lista de atomos
;;; OUTPUT : un or (cuando conector = and) o un and (conector = or)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun intercambio (connector)
	(cond	
		((equal +or+ connector) +and+)
		((equal +and+ connector) +or+)
		(t connector)))

		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; valor-verdad
;;; Devuelve T o NIL segun el valor-verdad de esa lista
;;;
;;; INPUT  : x: lista de valores de verdad (T o F)
;;; OUTPUT : T si son todos T, NIL, si no
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun valor-verdad (x)
	(cond	((null x) T)
			((equal NIL (first x)) nil)
			((valor-verdad (cdr x)))))

			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evaluar
;;; Evalua si hay contradicciones en una lista de atomos
;;;
;;; INPUT  : lst: lista de atomos
;;; 		 
;;; OUTPUT : lista con tantos NIL como atomos con contradiccion haya
;;;			   si un atomo no tiene contradccion le corresponde un T
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun evaluar (lst)
	(if (or (null lst) (literal-p lst)) lst
		(mapcar #'(lambda(x) (evaluar-aux (eliminar-not x) (cdr lst))) lst)))
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evaluar-aux
;;; Mira las contradicciones de un literal con otra lista de literales
;;;
;;; INPUT  : elt: listeral que miramos si tiene contradicciones
;;; 		 lst: lista de atomos
;;; OUTPUT : T si no hay contradiccion, NIL, si la hay
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun evaluar-aux (elt lst)
	(if (or (null lst) (literal-p lst))
		(not (equal elt (eliminar-not (list +not+ lst))))
	(and (evaluar-aux elt (first lst)) (evaluar-aux elt (cdr lst)))))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAT
;;; Mira si un arbol es SAT
;;;
;;; INPUT  : ramas: lista de listas de atomos
;;; OUTPUT : T si alguna de las ramas no tiene contradicciones, Nil si no
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun SAT (ramas)
	(cond ((null ramas) nil)
		  ((equal T (first ramas)) T)
		  (T (SAT (cdr ramas)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; truth-tree
;;; Recibe una expresion y construye su arbol de verdad para
;;; determinar si es SAT o UNSAT
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : T   - FBF es SAT
;;;          N   - FBF es UNSAT
;;;
(defun truth-tree (fbf)
	(if (null fbf) NIL
	(SAT (mapcar #'(lambda(x) (valor-verdad (evaluar (eliminar-parentesis NIL x))))(expand-truth-tree-aux NIL fbf)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shortest-path-improved
;;; Version de busqueda en anchura que no entra en recursion
;;; infinita cuando el grafo tiene ciclos
;;; INPUT:   end: nodo final
;;;          queue: cola de nodos por explorar
;;;          net: grafo
;;; OUTPUT: camino mas corto entre dos nodos
;;;         nil si no lo encuentra

(defun bfs-improved (end queue net)
  )

(defun shortest-path-improved (end queue net)
  )

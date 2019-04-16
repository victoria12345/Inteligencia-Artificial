%% Esta funcion nos la daban en la practica %%
% La tercera lista es el resultado de concatenar
% las dos primeras

% Condicion de parada
concatena([],L,L).
concatena([X|L1], L2, [X|L3]):-
    concatena(L1,L2,L3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				EJERCICIO 1 			   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Condicion de parada
duplica([],[]).

% Comprobamos que el primer elemento sea el primero
% y el segundo de L1 y comparamos el resto de L
% con el resto, a partir del 3 de L1
duplica(L,L1) :-
  [T|H] = L,
  [X|C] = L1,
  [X|B] = C,
    member(T,L1), member(T, C), duplica(H,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				EJERCICIO 2 			   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Condicion de parada
invierte([],[]).

% R es el resultado de concatenar el Resto de la lista
% lo concatenamos con H de manera que el resultado es L
invierte([H|T],L):-
     invierte(T,R), concatena(R,[H],L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				EJERCICIO 3 			   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Condicion de parada
palindromo([]).

palindromo(L):-
    invierte(L, X), L = X.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				EJERCICIO 4 			   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Si N es 0 L deberia ser igual a L2 y L1 = []
divide(L,0,[],L2):-
    L = L2.

% Comprobamos que el primero de L1 sea el primero de L1
% y que N sea mayor que cero, si no, se debe devolver
% falso

divide([H|T], N, [H|R],L2):-
    N > 0,
    N1 is N - 1,
    divide(T,N1,R,L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				EJERCICIO 5 			   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Condicion de parada
aplasta(L, [L]):-
	\+ is_list(L).
aplasta([], []).

aplasta([H|T], L_aplastada) :-
    aplasta(H, L1),
    aplasta(T, L2),
    concatena(L1, L2, L_aplastada).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				EJERCICIO 6 			   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Comprobamos que sea positivo.
% El tercer argumento en primos(N, L, K) indica que N
% no tiene factores primos menores que K
primos(N, L) :- N > 0,  primos(N, L, 2).

% Si N es 1 la lista de factores primos es vacía (obviamos al 1)
primos(1, [], _) :- !.

% Comprobamos si el primer elemento de la lista de factores divide a N.
% Si es así, llama a primos sobre N/F y el resto de la lista de factores.
primos(N, [F|L], F) :-
  R is N // F, N =:= R * F, !, primos(R, L, F).

% Si el primer factor de la lista no divide a N, buscamos el siguiente
primos(N, L, F) :-
  siguiente_factor(N, F, NF), primos(N, L, NF).

% Caso base, el siguiente factor primo posible mayor que 2 es 3.
siguiente_factor(_, 2, 3) :- !.

% Si NF no es primo, nunca dividirá a N ya que N ya está dividido por
% los factores primos que componen NF, por lo que seguirá buscando el siguiente
% factor sin problema.
siguiente_factor(N, F, NF) :- F * F < N, !, NF is F + 2.

% Si F es mayor que la raíz de N no es posible que sea factor primo suyo
siguiente_factor(N, _, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				EJERCICIO 7 			   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EJERCICIO 7.1 %%

% Cuando X coincide con el primero de la lista
% Annade X al principio de Lfront y llama de nuevo a la
% funcion con el resto de la lista
cod_primero(X, [X|R],Lrem, Lfront):-
    cod_primero(X,R,Lrem, L2), Lfront = [X|L2].

% Devuelve Lfront = [X] y Lrest como el resto de la lista
% esto lo hace cuando ya el primero no coincide con X|L2
cod_primero(X,[P|R],[P|R],[X]).

% Caso en el que la lista es vacia
cod_primero(X,[],[],[X]).


%% EJERCICIO 7.2 %%

% Condicion de parada
cod_all([],[]).

% Aplicamos al primer elemento cod_primero
% A la segunda lista concatenamos el resultado de cod_primero
% y volvemos a llamar a cod_primero con el primer elemento de Lrest
% de cod_primero, que sera distinto a X.

cod_all([X|T], [P|Lfront]):-
    cod_primero(X,T, L1, P),
    cod_all(L1, Lfront).

%% EJERCICIO 7.3 %%

% Condicion de parada
run_length_aux([],[]).

% Recibe una lista de listas de el mismo elemento, X
% Calcula su longitud, N
% En la segunda lista concatena elementos de la forma
% [X,N]
run_length_aux([[H|T]|R], [[N,H]|L]):-
    length([H|T],N),
    run_length_aux(R,L).

% Condicion de parada
run_length([],[]).

% aplica cod_all a la primera lista
% Llama a su funcion auxiliar para obtener elementos
% de la forma ["elemento", "num. de veces que aparece"]
run_length(L1,L2):-
   cod_all(L1,L),
    run_length_aux(L,L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				EJERCICIO 8			    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EJERCICIO 8.0 %%

% Si la lista de elementos esta vacia el arbol es nulo
build_tree([], nil).

% Caso cuando solo queda un nodo que añadir al arbol
build_tree([X-_], tree(X, nil, nil)).

% Caso general, el primer nodo sera el hijo izquierdo del nodo raiz
% y recursivamente va construyendo los demas hijos.
build_tree([X-_|Rest], Tree) :- build_tree(Rest, S), Tree = tree(1, tree(X, nil, nil), S).


%% EJERCICIO 8.1 %%

% Si el nodo a codificar es el hijo izquierdo añadimos
% un cero a la codificacion.
encode_elem(X1, X2, tree(1, tree(X1, nil, nil), _)) :- concatena([0], [], X2).

% Si nos encontramos ante el hijo derecho del ultimo nivel
% no hace falta añadir mas digitos.
encode_elem(X1, [], tree(X1, nil, nil)).

% Caso general, nos movemos hacia la derecha y añadimos un 1
% a la lista de codificacion.
encode_elem(X1, X2, tree(1, _, R)) :- encode_elem(X1, List, R), concatena([1], List, X2).


%% EJERCICIO 8.2 %%

% Si no hay elementos en la lista la lista codificada es vacia.
% Sirve como control de errores.
encode_list([], [], _).

% Si solo queda un elemento por codificar, lo hacemos y concatenamos
% con la lista donde se guardara la codificacion.
encode_list([X], L2, Tree) :- encode_elem(X, Laux, Tree), concatena([Laux], [], L2).

% Caso general.
% Aplicamos encode_elem recursivamente y vamos concatenando la listas.
encode_list([H|T], L2, Tree) :- encode_elem(H, Laux, Tree), encode_list(T, L, Tree), concatena([Laux], L, L2).


%% EJERCICIO 8.3 %%

dictionary([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

% Para la realizacion de encode necesitaremos varios predicados auxiliares.
% contar_elemento cuenta el numero de veces que aparece un elemento en una lista.
contar_elemento(_, [], 0).
contar_elemento(X, [X|T], M) :- contar_elemento(X, T, N), M is N+1.
contar_elemento(X, [Y|T], M) :- X \= Y, contar_elemento(X, T, M).

% contar_elementos cuenta el numero de veces que aparece en una lista
% cada elemento de otra lista que le pasamos.
contar_elementos([], _, _).
contar_elementos([X], L, S) :- contar_elemento(X, L, C), concatena([X-C], [], S).
contar_elementos([X|T], L, S) :- contar_elemento(X, L, C), concatena([X-C], D, S),
                                  contar_elementos(T, L, D), T \= [].

% Inserta en orden un elemento en una lista
insert([X-P], [], [X-P]).
insert([X-P], [Y-Q|Rest], [X-P, Y-Q|Rest]) :- P =< Q.
insert([X-P], [Y-Q|Rest], [Y-Q|Z]) :- insert([X-P], Rest, Z), P > Q.

% Ordena una lista utilizando la insercion previamente creada.
sort_list([], []).
sort_list([X-P|Rest], L) :- sort_list(Rest, S), insert([X-P], S, L).

% Primero obtenemos el diccionario, despues contamos cuantas veces aparece cada
% letra en la lista, la ordenamos e invertimos para que sea admitida por el
% predicado build_tree, construimos el arbol y codificamos en funcion del arbol.
encode(L1, L2) :- dictionary(D), contar_elementos(D, L1, Laux), sort_list(Laux, LS),
                  invierte(LS, LInv), build_tree(LInv, Tree), encode_list(L1, L2, Tree).

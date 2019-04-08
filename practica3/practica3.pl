    
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
aplasta([], []).

aplasta([H|T], L_aplastada) :-
    aplasta(H, L1),
    aplasta(T, L2),
    concatena(L1, L2, L_aplastada).

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

%Caso en el que la lista es vacia
cod_primero(X,[],[],[X]).

%% EJERCICIO 7.2 %%	
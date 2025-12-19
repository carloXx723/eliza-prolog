% Para los warnings
:- discontiguous padre/2.
:- discontiguous madre/2.
:- discontiguous esposos/2.
:- discontiguous amigo/2.
:- discontiguous companero/2.
:- discontiguous vecino/2.

% ======================================
% HECHOS
% ======================================

padre(homero, bart).
madre(marge, bart).

padre(homero, lisa).
madre(marge, lisa).

padre(homero, maggie).
madre(marge, maggie).

padre(abe, homero).
madre(mona, homero).
padre(clancy, marge).
madre(jacqueline, marge).

padre(clancy, patty).
madre(jacqueline, patty).

padre(clancy, selma).
madre(jacqueline, selma).

padre(bob, ling).
madre(selma, ling).

esposos(homero, marge).
esposos(marge, homero).

esposos(abe, mona).
esposos(mona, abe).

esposos(clancy, jacqueline).
esposos(jacqueline, clancy).

esposos(bob, selma).
esposos(selma, bob).

amigo(homero, lenny).
amigo(homero, carl).
amigo(lenny, carl).

amigo(bart, milhouse).
amigo(lisa, janey).

amigo(marge, patty).
amigo(marge, selma).

companero(homero, lenny).
companero(homero, carl).
companero(lenny, carl).

companero(bart, milhouse).
companero(bart, rafa).
companero(bart, nelson).

companero(lisa, janey).
companero(lisa, rafa).
companero(lisa, milhouse).
companero(lisa, nelson).

companero(marge, patty).
companero(marge, selma).

vecino(bart, milhouse).
vecino(lisa, milhouse).

edad(homero, 39).
edad(marge, 36).
edad(bart, 10).
edad(lisa, 8).
edad(maggie, 1).

trabaja_en(homero, planta_nuclear).
trabaja_en(marge, ama_de_casa).
trabaja_en(bart, estudiante).
trabaja_en(lisa, estudiante).
trabaja_en(maggie, bebe).

serie(los_simpson).

estreno(los_simpson, '17_diciembre_1989').
temporadas(los_simpson, 35).
episodios(los_simpson, 750).

creador(los_simpson, matt_groening).

cadena(los_simpson, fox).

genero(los_simpson, animacion).
genero(los_simpson, comedia).


pais_origen(los_simpson, estados_unidos).

ciudad_ficticia(los_simpson, springfield).

% ======================================
% REGLAS
% ======================================

amigo(X, Y) :- amigo(Y, X).
companero(X, Y) :- companero(Y, X).
vecino(X, Y) :- vecino(Y, X).

trabajo(Persona, Trabajo) :-
    trabaja_en(Persona, Trabajo).

dato_serie(Dato, Valor) :-
    call(Dato, los_simpson, Valor).

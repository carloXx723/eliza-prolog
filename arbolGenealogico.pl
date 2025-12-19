% Para los warnings
:- discontiguous padre/2.
:- discontiguous madre/2.
:- discontiguous hermano/2.
:- discontiguous hermana/2.
:- discontiguous esposos/2.

% ======================================
% HECHOS
% ======================================

padre(alberto, carlos).
madre(rosalinda, carlos).

padre(alberto, karla).
madre(rosalinda, karla).

padre(pepe, rosalinda).
madre(rosa, rosalinda).

padre(pepe, rubi).
madre(rosa, rubi).

padre(pepe, irma).
madre(rosa, irma).

padre(pepe, carmen).
madre(rosa, carmen).

padre(jose, alberto).
madre(elia, alberto).

padre(jose, armando).
madre(elia, armando).

padre(jose, blaneg).
madre(elia, blaneg).

padre(alejandro, aria).
madre(carmen, aria).

padre(alejandro, gael).
madre(carmen, gael).

padre(cesar, renata).
madre(irma, renata).

padre(cesar, alison).
madre(irma, alison).

padre(raul, sofia).
madre(blaneg, sofia).

esposos(alejandro, carmen).
esposos(carmen, alejandro).

esposos(cesar, irma).
esposos(irma, cesar).

esposos(raul, blaneg).
esposos(blaneg, raul).

esposos(alberto, rosalinda).
esposos(rosalinda, alberto).

% ======================================
% REGLAS
% ======================================

padres(X, Y) :- padre(X, Y).
padres(X, Y) :- madre(X, Y).

hermano(X, Y) :-
    padre(P, X),
    padre(P, Y),
    X \= Y.

hermano(X, Y) :-
    madre(M, X),
    madre(M, Y),
    X \= Y.

hermana(X, Y) :-
    hermano(X, Y).

abuelo(X, Y) :-
    padre(X, Z),
    padres(Z, Y).

abuela(X, Y) :-
    madre(X, Z),
    padres(Z, Y).

nieto(X, Y) :-
    abuelo(Y, X).

nieto(X, Y) :-
    abuela(Y, X).

tio(X, Y) :-
    hermano(X, Z),
    padres(Z, Y).

tia(X, Y) :-
    hermana(X, Z),
    padres(Z, Y).

primo(X, Y) :-
    padres(P1, X),
    padres(P2, Y),
    hermano(P1, P2),
    X \= Y.

hijo(X, Y) :-
    padres(Y, X).

padre_de(Y, X) :-
    hijo(X, Y).

madre_de(Y, X) :-
    hijo(X, Y).

esposo(X, Y) :-
    esposos(X, Y),
    padre(X, _).

esposa(X, Y) :-
    esposos(X, Y),
    madre(X, _).
% ======================================
% CONFIGURACION
% ======================================

:- discontiguous response/2.
:- dynamic paciente_actual/1.

:- consult('medicoextendido.pl').
:- consult('arbolGenealogico.pl').

paciente_actual(paciente1).

% ======================================
% TOKENIZACION
% ======================================

tokenize(Line, Tokens) :-
    string_lower(Line, Lower),
    split_string(Lower, " ", "?.!,", Tokens).

% ======================================
% ARBOL GENEALOGICO 
% ======================================

response(Tokens, Respuesta) :-
    consulta_familia(Tokens, Respuesta),
    !.

% ---------- PADRE ----------
consulta_familia([Xs,"es","padre","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( padre(X,Y)
      -> format(string(R), "~w si es padre de ~w", [X,Y])
      ;  format(string(R), "~w no es padre de ~w", [X,Y])
    ).

% ---------- MADRE ----------
consulta_familia([Xs,"es","madre","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( madre(X,Y)
      -> format(string(R), "~w si es madre de ~w", [X,Y])
      ;  format(string(R), "~w no es madre de ~w", [X,Y])
    ).

% ---------- HIJO ----------
consulta_familia([Xs,"es","hijo","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( hijo(X,Y)
      -> format(string(R), "~w si es hijo de ~w", [X,Y])
      ;  format(string(R), "~w no es hijo de ~w", [X,Y])
    ).

% ---------- HERMANO ----------
consulta_familia([Xs,"es","hermano","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( hermano(X,Y)
      -> format(string(R), "~w si es hermano de ~w", [X,Y])
      ;  format(string(R), "~w no es hermano de ~w", [X,Y])
    ).

% ---------- HERMANA ----------
consulta_familia([Xs,"es","hermana","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( hermana(X,Y)
      -> format(string(R), "~w si es hermana de ~w", [X,Y])
      ;  format(string(R), "~w no es hermana de ~w", [X,Y])
    ).

% ---------- ABUELO ----------
consulta_familia([Xs,"es","abuelo","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( abuelo(X,Y)
      -> format(string(R), "~w si es abuelo de ~w", [X,Y])
      ;  format(string(R), "~w no es abuelo de ~w", [X,Y])
    ).

% ---------- ABUELA ----------
consulta_familia([Xs,"es","abuela","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( abuela(X,Y)
      -> format(string(R), "~w si es abuela de ~w", [X,Y])
      ;  format(string(R), "~w no es abuela de ~w", [X,Y])
    ).

% ---------- NIETO ----------
consulta_familia([Xs,"es","nieto","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( nieto(X,Y)
      -> format(string(R), "~w si es nieto de ~w", [X,Y])
      ;  format(string(R), "~w no es nieto de ~w", [X,Y])
    ).

% ---------- TIO ----------
consulta_familia([Xs,"es","tio","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( tio(X,Y)
      -> format(string(R), "~w si es tio de ~w", [X,Y])
      ;  format(string(R), "~w no es tio de ~w", [X,Y])
    ).

% ---------- TIA ----------
consulta_familia([Xs,"es","tia","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( tia(X,Y)
      -> format(string(R), "~w si es tia de ~w", [X,Y])
      ;  format(string(R), "~w no es tia de ~w", [X,Y])
    ).

% ---------- PRIMO ----------
consulta_familia([Xs,"es","primo","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( primo(X,Y)
      -> format(string(R), "~w si es primo de ~w", [X,Y])
      ;  format(string(R), "~w no es primo de ~w", [X,Y])
    ).

% ---------- ESPOSO ----------
consulta_familia([Xs,"es","esposo","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( esposo(X,Y)
      -> format(string(R), "~w si es esposo de ~w", [X,Y])
      ;  format(string(R), "~w no es esposo de ~w", [X,Y])
    ).

% ---------- ESPOSA ----------
consulta_familia([Xs,"es","esposa","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( esposa(X,Y)
      -> format(string(R), "~w si es esposa de ~w", [X,Y])
      ;  format(string(R), "~w no es esposa de ~w", [X,Y])
    ).




% ======================================
% RESPUESTAS NORMALES (ELIZA)
% ======================================

% Saludos
response(["hola"], ["Hola","como","estas","?"]).
response(["hola","eliza"], ["Hola","como","estas","?"]).
response(["buenos","dias"], ["Buenos","dias","como","estas","?"]).
response(["buenas","tardes"], ["Buenas","tardes","como","estas","?"]).

% Estado de animo
response(["bien"], ["Me","alegro"]).
response(["mal"], ["Lamento","que","te","sientas","asi"]).
response(["me","siento","triste"], ["Quieres","contarme","mas","?"]).

% Diagnostico
response(["me","siento","enfermo"], iniciar_diagnostico).
response(["estoy","enfermo"], iniciar_diagnostico).

% Conversacion
response(["como","estas"], ["Estoy","bien","gracias"]).
response(["que","haces"], ["Estoy","aqui","para","escucharte"]).
response(["quien","eres"], ["Soy","Eliza"]).

% Despedida
response(["adios"], ["Adios","espero","haberte","ayudado"]).
response(["bye"], ["Bye","cuidate"]).

% ======================================
% EJECUTAR RESPUESTA
% ======================================

ejecutar_respuesta(iniciar_diagnostico) :-
    writeln("Voy a ayudarte con el diagnostico."),
    writeln("Como te llamas?"),
    read(Nombre),
    retractall(paciente_actual(_)),
    assertz(paciente_actual(Nombre)),
    diagnosticar(Nombre).

ejecutar_respuesta(Lista) :-
    is_list(Lista),
    atomic_list_concat(Lista, ' ', Texto),
    writeln(Texto).

ejecutar_respuesta(String) :-
    string(String),
    writeln(String).

% ======================================
% RESPONDER
% ======================================

respond(Tokens) :-
    response(Tokens, Resp),
    ejecutar_respuesta(Resp),
    !.

respond(_) :-
    writeln("No te entiendo, puedes explicarme un poco mas?").

% ======================================
% LOOP PRINCIPAL
% ======================================

eliza :-
    writeln("Hola, soy Eliza."),
    writeln("Escribe algo:"),
    loop.

loop :-
    write("> "),
    read_line_to_string(user_input, Line),
    ( Line == end_of_file ->
        true
    ;
        tokenize(Line, Tokens),
        respond(Tokens),
        ( Tokens = ["adios"] ; Tokens = ["bye"] )
        -> true
        ;  loop
    ).

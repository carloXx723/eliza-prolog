% ======================================
% CONFIGURACION
% ======================================

:- discontiguous response/2.
:- dynamic paciente_actual/1.

:- consult('medicoextendido.pl').
:- consult('arbolGenealogico.pl').
:- consult('losSimpson.pl').

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

consulta_familia([Xs,"es","padre","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( padre(X,Y)
      -> format(string(R), "~w si es padre de ~w", [X,Y])
      ;  format(string(R), "~w no es padre de ~w", [X,Y])
    ).

consulta_familia([Xs,"es","madre","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( madre(X,Y)
      -> format(string(R), "~w si es madre de ~w", [X,Y])
      ;  format(string(R), "~w no es madre de ~w", [X,Y])
    ).

consulta_familia([Xs,"es","hijo","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( hijo(X,Y)
      -> format(string(R), "~w si es hijo de ~w", [X,Y])
      ;  format(string(R), "~w no es hijo de ~w", [X,Y])
    ).

consulta_familia([Xs,"es","hermano","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( hermano(X,Y)
      -> format(string(R), "~w si es hermano de ~w", [X,Y])
      ;  format(string(R), "~w no es hermano de ~w", [X,Y])
    ).

consulta_familia([Xs,"es","hermana","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( hermana(X,Y)
      -> format(string(R), "~w si es hermana de ~w", [X,Y])
      ;  format(string(R), "~w no es hermana de ~w", [X,Y])
    ).

consulta_familia([Xs,"es","abuelo","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( abuelo(X,Y)
      -> format(string(R), "~w si es abuelo de ~w", [X,Y])
      ;  format(string(R), "~w no es abuelo de ~w", [X,Y])
    ).

consulta_familia([Xs,"es","abuela","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( abuela(X,Y)
      -> format(string(R), "~w si es abuela de ~w", [X,Y])
      ;  format(string(R), "~w no es abuela de ~w", [X,Y])
    ).

consulta_familia([Xs,"es","nieto","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( nieto(X,Y)
      -> format(string(R), "~w si es nieto de ~w", [X,Y])
      ;  format(string(R), "~w no es nieto de ~w", [X,Y])
    ).

consulta_familia([Xs,"es","tio","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( tio(X,Y)
      -> format(string(R), "~w si es tio de ~w", [X,Y])
      ;  format(string(R), "~w no es tio de ~w", [X,Y])
    ).

consulta_familia([Xs,"es","tia","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( tia(X,Y)
      -> format(string(R), "~w si es tia de ~w", [X,Y])
      ;  format(string(R), "~w no es tia de ~w", [X,Y])
    ).

consulta_familia([Xs,"es","primo","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( primo(X,Y)
      -> format(string(R), "~w si es primo de ~w", [X,Y])
      ;  format(string(R), "~w no es primo de ~w", [X,Y])
    ).

consulta_familia([Xs,"es","esposo","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( esposo(X,Y)
      -> format(string(R), "~w si es esposo de ~w", [X,Y])
      ;  format(string(R), "~w no es esposo de ~w", [X,Y])
    ).

consulta_familia([Xs,"es","esposa","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( esposa(X,Y)
      -> format(string(R), "~w si es esposa de ~w", [X,Y])
      ;  format(string(R), "~w no es esposa de ~w", [X,Y])
    ).

consulta_familia([Xs,"es","amigo","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( amigo(X,Y)
      -> format(string(R), "~w si es amigo de ~w", [X,Y])
      ;  format(string(R), "~w no es amigo de ~w", [X,Y])
    ).

% ======================================
% LOS SIMPSON
% ======================================

consulta_familia([Xs,"es","companero","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( companero(X,Y)
      -> format(string(R), "~w si es companero de ~w", [X,Y])
      ;  format(string(R), "~w no es companero de ~w", [X,Y])
    ).

consulta_familia([Xs,"es","vecino","de",Ys], R) :-
    atom_string(X, Xs),
    atom_string(Y, Ys),
    ( vecino(X,Y)
      -> format(string(R), "~w si es vecino de ~w", [X,Y])
      ;  format(string(R), "~w no es vecino de ~w", [X,Y])
    ).

consulta_familia(["en","que","trabaja",Xs], R) :-
    atom_string(X, Xs),
    ( trabaja_en(X, T)
      -> format(string(R), "~w trabaja en ~w", [X, T])
      ;  format(string(R), "No se en que trabaja ~w", [X])
    ).

consulta_familia(["cual","es","la","edad","de",Xs], R) :-
    atom_string(X, Xs),
    ( edad(X, E)
      -> format(string(R), "~w tiene ~w anos", [X, E])
      ;  format(string(R), "No se la edad de ~w", [X])
    ).

consulta_familia(["cuando","salieron","los","simpson"], R) :-
    estreno(los_simpson, F),
    format(string(R), "Los Simpson se estrenaron el ~w", [F]).

consulta_familia(["cuantas","temporadas","tienen","los","simpson"], R) :-
    temporadas(los_simpson, F),
    format(string(R), "Los Simpson tienen ~w temporadas", [F]).

consulta_familia(["quien","creo","los","simpson"], R) :-
    creador(los_simpson, C),
    format(string(R), "Los Simpson fueron creados por ~w", [C]).

consulta_familia(["en","que","canal","se","transmiten","los","simpson"], R) :-
    cadena(los_simpson, Canal),
    format(string(R), "Los Simpson se transmiten en ~w", [Canal]).

consulta_familia(["de","que","trata","los","simpson"], 
"Los Simpson son una serie animada que satiriza la vida de una familia estadounidense").

consulta_familia(["donde","viven","los","simpson"], R) :-
    ciudad_ficticia(los_simpson, C),
    format(string(R), "Los Simpson viven en la ciudad ficticia de ~w", [C]).

consulta_familia(["de","que","pais","son","los","simpson"], R) :-
    pais_origen(los_simpson, P),
    format(string(R), "Los Simpson son de ~w", [P]).

consulta_familia(["cual","es","el","genero","de","los","simpson"], R) :-
    genero(los_simpson, G),
    format(string(R), "El genero de Los Simpson es ~w", [G]).

% ======================================
% RESPUESTAS DIRECTAS
% ======================================

response(["hola"], ["Hola","como","estas","?"]).
response(["buenos","dias"], ["Buenos","dias","como","estas","?"]).
response(["buenas","tardes"], ["Buenas","tardes","como","estas","?"]).
response(["hola","eliza"], ["Hola","como","estas","?"]).
response(["hola","eliza","soy",Nombre],
         ["Hola",Nombre,"como","estas","?"]).

response(["bien"], ["Me","alegro"]).
response(["mal"],
         ["Lamento","que","te","sientas","asi,","por","que","te","sientes","mal","?"]).
response(["me","siento","mal"], ["Por","que","?"]).
response(["me","siento","triste"],
         ["Quieres","contarme","que","te","hace","sentirte","asi","?"]).
response(["me","siento","enfermo"], iniciar_diagnostico).
response(["estoy","enfermo"], iniciar_diagnostico).
response(["ayudame","a","saber","que","enfermedad","tengo"], iniciar_diagnostico).

response(["como","estas"],
         ["Estoy","bien","gracias","y","tu","?"]).
response(["que","haces"],
         ["Estoy","aqui","para","escucharte"]).
response(["quien","eres"],
         ["Soy","Eliza","un","programa","para","conversar","contigo"]).
response(["que","puedes","hacer"],
[
 "Te","puedo","ayudar","a","diagnosticar","enfermedades","como",
 "gripe,","alergia,","migrana,","resfriado,","difteria,",
 "escoliosis,","hipermetropia,",
 "tambien","te","puedo","ayudar","a","localizar","personas",
 "de","tu","arbol","genealogico",
 "y","podemos","conversar","un","poco","sobre","los","simpsons"
]).

response(["adios"], ["Adios","espero","haberte","ayudado"]).
response(["bye"], ["Bye","cuidate"]).


% ======================================
% MEDICO EXTENDIDO
% ======================================

pedir_nombre_paciente(Nombre) :-
    writeln("Cual es tu nombre?"),
    write("> "),
    read_line_to_string(user_input, Nombre),
    retractall(paciente_actual(_)),
    assertz(paciente_actual(Nombre)).

iniciar_diagnostico :-
    writeln("Entiendo, voy a ayudarte a obtener un diagnostico."),
    write("Cual es tu nombre? "),
    read(Nombre),
    retractall(paciente_actual(_)),
    assertz(paciente_actual(Nombre)),
    reset_paciente(Nombre),
    format("Perfecto ~w responde solo si o no a las preguntas.~n", [Nombre]),
    diagnostico_mas_probable(Nombre).
    

% ======================================
% EJECUTAR RESPUESTA
% ======================================

ejecutar_respuesta(iniciar_diagnostico) :-
    writeln("Entiendo, voy a ayudarte a obtener un diagnostico."),
    pedir_nombre_paciente(P),
    reset_paciente(P),
    format("Perfecto ~w responde solo si o no a las preguntas.~n", [P]),
    diagnostico_mas_probable(P).

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

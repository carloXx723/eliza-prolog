:- consult('medicoextendido.pl').
:- dynamic paciente_actual/1.
paciente_actual(paciente1).

% -------- TOKENIZACION --------
tokenize(Line, Tokens) :-
    string_lower(Line, Lower),
    split_string(Lower, " ", "?.!,", Tokens).

% -------- RESPUESTAS --------

% Saludos
response(["hola"], ["Hola","como","estas","?"]).
response(["buenos","dias"], ["Buenos","dias","como","estas","?"]).
response(["buenas","tardes"], ["Buenas","tardes","como","estas","?"]).
response(["hola","eliza"], ["Hola","como","estas","?"]).
response(["hola","eliza","soy",Nombre],
         ["Hola",Nombre,"como","estas","?"]).

% Estado de animo
response(["bien"], ["Me","alegro"]).
response(["mal"],
         ["Lamento","que","te","sientas","asi,","por","que","te","sientes","mal","?"]).
response(["me","siento","mal"], ["Por","que","?"]).
response(["me","siento","triste"],
         ["Quieres","contarme","que","te","hace","sentirte","asi","?"]).
response(["me","siento","enfermo"], iniciar_diagnostico).
response(["estoy","enfermo"], iniciar_diagnostico).
response(["ayudame","a","saber","que","enfermedad","tengo"], iniciar_diagnostico).


% Conversacion general
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

% Despedidas
response(["adios"], ["Adios","espero","haberte","ayudado"]).
response(["bye"], ["Bye","cuidate"]).

% Respuesta por defecto
response(_, ["No","te","entiendo","puedes","explicarme","un","poco","mas","?"]).

%pedir nombre al paciente
pedir_nombre_paciente(Nombre) :-
    writeln("Cual es tu nombre?"),
    write("> "),
    read_line_to_string(user_input, Nombre),
    retractall(paciente_actual(_)),
    assertz(paciente_actual(Nombre)).




% Diagnostico de enfermedades
iniciar_diagnostico :-
    writeln("Entiendo, voy a ayudarte a obtener un diagnostico."),
    write("Cual es tu nombre? "),
    read(Nombre),
    retractall(paciente_actual(_)),
    assertz(paciente_actual(Nombre)),
    reset_paciente(Nombre),
    format("Perfecto ~w responde solo si o no a las preguntas.~n", [Nombre]),
    diagnosticar(Nombre).


% -------- SELECCION DE RESPUESTA --------
find_response(Tokens, Response) :-
    response(Pattern, Response),
    Tokens = Pattern,
    !.
% -------- EJECUTAR RESPUESTA --------

% Caso 1: activar diagnostico medico
ejecutar_respuesta(iniciar_diagnostico) :-
    writeln("Entiendo, voy a ayudarte a obtener un diagnostico."),
    pedir_nombre_paciente(P),
    reset_paciente(P),
    format("Perfecto ~w responde solo si o no a las preguntas.~n", [P]),
    diagnosticar(P).


% Caso 2: respuesta normal (lista de palabras)
ejecutar_respuesta(Lista) :-
    is_list(Lista),
    atomic_list_concat(Lista, ' ', Texto),
    writeln(Texto).

% -------- RESPONDER --------
respond(Tokens) :-
    find_response(Tokens, Resp),
    ejecutar_respuesta(Resp).

% -------- LOOP PRINCIPAL --------
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
        ( Tokens = ["adios"] ; Tokens = ["bye"] ) ->
            true
        ;
            loop
    ).

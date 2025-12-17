% ==========================================================
% HECHOS: Enfermedades y Síntomas
% ==========================================================

tiene_sintoma(gripe, fiebre).
tiene_sintoma(gripe, dolor_cabeza).
tiene_sintoma(gripe, congestion).

tiene_sintoma(alergia, estornudos).
tiene_sintoma(alergia, picazon_ojos).
tiene_sintoma(alergia, congestion).

tiene_sintoma(migrana, dolor_cabeza_severo).
tiene_sintoma(migrana, sensibilidad_luz).
tiene_sintoma(migrana, nauseas).

tiene_sintoma(resfriado, estornudos).
tiene_sintoma(resfriado, congestion).
tiene_sintoma(resfriado, dolor_garganta).

% DIFTERIA
tiene_sintoma(difteria, dolor_garganta).
tiene_sintoma(difteria, fiebre).
tiene_sintoma(difteria, dificultad_respirar).

% ESCOLIOSIS
tiene_sintoma(escoliosis, dolor_espalda).
tiene_sintoma(escoliosis, hombros_desiguales).
tiene_sintoma(escoliosis, mala_postura).

% HIPERMETROPIA
tiene_sintoma(hipermetropia, vision_borrosa).
tiene_sintoma(hipermetropia, dolor_ojos).
tiene_sintoma(hipermetropia, dolor_cabeza).


% ==========================================================
% HECHOS: Tratamientos
% ==========================================================

tratamiento(gripe, 'Reposo, hidratacion, paracetamol y aislamiento.').
tratamiento(alergia, 'Antihistaminicos y evitar el alergeno conocido.').
tratamiento(migrana, 'Medicacion especifica, ambiente oscuro y tranquilo.').
tratamiento(resfriado, 'Liquidos calientes, descongestionantes y vitamina C.').
tratamiento(difteria,'Antibioticos, reposo, aislamiento y atencion medica inmediata.').
tratamiento(escoliosis,'Ejercicios posturales, fisioterapia y en casos severos cirugia.').
tratamiento(hipermetropia,'Uso de lentes correctivos y revision oftalmologica periodica.').
% ==========================================================
% SINTOMA EXCLUSIVO
% ==========================================================

sintoma_exclusivo(Enfermedad, Sintoma) :-
    tiene_sintoma(Enfermedad, Sintoma),
    \+ (
        tiene_sintoma(OtraEnfermedad, Sintoma),
        OtraEnfermedad \= Enfermedad
    ).

% ==========================================================
% PREDICADO DINÁMICO
% ==========================================================

:- dynamic sintoma/2.

reset_paciente(P) :- retractall(sintoma(P,_)).

% Sistema de interacción
validar_respuesta(Paciente, Sintoma, "si") :-
    assertz(sintoma(Paciente, Sintoma)).

validar_respuesta(_, _, "no") :-
    true.

validar_respuesta(Paciente, Sintoma, _) :-
    writeln("Por favor responde solo con si o no."),
    pregunta(Paciente, Sintoma).

pregunta(Paciente, Sintoma) :-
    sintoma(Paciente, Sintoma), !.   

pregunta(Paciente, Sintoma) :-
    format('El paciente ~w tiene ~w? (si/no): ', [Paciente, Sintoma]),
    read_line_to_string(user_input, Resp),
    string_lower(Resp, R),
    ( R = "si" ->
        assertz(sintoma(Paciente, Sintoma)),
        ( sintoma_exclusivo(Enfermedad, Sintoma) ->
            writeln('=========================='),
            format('Diagnostico por sintoma exclusivo! El paciente tiene: ~w~n', [Enfermedad]),
            tratamiento(Enfermedad, T),
            format('Tratamiento recomendado: ~w~n', [T]),
            writeln('=========================='),
            !, fail  
        ;
            true
        )
    ; R = "no" ->
        true
    ;
        writeln('Por favor responde solo con si o no.'),
        pregunta(Paciente, Sintoma)  
    ).



 
diagnosticar(P) :-
    ( diagnostico_basico(P, E) ->
        severidad(P, E, Sev),
        tratamiento(E, T),
        nl,
        writeln("=== RESULTADO ==="),
        format("Posible enfermedad: ~w~n", [E]),
        format("Severidad: ~w~n", [Sev]),
        format("Tratamiento: ~w~n", [T])
    ;
        writeln("No tengo suficiente informacion para un diagnostico.")
    ).


% ==========================================================
% DIAGNÓSTICO BÁSICO
% ==========================================================

diagnostico_basico(Paciente, Enfermedad) :-
    forall(
        tiene_sintoma(Enfermedad, S),
        pregunta(Paciente, S)
    ).


% ==========================================================
% DIAGNÓSTICO COMPLETO
% ==========================================================

diagnostico_completo(Paciente, Enfermedad) :-
    findall(S, tiene_sintoma(Enfermedad, S), Lista),
    todos_confirmados(Paciente, Lista).

todos_confirmados(_, []).
todos_confirmados(Paciente, [S|R]) :-
    pregunta(Paciente, S),
    todos_confirmados(Paciente, R).

% ==========================================================
% DIAGNOSTICO POR SINTOMA EXCLUSIVO
% ==========================================================



% ==========================================================
% DISTINCIÓN FUERTE Y TRATAMIENTOS
% ==========================================================

distincion_fuerte(P, gripe) :-
    diagnostico_basico(P, gripe),
    pregunta(P, fiebre),
    \+ pregunta(P, estornudos).

distincion_fuerte(P, resfriado) :-
    diagnostico_basico(P, resfriado),
    pregunta(P, estornudos),
    \+ pregunta(P, fiebre).

obtener_tratamiento(P, Trat) :-
    (distincion_fuerte(P, E) ; diagnostico_basico(P, E)),
    tratamiento(E, Trat).

% ==========================================================
% SEVERIDAD
% ==========================================================

contar_sintomas_confirmados(P, Enfermedad, C) :-
    findall(S, (tiene_sintoma(Enfermedad,S), sintoma(P,S)), L),
    length(L, C).

severidad(P, E, 'Severa') :-
    contar_sintomas_confirmados(P, E, C), C >= 3, !.

severidad(P, E, 'Moderada') :-
    contar_sintomas_confirmados(P, E, C), C = 2, !.

severidad(P, E, 'Leve') :-
    contar_sintomas_confirmados(P, E, C), C = 1, !.
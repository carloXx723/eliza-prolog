% ==========================================================
% HECHOS: Enfermedades y Síntomas
% ==========================================================

tiene_sintoma(gripe, fiebre).
tiene_sintoma(gripe, dolor_cabeza).
tiene_sintoma(gripe, congestion).

tiene_sintoma(alergia, estornudos).
tiene_sintoma(alergia, picazon_ojos).
tiene_sintoma(alergia, congestion).

tiene_sintoma(migrana, dolor_cabeza).
tiene_sintoma(migrana, sensibilidad_luz).
tiene_sintoma(migrana, nauseas).

tiene_sintoma(resfriado, estornudos).
tiene_sintoma(resfriado, congestion).
tiene_sintoma(resfriado, dolor_garganta).

tiene_sintoma(difteria, dificultad_respirar).
tiene_sintoma(difteria, dolor_garganta).
tiene_sintoma(difteria, fiebre).

tiene_sintoma(escoliosis, hombros_desiguales).
tiene_sintoma(escoliosis, dolor_espalda).
tiene_sintoma(escoliosis, mala_postura).

tiene_sintoma(hipermetropia, vision_borrosa).
tiene_sintoma(hipermetropia, dolor_ojos).
tiene_sintoma(hipermetropia, dolor_cabeza).

riesgo_enfermedad(gripe, bajo).
riesgo_enfermedad(alergia, bajo).
riesgo_enfermedad(migrana, medio).
riesgo_enfermedad(resfriado, bajo).
riesgo_enfermedad(difteria, alto).
riesgo_enfermedad(escoliosis, medio).
riesgo_enfermedad(hipermetropia, bajo).

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
% PREDICADO DINÁMICO
% ==========================================================

:- dynamic sintoma/2.

reset_paciente(P) :- retractall(sintoma(P,_)).

preguntar_todos_los_sintomas(Paciente) :-
    tiene_sintoma(_, Sintoma),
    \+ sintoma(Paciente, Sintoma),
    pregunta(Paciente, Sintoma),
    fail.
preguntar_todos_los_sintomas(_).

recomendacion(_Paciente, Enfermedad, Texto) :-
    riesgo_enfermedad(Enfermedad, NivelRiesgo),
    generar_recomendacion(NivelRiesgo, Enfermedad, Texto).

generar_recomendacion(alto, Enfermedad, Texto) :-
    atomic_list_concat(['La enfermedad', Enfermedad,
                        'tiene riesgo alto, requiere atención medica inmediata. '], ' ', Texto).

generar_recomendacion(medio, Enfermedad, Texto) :-
    atomic_list_concat(['La enfermedad', Enfermedad,
                        'tiene riesgo medio, consulta a tu medico pronto. '], ' ', Texto).

generar_recomendacion(bajo, Enfermedad, Texto) :-
    atomic_list_concat(['La enfermedad', Enfermedad,
                        'tiene riesgo bajo, sigue los cuidados habituales.'], ' ', Texto).

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

% ==========================================================
% Diagnóstico por síntoma exclusivo
% ==========================================================

sintoma_exclusivo(Enfermedad, Sintoma) :-
    tiene_sintoma(Enfermedad, Sintoma),
    \+ (
        tiene_sintoma(OtraEnfermedad, Sintoma),
        OtraEnfermedad \= Enfermedad
    ).

% ==========================================================
% Diagnóstico basado en la enfermedad con mayor probabilidad
% ==========================================================

diagnostico_mas_probable(Paciente) :-
    reset_paciente(Paciente),
    preguntar_todos_los_sintomas(Paciente),

    findall(S, sintoma(Paciente, S), SintomasConfirmados),
    format('Sintomas confirmados de ~w: ~w~n~n', [Paciente, SintomasConfirmados]),

    findall(E, tiene_sintoma(E,_), EnfermedadesDup),
    sort(EnfermedadesDup, Enfermedades),

    findall([Porcentaje, Enfermedad],
            (member(E, Enfermedades),
             probabilidad(Paciente, E, Porcentaje),
             Porcentaje > 0,
             Enfermedad = E),
            ProbList),

    writeln('Probabilidades por enfermedad:'),
    forall(member([P,E], ProbList),
           format('- ~w -> ~2f%%~n', [E,P])),
    nl,

    ( ProbList \= [] ->
        sort(0, @>=, ProbList, [[MaxPorc, EnfermedadMax]|_]),
        format('=== DIAGNOSTICO FINAL ===~n'),
        format('El paciente ~w probablemente tiene: ~w (~2f%% de probabilidad)~n', [Paciente, EnfermedadMax, MaxPorc]),

        tratamiento(EnfermedadMax, Tratamiento),
        format('Tratamiento recomendado: ~w~n', [Tratamiento]),

        severidad(Paciente, EnfermedadMax, NivelSeveridad),
        format('Severidad : ~w~n', [NivelSeveridad]),

        recomendacion(Paciente, EnfermedadMax, TextoRecomendacion),
        format('Recomendacion: ~w~n', [TextoRecomendacion]),

        writeln('=========================')
    ;
        writeln('No se detectaron síntomas que permitan un diagnóstico.')
    ).

% ==========================================================
% Probabilidad
% ==========================================================

probabilidad(Paciente, Enfermedad, Porcentaje) :-
    findall(S, tiene_sintoma(Enfermedad, S), Todos),
    length(Todos, Total),
    findall(S, (tiene_sintoma(Enfermedad, S), sintoma(Paciente, S)), Confirmados),
    length(Confirmados, C),
    ( Total > 0 -> Porcentaje is (C / Total) * 100 ; Porcentaje = 0 ).

% ==========================================================
% DIAGNÓSTICO BÁSICO
% ==========================================================

diagnostico_basico(Paciente, Enfermedad) :-
    tiene_sintoma(Enfermedad, S),
    pregunta(Paciente, S).

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
% DIAGNÓSTICAR
% ==========================================================

diagnosticar(Paciente) :-
    diagnostico_completo(Paciente, Enfermedad),
    format("El paciente ~w probablemente tiene ~w.~n", [Paciente, Enfermedad]),
    obtener_tratamiento(Paciente, Tratamiento),
    format("Tratamiento recomendado: ~w~n", [Tratamiento]).

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

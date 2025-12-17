% ==========================================================
% HECHOS: Enfermedades y Síntomas
% ==========================================================

%SINTOMAS EXCLUSIVOS
tiene_sintoma(alergia, picazon_ojos).
tiene_sintoma(migrana, sensibilidad_luz).
tiene_sintoma(difteria, dificultad_respirar).
tiene_sintoma(escoliosis, hombros_desiguales).
tiene_sintoma(hipermetropia, vision_borrosa).
tiene_sintoma(hipermetropia, dolor_ojos).

tiene_sintoma(gripe, fiebre).
tiene_sintoma(gripe, dolor_cabeza).
tiene_sintoma(gripe, congestion).

tiene_sintoma(alergia, estornudos).
tiene_sintoma(alergia, congestion).

tiene_sintoma(migrana, dolor_cabeza).
tiene_sintoma(migrana, nauseas).

tiene_sintoma(resfriado, estornudos).
tiene_sintoma(resfriado, congestion).
tiene_sintoma(resfriado, dolor_garganta).
tiene_sintoma(resfriado, dolor_cabeza).
tiene_sintoma(resfriado, dolor_espalda).

% DIFTERIA
tiene_sintoma(difteria, dolor_garganta).
tiene_sintoma(difteria, fiebre).

% ESCOLIOSIS
tiene_sintoma(escoliosis, dolor_espalda).
tiene_sintoma(escoliosis, mala_postura).

% HIPERMETROPIA
tiene_sintoma(hipermetropia, dolor_cabeza).

%Nivel de riesgo
riesgo_enfermedad(gripe, bajo).
riesgo_enfermedad(alergia, bajo).
riesgo_enfermedad(migrana, medio).
riesgo_enfermedad(resfriado, bajo).
riesgo_enfermedad(difteria, alto).
riesgo_enfermedad(escoliosis, medio).
riesgo_enfermedad(hipermetropia, bajo).

% riesgo(+Paciente, +Enfermedad, -Nivel)
riesgo(_Paciente, Enfermedad, Nivel) :-
    riesgo_enfermedad(Enfermedad, Nivel).

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
% TRATAMIENTO COMBINADO
% ==========================================================
tratamiento_combinado(Paciente, Lista) :-
    findall(Trat,
            (diagnostico_final(Paciente, Enfermedad),
             tratamiento(Enfermedad, Trat)),
            Lista).

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



 preguntar_todos_los_sintomas(Paciente) :-
    tiene_sintoma(_, Sintoma),
    \+ sintoma(Paciente, Sintoma),  % solo preguntar si no está confirmado
    pregunta(Paciente, Sintoma),
    fail.
preguntar_todos_los_sintomas(_).

% Diagnostico final: enfermedad con más síntomas confirmados
diagnostico_final(Paciente, Enfermedad) :-
    findall(E-P, (tiene_sintoma(E,_), probabilidad(Paciente,E,P)), Lista),
    Lista \= [],
    keysort(Lista, Sorted),
    reverse(Sorted, [Enfermedad-_|_]).

% Mostrar probabilidades de todas las enfermedades
mostrar_probabilidades(Paciente) :-
    findall(E, tiene_sintoma(E,_), Enfermedades),
    sort(Enfermedades, Unicas), % eliminar duplicados
    forall(
        member(E, Unicas),
        (
            probabilidad(Paciente, E, P),
            P > 0,  % solo mostrar si hay al menos un síntoma confirmado
            format('- ~w: ~2f%% de probabilidad~n', [E, P])
        )
    ).

% Diagnosticar al paciente y mostrar todo el reporte
diagnosticar(Paciente) :-
    writeln('================ DIAGNOSTICO ================='),
    % 1. Preguntar todos los síntomas
    preguntar_todos_los_sintomas(Paciente),
    % 2. Síntomas confirmados
    findall(S, sintoma(Paciente, S), SintomasConfirmados),
    ( SintomasConfirmados \= [] ->
        format('Sintomas confirmados: ~w~n', [SintomasConfirmados])
    ;
        writeln('No se confirmaron sintomas aun.')
    ),
    nl,
    % 3. Probabilidades por enfermedad
    writeln('Enfermedades posibles y probabilidad:'),
    mostrar_probabilidades(Paciente),
    nl,
    % 4. Diagnóstico final
    ( diagnostico_final(Paciente, DiagnosticoFinal) ->
        format('Diagnóstico final: ~w~n', [DiagnosticoFinal]),
        % Severidad
        severidad(Paciente, DiagnosticoFinal, Sev),
        format('Severidad: ~w~n', [Sev]),
        % Tratamiento
        tratamiento(DiagnosticoFinal, Trat),
        format('Tratamiento: ~w~n', [Trat]),
        % Recomendación por riesgo
        recomendacion(Paciente, DiagnosticoFinal, Recomendacion),
        format('Recomendación: ~w~n', [Recomendacion])
    ;
        writeln('No se pudo obtener un diagnóstico final.')
    ),
    writeln('==============================================='), nl.
% ==========================================================
% DIAGNÓSTICO BÁSICO
% ==========================================================

diagnostico_final(Paciente, Enfermedad) :-
    % Luego seleccionamos enfermedad con más síntomas confirmados
    findall(E-P, (tiene_sintoma(E,_), probabilidad(Paciente,E,P)), Lista),
    Lista \= [],
    keysort(Lista, Sorted),
    reverse(Sorted, [Enfermedad-_|_]).



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
% DIAGNOSTICO POR PROBABILIDAD 
% ==========================================================


probabilidad(Paciente, Enfermedad, Porcentaje) :-
    findall(S, tiene_sintoma(Enfermedad, S), SintomasTotales),
    length(SintomasTotales, Total),
    
    findall(S, (tiene_sintoma(Enfermedad, S), sintoma(Paciente, S)), Confirmados),
    length(Confirmados, NumConfirmados),
    
    (Total > 0 ->
        Porcentaje is (NumConfirmados / Total) * 100
    ;
        Porcentaje = 0
    ).

mostrar_probabilidades(Paciente) :-
    findall(E, tiene_sintoma(E,_), TodasEnfermedades),
    sort(TodasEnfermedades, EnfermedadesUnicas),
    forall(
        member(E, EnfermedadesUnicas),
        (
            probabilidad(Paciente, E, P),
            P > 0,
            format('~w: ~2f%% de probabilidad~n', [E, P])
        )
    ).



% ==========================================================
% DIAGNOSTICO PREVENTIVO 
% ==========================================================

diagnostico_preventivo(Paciente, Enfermedad) :-
    findall(S, tiene_sintoma(Enfermedad, S), SintomasTotales),
    length(SintomasTotales, Total),
    
    findall(S, (tiene_sintoma(Enfermedad, S), sintoma(Paciente, S)), Confirmados),
    length(Confirmados, NumConfirmados),
    
    NumConfirmados > 0,
    NumConfirmados < Total.

% ==========================================================
% DIAGNOSTICAR Y TRATAR EN UN SOLO PASO
% ==========================================================

% diagnosticar_y_tratar(+Paciente, -Diagnostico, -Tratamiento)
diagnosticar_y_tratar(Paciente, Diagnostico, Tratamiento) :-
    reset_paciente(Paciente),                  % Limpiamos síntomas previos
    diagnostico_final(Paciente, Diagnostico),% Diagnóstico basado en síntomas
    tratamiento(Diagnostico, Tratamiento).    % Obtener tratamiento correspondiente


% ==========================================================
% ENFERMEDADES SIMILARES 
% ==========================================================
enfermedades_similares(E1, E2) :-
    E1 \= E2,  
    findall(S, (tiene_sintoma(E1, S), tiene_sintoma(E2, S)), SintomasComunes),
    length(SintomasComunes, N),
    N >= 2.

% ==========================================================
% SINTOMAS CONTRADICTORIOS 
% ==========================================================

sintomas_contradictorios(Paciente) :-
    sintoma(Paciente, S1),
    sintoma(Paciente, S2),
    S1 \= S2,
    (contradictorio(S1, S2); contradictorio(S2, S1)),
    !,  
    format("Atencion: El paciente ~w tiene sintomas contradictorios: ~w y ~w~n", [Paciente, S1, S2]).

% ==========================================================
% RECOMENDACIONES POR RIESGO
% ==========================================================
% recomendacion(+Paciente, +Enfermedad, -Texto)
recomendacion(_Paciente, Enfermedad, Texto) :-
    riesgo_enfermedad(Enfermedad, NivelRiesgo),
    tratamiento(Enfermedad, Tratamiento),
    generar_recomendacion(NivelRiesgo, Enfermedad, Tratamiento, Texto).

% Genera texto según nivel de riesgo
generar_recomendacion(alto, Enfermedad, Tratamiento, Texto) :-
    atomic_list_concat(['La enfermedad', Enfermedad,
                        'tiene riesgo alto, requiere atención médica inmediata. Puedes seguir:', Tratamiento], ' ', Texto).

generar_recomendacion(medio, Enfermedad, Tratamiento, Texto) :-
    atomic_list_concat(['La enfermedad', Enfermedad,
                        'tiene riesgo medio, consulta a tu médico pronto. Puedes seguir:', Tratamiento], ' ', Texto).

generar_recomendacion(bajo, Enfermedad, Tratamiento, Texto) :-
    atomic_list_concat(['La enfermedad', Enfermedad,
                        'tiene riesgo bajo, sigue los cuidados habituales. Puedes seguir:', Tratamiento], ' ', Texto).



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
    (distincion_fuerte(P, E) ; diagnostico_final(P, E)),
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

% ==========================================================
% REPORTE COMPLETO DEL PACIENTE
% ==========================================================


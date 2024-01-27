fichas([0,0], 1).
fichas([1,0], 2).
fichas([1,1], 3).
fichas([2,0], 4).
fichas([2,1], 5).
fichas([2,2], 6).
fichas([3,0], 7).
fichas([3,1], 8).
fichas([3,2], 9).
fichas([3,3], 10).
fichas([4,0], 11).
fichas([4,1], 12).
fichas([4,2], 13).
fichas([4,3], 14).
fichas([4,4], 15).

triangulo(-99, [[1], [2,3], [4,5,6], [7,8,9,10], [11,12,13,14,15]]).
triangulo(1, [[0], [1,1], [1,1,1], [1,1,1,1], [1,1,1,1,1]]).
triangulo(2, [[1], [0,1], [1,1,1], [1,1,1,1], [1,1,1,1,1]]).
triangulo(3, [[1], [1,0], [1,1,1], [1,1,1,1], [1,1,1,1,1]]).
triangulo(4, [[1], [1,1], [0,1,1], [1,1,1,1], [1,1,1,1,1]]).
triangulo(5, [[1], [1,1], [1,0,1], [1,1,1,1], [1,1,1,1,1]]).
triangulo(6, [[1], [1,1], [1,1,0], [1,1,1,1], [1,1,1,1,1]]).
triangulo(7, [[1], [1,1], [1,1,1], [0,1,1,1], [1,1,1,1,1]]).
triangulo(8, [[1], [1,1], [1,1,1], [1,0,1,1], [1,1,1,1,1]]).
triangulo(9, [[1], [1,1], [1,1,1], [1,1,0,1], [1,1,1,1,1]]).
triangulo(10, [[1], [1,1], [1,1,1], [1,1,1,0], [1,1,1,1,1]]).
triangulo(11, [[1], [1,1], [1,1,1], [1,1,1,1], [0,1,1,1,1]]).
triangulo(12, [[1], [1,1], [1,1,1], [1,1,1,1], [1,0,1,1,1]]).
triangulo(13, [[1], [1,1], [1,1,1], [1,1,1,1], [1,1,0,1,1]]).
triangulo(14, [[1], [1,1], [1,1,1], [1,1,1,1], [1,1,1,0,1]]).
triangulo(15, [[1], [1,1], [1,1,1], [1,1,1,1], [1,1,1,1,0]]).

intercambiar_posicion(J, Lista1, Ficha1, Lista2, Ficha2) :-
    (var(J) ->
        intercambia_posicion_desconocida(0, J, Lista1, Ficha1, Lista2, Ficha2)
    ;
        intercambia_posicion_conocida(J, Lista1, Ficha1, Lista2, Ficha2)
    ).

intercambia_posicion_conocida(0, [Ficha1|R], Ficha1, [Ficha2|R], Ficha2) :- !.
intercambia_posicion_conocida(J, [F|R1], Ficha1, [F|R2], Ficha2) :-
    J1 is J-1,
    intercambia_posicion_conocida(J1, R1, Ficha1, R2, Ficha2).

intercambia_posicion_desconocida(J, J, [Ficha1|R], Ficha1, [Ficha2|R], Ficha2).
intercambia_posicion_desconocida(A, J, [F|R1], Ficha1, [F|R2], Ficha2) :-
    A1 is A+1,
    intercambia_posicion_desconocida(A1, J, R1, Ficha1, R2, Ficha2).

cambiar_tablero((I,J), TableroInicial, Ficha1, TableroResultante, Ficha2) :-
    intercambiar_posicion(I, TableroInicial, Fila1, TableroResultante, Fila2),
    intercambiar_posicion(J, Fila1, Ficha1, Fila2, Ficha2).

validar_adyacencia((I,J1), (I,J2), 0) :- succ(J1,J2).
validar_adyacencia((I1,J), (I2,J), 1) :- succ(I2,I1).
validar_adyacencia((I1,J1), (I2,J2), 2) :- succ(I2,I1), succ(J2,J1).
validar_adyacencia((I,J1), (I,J2), 3) :- succ(J2,J1).
validar_adyacencia((I1,J), (I2,J), 4) :- succ(I1,I2).
validar_adyacencia((I1,J1), (I2,J2), 5) :- succ(I1,I2), succ(J1,J2).

realizar_movimiento([Inicio,Fin], TableroInicial, TableroResultante) :-
    cambiar_tablero(Inicio, TableroInicial, 1, TableroA, 0),  
    validar_adyacencia(Inicio, Medio, Direccion),     
    validar_adyacencia(Medio, Fin, Direccion),      
    cambiar_tablero(Medio, TableroA, 1, TableroB, 0),  
    cambiar_tablero(Fin, TableroB, 0, TableroResultante, 1).  

resolver(X) :-
    triangulo(X, TableroInicial),
    length(MovimientosCoordenadas, 13),
    scanl(realizar_movimiento, MovimientosCoordenadas, TableroInicial, TablerosResultantes),
    take(13, TablerosResultantes, TablerosSeleccionados),
    maplist(imprimir_tablero_con_movimiento, TablerosSeleccionados, MovimientosCoordenadas),
    imprimir_tablero_final(TablerosResultantes),
    !.

resolver_desde_config_actual(X, N) :-
    N1 is 13-N,
    length(MovimientosCoordenadas, N1),
    scanl(realizar_movimiento, MovimientosCoordenadas, X, TablerosResultantes),
    take(N1, TablerosResultantes, TablerosSeleccionados),
    maplist(imprimir_tablero_con_movimiento, TablerosSeleccionados, MovimientosCoordenadas),
    imprimir_tablero_final(TablerosResultantes),
    !.

take(0, _, []).
take(N, [X|XS], [X|YS]) :- N > 0, N1 is N - 1, take(N1, XS, YS).

imprimir_tablero_con_movimiento(T, Movimiento) :-
    format("    ~w~n   ~w~n  ~w~n ~w~n~w~n", T),
    nth0(0, Movimiento, (I1, J1)),
    nth0(1, Movimiento, (I2, J2)),
    fichas([I1, J1], H1),
    fichas([I2, J2], H2),
    format("Se mueve la ficha ~w hacia ficha ~w. ~n", [H1, H2]).

imprimir_tablero_final(T) :- last(T, T1), format("    ~w~n   ~w~n  ~w~n ~w~n~w~n", T1).

imprimir_triangulo(T) :- format("    ~w~n   ~w~n  ~w~n ~w~n~w~n", T).

jugar(X) :-
    writeln('Bienvenido al Triangle Quest!'),
    triangulo(-99,K),
    imprimir_triangulo(K),
    writeln('-----------------------------------'),
    writeln('Ejemplo de movimiento "(1,3)."'),
    writeln('moveria la ficha 1 hacia la ficha 3'),
    writeln('-----------------------------------'),
    writeln('El tablero inicial es el siguiente:'),
    triangulo(X, T),
    imprimir_triangulo(T),
    jugar_interactivo(X, T, 0).

jugar_interactivo(X, T, N) :-
    writeln('Introduce el movimiento a realizar "(inicio, fin)." o escribe "rendirse." para rendirte'),
    read(Movimiento),
    (Movimiento = rendirse ->
        writeln('Te rendiste. Intentando resolver el juego desde el estado actual...'),
        (resolver_desde_config_actual(T, N) ->
            writeln('Fin del juego.')
        ;
            writeln('Solucion imposible.')
        )
    ;
        procesar_movimiento(Movimiento, M),
        (realizar_movimiento(M, T, T2) ->
            imprimir_tablero_con_movimiento(T2, M),
            N1 is N + 1,
            (quedan_solo_una_ficha(T2) ->
                writeln('Ganaste.')
            ;
                jugar_interactivo(X, T2, N1)
            )
        ;
            writeln('Movimiento no valido, el que persevera vence, vuelve a intentar.'),
            jugar_interactivo(X, T, N)
        )
    ).

procesar_movimiento(Entrada, Movimiento) :-
    Entrada = (Ficha1,Ficha2),
    fichas([I1, J1], Ficha1),
    fichas([I2, J2], Ficha2),
    Movimiento = [(I1, J1), (I2,J2)].

quedan_solo_una_ficha(T) :-
    contar_fichas(T, C),
    C =:= 1.

contar_fichas([], 0).
contar_fichas([Fila | Resto], Suma) :-
    sum_list(Fila, ACum),
    contar_fichas(Resto, RestoSuma),
    Suma is ACum + RestoSuma.
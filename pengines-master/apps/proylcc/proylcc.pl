:- module(proylcc, 
		[  
                emptyBoard/1,
                goMove/4
        ]).

% checked/1 sirve para marcar cuando recorro
:- dynamic checked/1.

% encerradoActual/1 sirve para marcar los encerrados para luego eliminarlos
:- dynamic encerradoActual/1.

% fueOcupado/1 sirve para marcar los lugares donde alguna vez se coloco una piedra
% y evitar que si ahora esta vacio se vuelva a poner una.
:- dynamic fueOcupado/1.


emptyBoard([["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
             ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% goMove(+Board, +Player, +Pos, -RBoard)
%
% RBoard es la configuración resultante de reflejar la movida del jugador Player
% en la posición Pos a partir de la configuración Board.

goMove(Board, Player, [R,C], RBoard):-
    replace(Row, R, NRow, Board, AuxBoard),
    replace("-", C, Player, Row, NRow),
    checkEncerradoPlayer(AuxBoard, [R,C], Player, RBoard),
    !,
    esValida(RBoard, Player, [R,C]),
    assert(fueOcupado([R,C])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > -1,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).
    

%
%	esValida(Board, Player, Index)
%	Chequea si que player ponga una piedra en index es valido (no es suicidio)
%
esValida(Board, "w", Index):- 
    not(estaEncerrado(Board, "b", "w", Index)),
    not(fueOcupado(Index)).
esValida(Board, "b", Index):- 
    not(estaEncerrado(Board, "w", "b", Index)),
    not(fueOcupado(Index)).

%
%   checkEncerradoPlayer(Board, Player, NBoard)
%  Averigua quien es opponent y llama a checkEncerrado
%  
checkEncerradoPlayer(Board, Index, "w", NBoard):-checkEncerrado(Board, Index, "w", "b", NBoard).
checkEncerradoPlayer(Board, Index, "b", NBoard):-checkEncerrado(Board, Index, "b", "w", NBoard).


%
%   checkEncerrado(Board, Index, Player, Opponent, NBoard)
%   Chequea si los que vecinos del index estan encerrados.
%   Devuelve el nuevo Board actualizado en NBoard
%   

checkEncerrado(Board, [R,C], Player, Opponent, NBoard):-
    RArriba is R-1,
    IndexVecinoArriba = [RArriba, C] ,
    RAbajo is R+1,
    IndexVecinoAbajo = [RAbajo, C] ,
    CIzq is C-1,
    IndexVecinoIzquierdo = [R, CIzq],
    CDer is C+1,
    IndexVecinoDerecho = [R, CDer],
    checkEncerradoCascara(Board, Player, Opponent, IndexVecinoArriba),
    eliminarEncerradosActuales(Board, NBoard1),
    checkEncerradoCascara(Board, Player, Opponent, IndexVecinoAbajo),
	eliminarEncerradosActuales(NBoard1, NBoard2),
    checkEncerradoCascara(Board, Player, Opponent, IndexVecinoIzquierdo),
	eliminarEncerradosActuales(NBoard2, NBoard3),
    checkEncerradoCascara(Board, Player, Opponent, IndexVecinoDerecho),
    eliminarEncerradosActuales(NBoard3, NBoard).

%
%   checkEncerradoCascara(Board, Player, Opponent, NBoard, Index)
%   Chequea si en el Board hay algun Opponent encerrado por Player
%   Se usa para avanzar en el index grande
%
checkEncerradoCascara(_Board,_Player,_Opponent,[R,C]):-R<0;R>18;C<0;C>18.

checkEncerradoCascara(Board, Player, Opponent, [R,C]):-
    estaEncerrado(Board, Player, Opponent, [R,C]),
    retractall(checked(_)).

    
checkEncerradoCascara(Board, Player, Opponent, [R,C]):-
    noEstaEncerrado(Board,Player,Opponent, [R,C]),
    retractall(checked(_)).

%
%     estaEncerrado(Board,Player, Opponent, Index)
%     true si en [R,C] hay un Opponent encerrado por Player
%   

estaEncerrado(Board, Player, Opponent, [R,C]):-
    getValueOnBoard(Board, [R,C], Value),
    Value \= Player,
    RArriba is R-1,
    IndexVecinoArriba = [RArriba, C] ,
    RAbajo is R+1,
    IndexVecinoAbajo = [RAbajo, C] ,
    CIzq is C-1,
    IndexVecinoIzquierdo = [R, CIzq],
    CDer is C+1,
    IndexVecinoDerecho = [R, CDer],
    IndexVecinos = [IndexVecinoArriba, IndexVecinoAbajo, IndexVecinoIzquierdo, IndexVecinoDerecho],
    getValueListOnBoard(Board, IndexVecinos, ValuesVecinos),
    not(member("-", ValuesVecinos)),
    assert(checked([R,C])),
    vecinoEstaEncerrado(Board, Player, Opponent, ValuesVecinos, IndexVecinos),
    assert(encerradoActual([R,C])).

%
%   vecinoEstaEncerrado(Board, Player, Opponent, ValuesVecinos, IndexVecinos).
%   chequea si los vecinos estan encerrados.
%

vecinoEstaEncerrado(_,_,_,[],[]).

vecinoEstaEncerrado(Board, Player, Opponent, [Player|Vs], [_|Is]):-
    vecinoEstaEncerrado(Board, Player, Opponent, Vs, Is).

vecinoEstaEncerrado(Board, Player, Opponent, ["o"|Vs], [_|Is]):-
    vecinoEstaEncerrado(Board, Player, Opponent, Vs, Is).

vecinoEstaEncerrado(Board, Player, Opponent, [Opponent|Vs], [I|Is]):-
    checked(I),
    vecinoEstaEncerrado(Board, Player, Opponent, Vs, Is).

vecinoEstaEncerrado(Board, Player, Opponent, [Opponent|Vs], [I|Is]):-
    not(checked(I)),
    estaEncerrado(Board, Player, Opponent, I),
    vecinoEstaEncerrado(Board, Player, Opponent, Vs, Is).

%
%     noEstaEncerrado(Board,Player, Opponent, Index):-
%     true si en index no hay un Opponent encerrado por Player
%
noEstaEncerrado(Board, Player, Opponent, Index):-
	retractall(checked(_)),
    not(estaEncerrado(Board, Player, Opponent, Index)).
    

%
%   getValueListOnBoard(Board, Indexes, Values)
%   Obtiene los valores que corresponden a las listas de Indexes en el Board
%
getValueListOnBoard(_Board, [], []).
getValueListOnBoard(Board, [I|Is], [V|Vs]):-
    getValueOnBoard(Board, I, V),
    getValueListOnBoard(Board,Is,Vs).
 
    
%
%   getValueOnBoard(Board, Index, Value)
%   Obtiene el valor del Index en el Board
%
getValueOnBoard(Board, [R,C], Value):-
    getListaIndex(R, Board, Lista),
    getValueLista(C, Lista, Value).
    
%
%   getValueLista(Columna, Lista, Value)
%   Obtiene el valor de la Columna en la Lista.
%   Si la lista esta fuera del tablero, devuelve o.
%
getValueLista(_Columna, [], "o").
getValueLista(C, _, "o"):- C < 0 ; C > 18.
getValueLista(Columna, Lista, Value):-nth0(Columna, Lista, Value).
    
%
%   getListaIndex(Index, Board, Lista)
%   Obtiene la lista del index indicado del Board, la devuelve en Lista
%

getListaIndex(Index,_,[]):-Index<0;Index>18.
getListaIndex(Index, Board, Lista):-
    nth0(Index, Board, Lista).

%
%	eliminarEncerradosActuales(Board, NBoard)
%	Elimina todos los marcados como encerrados actuales y devuelve el nuevo board
%
eliminarEncerradosActuales(Board,Board):-
    not(encerradoActual(_)).

eliminarEncerradosActuales( Board, NBoard):-
    encerradoActual([R,C]),
    replace(Row, R, NRow, Board,AuxBoard),
    replace(_, C, "-", Row, NRow),
    retract(encerradoActual([R,C])),
    eliminarEncerradosActuales(AuxBoard, NBoard).
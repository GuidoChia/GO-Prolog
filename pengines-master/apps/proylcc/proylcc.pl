:- module(proylcc, 
		[  
                emptyBoard/1,
                goMove/4
        ]).

:- dynamic checked/1.
:- dynamic encerradoActual/1.

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
    checkEncerradoPlayer(AuxBoard, Player, RBoard).


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
%   checkEncerradoPlayer(Board, Player, NBoard)
%  Averigua quien es opponent y llama a checkEncerrado
%  
checkEncerradoPlayer(Board, "w", NBoard):-checkEncerrado(Board, "w", "b",NBoard).
checkEncerradoPlayer(Board, "b", NBoard):-checkEncerrado(Board, "b", "w",NBoard).


%
%   checkEncerrado(Board, Player, Opponent, NBoard)
%   Chequea si en el Board hay algun Opponent encerrado por Player
%   Devuelve el nuevo Board actualizado en NBoard

checkEncerrado(Board, Player, Opponent, NBoard):-
    Index = [0,0],
    checkEncerradoCascara(Board, Player, Opponent, Index),
    eliminarEncerradosActuales(Board, NBoard).

%
%   checkEncerradoCascara(Board, Player, Opponent, NBoard, Index)
%   Chequea si en el Board hay algun Opponent encerrado por Player
%   Se usa para avanzar en el index grande
%

checkEncerradoCascara(_Board, _Player, _Opponent, [19,_C]):-retractall(checked(_)).

checkEncerradoCascara(Board, Player, Opponent, [R,C]):-
    estaEncerrado(Board, Player, Opponent, [R,C]),
    assert(encerradoActual([R,C])),
    ((C < 19,NewC is C+1, NewR = R) ; (C=19, NewC = 0, NewR is R+1)),
    retractall(checked(_)),
    checkEncerradoCascara(Board, Player, Opponent, [NewR,NewC]).

    
checkEncerradoCascara(Board, Player, Opponent, [R,C]):-
    noEstaEncerrado(Board,Player,Opponent, [R,C]),
    ((C < 19,NewC is C+1, NewR = R) ; (C=19, NewC = 0, NewR is R+1)),
    retractall(checked(_)),
    checkEncerradoCascara(Board, Player, Opponent, [NewR,NewC]).

%
%     estaEncerrado(Board,Player, Opponent, Index)
%     true si en index hay un Opponent encerrado por Player
%

estaEncerrado(Board,Player, Opponent, [R,C]):-
    RArriba is R-1,
    IndexVecinoArriba = [RArriba, C] ,
    RAbajo is R+1,
    IndexVecinoAbajo = [RAbajo, C] ,
    CIzq is C-1,
    IndexVecinoIzquierdo = [R, CIzq],
    CDer is C+1,
    IndexVecinoDerecho = [R, CDer],
    IndexVecinos = [IndexVecinoArriba,IndexVecinoAbajo,IndexVecinoIzquierdo,IndexVecinoDerecho],
    getValueListOnBoard(Board, IndexVecinos, ValuesVecinos),
    estaEncerradoPorVecinos(Player, Opponent, ValuesVecinos, IndexVecinos).

estaEncerrado(Board,Player, Opponent, [R,C]):-
    RArriba is R-1,
    IndexVecinoArriba = [RArriba, C] ,
    RAbajo is R+1,
    IndexVecinoAbajo = [RAbajo, C] ,
    CIzq is C-1,
    IndexVecinoIzquierdo = [R, CIzq],
    CDer is C+1,
    IndexVecinoDerecho = [R, CDer],
    IndexVecinos = [IndexVecinoArriba,IndexVecinoAbajo,IndexVecinoIzquierdo,IndexVecinoDerecho],
    getValueListOnBoard(Board, IndexVecinos, ValuesVecinos),
    assert(checked([R,C])),
    not(member("-", ValuesVecinos)),
    member(Opponent, ValuesVecinos),
    vecinoEstaEncerrado(Board, Player, Opponent, ValuesVecinos, IndexVecinos).

%
%   vecinoEstaEncerrado(Board, Player, Opponent, ValuesVecinos, IndexVecinos).
%   chequea si los vecinos estan encerrados.
%

vecinoEstaEncerrado(_,_,_,[],[]).

vecinoEstaEncerrado(Board, Player, Opponent, [Opponent|Vs], [I|Is]):-
    not(checked(I)),
    estaEncerrado(Board, Player, Opponent, I),
    vecinoEstaEncerrado(Board, Player, Opponent, Vs, Is).

vecinoEstaEncerrado(Board, Player, Opponent, [Player|Vs], [_|Is]):-
    vecinoEstaEncerrado(Board, Player, Opponent, Vs, Is).

vecinoEstaEncerrado(Board, Player, Opponent, [Opponent|Vs], [I|Is]):-
    checked(I),
    vecinoEstaEncerrado(Board, Player, Opponent, Vs, Is).


%
%    estaEncerradoPorVecinos(Player, Opponent, ValuesVecinos, IndexVecinos).
%    Chequea si todos los vecinos son enemigos o son posiciones fuera del tablero
%

estaEncerradoPorVecinos(_,_,[],[]).
estaEncerradoPorVecinos(Player, Opponent, [V|Vs], [_I|Is]):-
    V = Player,
    estaEncerradoPorVecinos(Player, Opponent, Vs, Is).

estaEncerradoPorVecinos(Player, Opponent, [V|Vs], [_I|Is]):-
    V = "o",
    estaEncerradoPorVecinos(Player, Opponent, Vs, Is).

%
%     noEstaEncerrado(Board,Player, Opponent, Index):-
%     true si en index no hay un Opponent encerrado por Player
%
noEstaEncerrado(Board, Player, Opponent, Index):-
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
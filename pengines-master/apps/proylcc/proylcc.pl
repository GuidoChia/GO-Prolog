:- module(proylcc, 
		[  
                emptyBoard/1,
                goMove/4
        ]).
%
%  checked(Player, Index)
%  Se utiliza para chequear que Index ha sido "visitado" al buscar lugares encerrados
%  por Player.
% 
:- dynamic checked/2.

%
% 	encerradoActual(Player, Index)
% 	Se utiliza para marcar que Index ha sido encerrado por Player. 
%   
:- dynamic encerradoActual/2.

% 
% 	noEncerrado(Index) 
% 	Se utilza para indicar que un lugar no esta encerrado (sirve para hacer mas eficiente el chequeo de puntos)
% 	Únicamente se usa en el conte de puntos, ya que se tiene la certeza de que el tablero no cambiará.
% 
:- dynamic noEncerrado/1.



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
% 	goMove(+Board, +Player, +Pos, -RBoard)
%
% 	RBoard es la configuración resultante de reflejar la movida del jugador Player
% 	en la posición Pos a partir de la configuración Board.
% 	Luego de colocar una ficha de Player en Pos, se busca si algún adyacente ha sido 
% 	encerrado. De ser así, son eliminados, devolviendo el tablero en RBoard.
% 	Dado que se utiliza un predicado dinámico checked para marcar los visitados en el
% 	proceso de chequeo de encierro, esto da lugar a posibles soluciones alternativas.
% 	Por lo tanto, se utiliza el cut (!), para no permitirlas.
% 	Luego se chequea si la posición donde se ha colocado la ficha no es sucidio.
% 
% 

goMove(Board, Player, [R,C], RBoard):-
    replace(Row, R, NRow, Board, AuxBoard),
    replace("-", C, Player, Row, NRow),
    buscarEliminarVecinosEncerrados(AuxBoard, [R,C], Player, RBoard),
    esValida(RBoard, Player, [R,C]).

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
%	esValida(+Board, +Player, +Index)
%	Chequea si que player ponga una piedra en index es valido (no es suicidio)
%	Se retractan los checkeados correspondientemente.
%
esValida(Board, "w", Index):- 
    not(estaEncerrado(Board, "b", "w", "-", Index)),
    retractall(checked(_,_)).
esValida(Board, "b", Index):- 
    not(estaEncerrado(Board, "w", "b", "-", Index)),
    retractall(checked(_,_)).

%
%   buscarEliminarVecinosEncerrrados(+Board, +Index, +Player, -NBoard)
%  	Dado el index, busca sus adyacentes, y marca los encerrados.
%  	Luego elimina aquellos que fueron marcados como encerrados. 
%  
buscarEliminarVecinosEncerrados(Board, Index, "w", NBoard):-
    getVecinos(Index, IndexesVecinos),
    checkEncerrado(Board, IndexesVecinos, "w", "b", "-"),
    eliminarEncerradosActuales(Board, NBoard).

buscarEliminarVecinosEncerrados(Board, Index, "b", NBoard):-
    getVecinos(Index, IndexesVecinos),
    checkEncerrado(Board, IndexesVecinos, "b", "w", "-"),
    eliminarEncerradosActuales(Board, NBoard).


%
%   checkEncerrado(+Board, +Index, +Player, +Opponent, +Liberty, -NBoard)
%   Chequea si los que vecinos del index estan encerrados.
%   Liberty representa donde paro de chequear si estan encerrados
%   Devuelve el nuevo Board actualizado en NBoard
%   
checkEncerrado(_, [], _, _, _).
checkEncerrado(Board, [X|Xs], Player, Opponent, Liberty):-
    checkEncerradoCascara(Board, Player, Opponent, Liberty, X),
    checkEncerrado(Board, Xs, Player, Opponent, Liberty).

%
%   checkEncerradoCascara(+Board, +Player, +Opponent, +Liberty, -NBoard, +Index)
%   Chequea si en el Board hay algun Opponent encerrado por Player
%   Si al buscar se encuentra un liberty, automaticamente no esta encerrado
%   Si esta encerrado, cambio los chequeados por encerrados. 
%   Si no esta encerrado,  borro las marcas de chequeado.
%
checkEncerradoCascara( _Board, _Player, _Opponent, _Liberty, [R,C]):-R<0;R>18;C<0;C>18.

checkEncerradoCascara(Board, Player, Opponent, Liberty, [R,C]):-
    estaEncerrado(Board, Player, Opponent, Liberty, [R,C]),
    checkedToEncerrado.
    
checkEncerradoCascara(Board, Player, Opponent, Liberty, [R,C]):-
    noEstaEncerrado(Board, Player, Opponent, Liberty, [R,C]),
    retractall(checked(_,_)).

%
%     estaEncerrado(+Board, +Player, +Opponent, +Liberty, +Index)
%     True si en [R,C] hay un Opponent encerrado por Player
%     Empieza chequeando si el Index ya fue marcado como encerrado.
%     Si no fue así, recursivamente visita sus vecinos para chequear si esta encerrado.
%   

estaEncerrado(_, Player, _, _, [R,C]):- encerradoActual(Player, [R,C]).
estaEncerrado(Board, Player, Opponent, Liberty, [R,C]):-
    getValueOnBoard(Board, [R,C], Value),
    Value = Opponent,
    getVecinos([R,C], IndexVecinos),
    getValueListOnBoard(Board, IndexVecinos, ValuesVecinos),
    not(member(Liberty, ValuesVecinos)),
    assert(checked(Player, [R,C])),
    vecinoEstaEncerrado(Board, Player, Opponent, Liberty, ValuesVecinos, IndexVecinos).

%
%	getVecinos(Index, IndexesVecinos)
%	Obtiene los index de los vecinos adyacentes de Index, y los devuelve en la lista IndexesVecinos 
%
getVecinos([R,C], [IndexVecinoArriba, IndexVecinoAbajo, IndexVecinoIzquierdo, IndexVecinoDerecho]):-
    RArriba is R-1,
    IndexVecinoArriba = [RArriba, C] ,
    RAbajo is R+1,
    IndexVecinoAbajo = [RAbajo, C] ,
    CIzq is C-1,
    IndexVecinoIzquierdo = [R, CIzq],
    CDer is C+1,
    IndexVecinoDerecho = [R, CDer].

%
%   vecinoEstaEncerrado(+Board, +Player, +Opponent, +Liberty, +ValuesVecinos, +IndexVecinos).
%   Chequea recursivamente si los vecinos estan encerrados.
%   
%   Se utiliza el cut para evitar caminos alternativos al encontrar que un indice fue visitado.
%   Dado que el metodo usado para marcar visitados (checked/2) es dinámico, puede cambiar
%   dinámicamente, produciendo así soluciones alternativas no deseadas.
%

vecinoEstaEncerrado(_,_,_,_,[],[]).

vecinoEstaEncerrado(Board, Player, Opponent, Liberty, [Player|Vs], [_|Is]):-
    vecinoEstaEncerrado(Board, Player, Opponent, Liberty, Vs, Is).

vecinoEstaEncerrado(Board, Player, Opponent, Liberty, ["o"|Vs], [_|Is]):-
    vecinoEstaEncerrado(Board, Player, Opponent, Liberty, Vs, Is).

vecinoEstaEncerrado(Board, Player, Opponent, Liberty, [Opponent|Vs], [I|Is]):-
    checked(_,I),
    vecinoEstaEncerrado(Board, Player, Opponent, Liberty, Vs, Is),
	!.

vecinoEstaEncerrado(Board, Player, Opponent, Liberty, [Opponent|Vs], [I|Is]):-
    not(checked(_,I)),
    estaEncerrado(Board, Player, Opponent, Liberty, I),
    vecinoEstaEncerrado(Board, Player, Opponent, Liberty, Vs, Is).

%
%     noEstaEncerrado(+Board, +Player, +Opponent, +Index):-
%     True si en index no hay un Opponent encerrado por Player
%
noEstaEncerrado(Board, Player, Opponent, Liberty, Index):-
	retractall(checked(_,_)),
    not(estaEncerrado(Board, Player, Opponent, Liberty, Index)).
    

%
%   getValueListOnBoard(+Board, +Indexes, -Values)
%   Obtiene los valores que corresponden a la lista de Indexes en el Board
%   Los devuelve en una lista Values
%
getValueListOnBoard(_Board, [], []).
getValueListOnBoard(Board, [I|Is], [V|Vs]):-
    getValueOnBoard(Board, I, V),
    getValueListOnBoard(Board,Is,Vs).
 
    
%
%   getValueOnBoard(+Board, +Index, -Value)
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
%   getListaIndex(+Index, +Board, -Lista)
%   Obtiene la lista del index indicado del Board, la devuelve en Lista.
%   En caso de que el Index este fuera del tablero, devuelvo la lista vacía.
%

getListaIndex(Index,_,[]):-Index<0;Index>18.
getListaIndex(Index, Board, Lista):-
    nth0(Index, Board, Lista).

%
%	eliminarEncerradosActuales(+Board, -NBoard)
%	Elimina todos los marcados como encerrados actuales y devuelve el nuevo board.
%
eliminarEncerradosActuales(Board,Board):-
    not(encerradoActual(_,_)).

eliminarEncerradosActuales( Board, NBoard):-
    encerradoActual(_,[R,C]),
    replace(Row, R, NRow, Board,AuxBoard),
    replace(_, C, "-", Row, NRow),
    retract(encerradoActual(_,[R,C])),
    eliminarEncerradosActuales(AuxBoard, NBoard).

%
%	winPoints(+Board, -PWhite, -PBlack)
%	Calcula los puntos de cada equipo cuando se gana
%	Se calcula de la siguiente manera: Lugares en el tablero ocupados por cada color,
%	sumado a la cantidad de espacios vacios capturados por un color.
%	
%	
%
winPoints(Board, 0, 0):-
    emptyBoard(Board).

winPoints(Board, PWhite, PBlack):-
    calcularPuntos(Board, "w", "-", "b", PWhite),
    calcularPuntos(Board, "b", "-", "w", PBlack),
	retractall(encerradoActual(_,_)),
	retractall(noEncerrado(_)).
    
%
%	calcularPuntos(+Board, +Player, +Opponent, +Liberty, -P)
%	Calcula los puntos de Player, buscando donde encierra Opponents, y donde ocupa lugares
%

calcularPuntos(Board, Player, Opponent, Liberty, P):-
    IndexInicial = [0,0],
    calcularPuntosAux(Board,Player,Opponent, Liberty, IndexInicial),
    findall(I, encerradoActual(Player, I), LAux),
    length(LAux, P).

%
%	calcularPuntosAux(Board, Player, Opponent, Liberty, Index)
%  	Calcula los puntos de Player, buscando donde encierra Opponents, y donde ocupa lugares
%  	Se usa para recorrer todo el tablero recursivamente
%

calcularPuntosAux(_Board,_Player,_Opponent, _Liberty, [19,_]).

calcularPuntosAux(Board, Player, Opponent, Liberty, Index):-
    (encerradoActual(_,Index);noEncerrado(Index)),
    getNext(Index, Next),
    calcularPuntosAux(Board, Player, Opponent, Liberty, Next).

calcularPuntosAux(Board, Player, Opponent, Liberty, Index):-
    not(encerradoActual(_,Index)),
    not(noEncerrado(Index)),
    getValueOnBoard(Board, Index, V),
    V = Player,
    assert(encerradoActual(Player, Index)),
    getNext(Index, Next),
    calcularPuntosAux(Board, Player, Opponent, Liberty, Next).

calcularPuntosAux(Board, Player, Opponent, Liberty, Index):-
    not(encerradoActual(_,Index)),
    not(noEncerrado(Index)),
    getValueOnBoard(Board, Index, V),
    V = Liberty,
    getNext(Index, Next),
    calcularPuntosAux(Board, Player, Opponent, Liberty, Next).
    

calcularPuntosAux(Board, Player, Opponent, Liberty, Index):-
    not(encerradoActual(_,Index)),
    not(noEncerrado(Index)),
    getValueOnBoard(Board, Index, V),
    V = Opponent,
    checkEncerradoSinUncheck(Board, Player, Opponent, Liberty, Index),
    getNext(Index, Next),
    calcularPuntosAux(Board, Player, Opponent, Liberty, Next).

%
%	checkEncerradoSinUncheck(+Board, +Player, +Opponent, +Liberty, +Index)
%   Chequea si en el Board hay algun Opponent encerrado por Player
%   Si al buscar se encuentra un liberty, automaticamente no esta encerrado
%   Si esta encerrado, marca los chequeados como encerrados. 
%   Si no esta encerrado, marca los chequeados como no encerrados, ya que el tablero no cambiará.
%   
checkEncerradoSinUncheck(Board, Player, Opponent, Liberty, [R,C]):-
    estaEncerrado(Board, Player, Opponent, Liberty, [R,C]),
    checkedToEncerrado.
    
checkEncerradoSinUncheck(Board, Player, Opponent, Liberty, [R,C]):-
    noEstaEncerrado(Board, Player, Opponent, Liberty, [R,C]),
    checkedToNoEncerrado.

%
%	getNext(+Index, -NextIndex)
%	Obtiene el index siguiente al dado.
%	El siguiente será el vecino derecho, 
%	o bien el primero de la fila siguiente, si el index dado es de la última columna.
%

getNext([R,18], [NR,0]):-
    NR is R+1.

getNext([R,C], [R,NC]):-
    C<18,
    NC is C+1.
    
%
%	checkedToEncerrado
%	Transforma a aquellos marcados como chequeados en encerrados.
%	Termina cuando no hay más marcados como chequeados.
%

checkedToEncerrado:-not(checked(_,_)).
checkedToEncerrado:-
    checked(P, I),
    assert(encerradoActual(P, I)),
   	retract(checked(P,I)),
    checkedToEncerrado.

%
%	checkedToNoEncerrado
%	Transforma a aquellos marcados como chequeados en no encerrados.
%	Termina cuando no hay más marcados como chequeados.
%
checkedToNoEncerrado:-not(checked(_,_)).
checkedToNoEncerrado:-
    checked(P,I),
    assert(noEncerrado(I)),
    retract(checked(P,I)),
    checkedToNoEncerrado.

% ****************************************************************
% Inicializa��o do servidor.
% ****************************************************************


:-use_module(library(sockets)).

port(60001).

server:-
		port(Port),
		socket_server_open(Port,Socket),
		write('Breakthrough server started on port ['), write(Port), write(']'), nl,
		write('Waiting for connection...'), nl,
		socket_server_accept(Socket, _Client, Stream, [type(text)]),
		write('Connection received'), nl,
		server_loop(Stream),
		socket_server_close(Socket),
		write('Server Exit'),nl.

server_loop(Stream) :-
		repeat,
			read(Stream, ClientRequest),
			write('Received: '), write(ClientRequest), nl, 
			server_input(ClientRequest, ServerReply),
			format(Stream, '~q.~n', [ServerReply]),
			write('Send: '), write(ServerReply), nl, 
			flush_output(Stream),
		(ClientRequest == bye; ClientRequest == end_of_file), !.

% Initialize board
server_input(initialize(Side), ok(Board)):- 
		initDynBoard(Side, Board), !.

% Execute Move	
server_input(execute(Board, Ox-Oy, Dx-Dy), ok(NewBoard)):- 
		movePawn(Board, Ox, Oy, Dx, Dy, NewBoard), !.

% Calculate next Move
server_input(calculate(Board, Player), ok(Ox-Oy, Dx-Dy, NewBoard)):- 
		length(Board, Side),
		pickNextMove(Board, Side, Player, Ox, Oy, Dx, Dy ),
		movePawn(Board, Ox, Oy, Dx, Dy, NewBoard), !.
	
% Check Winner
server_input(game_end(Board), ok(Winner)):- 
		isWinner(Board, Winner), !.
	
	
server_input(bye, ok):-!.
server_input(end_of_file, ok):-!.
server_input(_, invalid) :- !.



% ****************************************************************
% ****************************************************************
% Jogo
% ****************************************************************
% ****************************************************************

:-use_module(library(random)).


% ****************************************************************
% ****************************************************************

% Imprime a pe�a do jogador correspondente.
writePlayer(1) :-
		write(1).
writePlayer(2) :-
		write('2').
writePlayer(0) :-
		write(' ').


% ****************************************************************
% Imprime o n�mero da coluna ou da linha, com um espa�o � esqueda
% ou � direita, de forma ao tabuleiro ser disposto correctamente
% com linhas ou colounas de n�mero superior a 9.
% printSpaceNumberL(+N)
% printSpaceNumberR(+N)
% ****************************************************************

printSpaceNumberL(N) :-
		N < 10,
		write(N), write(' ').
		
printSpaceNumberL(N) :-
		write(N).
                
printSpaceNumberR(N) :-
		N < 10,
		write(' '), write(N).

printSpaceNumberR(N) :-
		write(N).

% ****************************************************************
% printRow(+[H|T])
% ****************************************************************

% Crit�rio de paragem.
% Imprime tamb�m a borda direita da �ltima c�lula de cada linha.
printRow([]).

% Imprime a borda esquerda da c�lula, e a respectivo
% pe�a do jogador (cabe�a da lista).
% Por fim chama recursivamente a mesma fun��o com
% a cauda da lista, at� encontrar o crit�rio de paragem.
printRow([H|T]) :-
		write('| '),
		writePlayer(H),
		write(' '),
		printRow(T).


% ****************************************************************
% ****************************************************************

% Imprime a linha de separa��o horizontal.
printHorizSepp(S,S) :- !.

printHorizSepp(S, N) :-
		N1 is N+1,
		printHorizSepp(S, N1),
		write('--- ').


printHorizSep(S)   :-
		write('    '),
		printHorizSepp(S, 0).

% Imprime os n�meros das colunas.
%printColNumbers([], _).

printColNumbers([_], N) :-
		printSpaceNumberR(N),
		!.

printColNumbers([_|T], N) :-
		N1 is N+1,
		printSpaceNumberR(N),
		write('  '),
		printColNumbers(T, N1).

printColNumbers([H|T]) :-
		write('    '),
		printColNumbers([H|T], 1).


% ****************************************************************
% ****************************************************************

% Crit�rio de paragem.
printFullRow([], _, _).


% Imprime o n�mero da linha no �nicio e no fim de cada itera��o,
% bem como o separador horizontal. Chama tamb�m o predicado que ir�
% imprimir todas as c�lulas de cada linha, com a respectiva pe�a.
% Por fim chama recursivamente a mesma fun��o com a cauda da
% lista (contendo as restantes c�lulas da linha correspondente),
% e o n�mero actual da linha, at� atingir o crit�rio de paragem,
% ou seja, quando a a cauda for uma lista vazia.
printFullRow([H|T], N, S) :-
		N1 is N+1,
		printHorizSep(S),
		nl,
		printSpaceNumberR(N),
		write(' '),
		printRow(H),
		write('| '),
		printSpaceNumberL(N),
		nl,
		printFullRow(T, N1, S).


% ****************************************************************
% ****************************************************************

% Regra a ser usada quando � passado um tabuleiro vazio.
printBoard([]).

% Imprime o tabuleiro, chamando o predicado que imprime o n�mero
% das colunas (no in�cio e no fim do tabuleiro),
% e de seguida o predicado que imprime cada linha individual.
% � imprimido tamb�m o separador horizontal do tabuleiro.
printBoard([H|T]) :-
		length([H|T], S),
		printColNumbers([H|T]),
		nl,
		printFullRow([H|T], 1, S),
		printHorizSep(S),
		nl,
		printColNumbers([H|T]).


% ****************************************************************
% getPawn(+Tabuleiro, +X, +Y, -Jogador)
% Retorna o n�mero do jogador na posi��o (X,Y) do Tabuleiro.
% Recorre a getPawnPos() para percorrer o tabuleiro.
% ****************************************************************

% Condi��o de paragem, encontrou a casa.
% Retorna InTabH
getPawnPos([Player|_], Dest, Dest, Player) :- !.

% Percorre o tabuleiro
getPawnPos([_|InTabT], Dest, BaseIncrement, Value) :-
		Increment is BaseIncrement + 1,
		getPawnPos(InTabT, Dest, Increment, Value).

% Verifica se X e Y est�o dentro dos valores permitidos
% Recorre a getPawnPos para percorrer o tabuleiro e encontra o jogador
getPawn(InTab, X, Y, Player) :-
		X > 0,
		Y > 0,
		length(InTab, Len),
		X =< Len,
		Y =< Len,
		getPawnPos(InTab, Y, 1, L),
		getPawnPos(L, X, 1, Player).

        
% ****************************************************************
% getll(Linha,Coluna,Tabuleiro,Valor)
% Obter o valor da casa
% ****************************************************************

% Obtem o valor de determinada posi��o N numa Lista.
getl(Lista,N,Valor) :-
		getl(Lista,N,1,Valor).

getl([H|_],N,N,H) :- !.

getl([_|R],N,Nactual,Valor) :-
		NR is Nactual+1,
		getl(R,N,NR,Valor).

% ****************************************************************
% Obtem o valor de determinada posi��o (Linha,Coluna) num Tabuleiro
% ****************************************************************
getll(Linha,Coluna,Tabuleiro,Valor) :-
		% Obtemos primeiro uma Linha completa
		getl(Tabuleiro,Linha,Lista),
		% Obtemos o valor da coluna na Linha obtida
		getl(Lista,Coluna,Valor).

                 
                 
% ****************************************************************
% substll(Tabuleiro,Linha, Coluna, Valor, Resultado)
% Substitui a pe�a de uma Casa por outra
% ****************************************************************

substl(Lista,N,Valor,Resultado) :-
        substl(Lista,N,1,Valor,[],Resultado).

substl([],_,_,_,NovaLista,NovaLista).

substl([_|R],N,N,Valor,NovaLista,Resultado) :- !,
		append(NovaLista,[Valor],NovaNovaLista),
		NR is N+1,
		substl(R,N,NR,Valor,NovaNovaLista,Resultado).

substl([H|R],N,Nactual,Valor,NovaLista,Resultado) :-
		append(NovaLista,[H],NovaNovaLista),
		NR is Nactual+1,
		substl(R,N,NR,Valor,NovaNovaLista,Resultado).

substll(Tabuleiro,Linha, Coluna, Valor, Resultado) :-
		getl(Tabuleiro,Linha,Lista),
		substl(Lista,Coluna,Valor,NovaLista),
		substl(Tabuleiro,Linha,NovaLista,Resultado).

% ****************************************************************
% Verifica��o de um vencedor
% ****************************************************************

checkForInvasion([]).
checkForInvasionOfP1([]).

% Verificar se h� pe�as do jogador 1 na �ltima linha
% (�ltimo predicado a ser executado)
checkForInvasionOfP1([InTabT], Player) :-
		member(1, InTabT),
		Player is 1,
		!.

% Corrre resto do tabuleiro recursivamente.
checkForInvasionOfP1([_|InTabT], Player) :-
		checkForInvasionOfP1(InTabT, Player).

% Verificar se h� pe�as do jogador 2 na primeira linha
% (Primeiro predicado a ser executado).
checkForInvasion([InTabH|_], Player) :-
		member(2, InTabH),
		Player is 2,
		!.

% Se n�o encontrar pe�as do jogador 2, corre
% recursivamente o resto do tabuleiro at� chegar � ultima linha.
checkForInvasion([_|InTabT], Player) :-
		checkForInvasionOfP1(InTabT, Player).

% Verifica se existe pelo menos 1 pe�a do jogador P
hasPieces([], _).
hasPieces([H|T], P) :-
		\+(member(P, H)),
		hasPieces(T, P).


% Predicado principal
% O jogador � vencedor se 1 de 2 casos acontecer:
% 1. tiver chegado � base advers�ria
% 2. o advers�rio n�o tiver pe�as.
isWinner(Tab, 1) :-
		checkForInvasion(Tab, 1);
		hasPieces(Tab, 2).

isWinner(Tab, 2) :-
		checkForInvasion(Tab, 2);
		hasPieces(Tab, 1).

% ****************************************************************
% Valida��o de uma jogada
% ****************************************************************

% checkMove(+Tab, +Ox, +Oy, +Dx, +Dy, +P)
% Valida movimentos exclusivamente verticais

% JOGADOR 1
% Movimento vertical
checkMove(Tab, Ox, Oy, Ox, Dy, 1) :-
		!,
		Dy =:= Oy+1,
		getPawn(Tab, Ox, Dy, P),
		P = 0.

% Movimento diagonal
checkMove(Tab, Ox, Oy, Dx, Dy, 1) :-
        Dy =:= Oy+1,
		Dx =:= Ox+1,
		getPawn(Tab, Dx, Dy, P),
		P \= 1.

checkMove(Tab, Ox, Oy, Dx, Dy, 1) :-
        Dy =:= Oy+1,
		Dx =:= Ox-1,
		getPawn(Tab, Dx, Dy, P),
		P \= 1.

% JOGADOR 2
% Movimento vertical
checkMove(Tab, Ox, Oy, Ox, Dy, 2) :-
		!,
		Dy =:= Oy-1,
		getPawn(Tab, Ox, Dy, P),
		P = 0.

% Movimento diagonal
checkMove(Tab, Ox, Oy, Dx, Dy, 2) :-
		Dy =:= Oy-1,
		Dx =:= Ox+1,
		getPawn(Tab, Dx, Dy, P),
		P \= 2.

checkMove(Tab, Ox, Oy, Dx, Dy, 2) :-
		Dy =:= Oy-1,
		Dx =:= Ox-1,
		getPawn(Tab, Dx, Dy, P),
		P \= 2.


% ****************************************************************
% Move player
% ****************************************************************

switchPlayer(1,2).
switchPlayer(2,1).

movePawn(Tab, Ox, Oy, Dx, Dy, TabOut) :-
		getPawn(Tab, Ox, Oy, Me),
		checkMove(Tab, Ox, Oy, Dx, Dy, Me),
		getPawn(Tab, Dx, Dy, Opponent),
		Opponent = 0,
		substll(Tab, Dy, Dx, Me, TabTmpOut),
		substll(TabTmpOut, Oy, Ox, 0, TabOut).

movePawn(Tab, Ox, Oy, Dx, Dy, TabOut) :-
		getPawn(Tab, Ox, Oy, Me),
		checkMove(Tab, Ox, Oy, Dx, Dy, Me),
		getPawn(Tab, Dx, Dy, Opponent),
		Opponent \= 0,
		substll(Tab, Dy, Dx, Me, TabTmpOut),
		substll(TabTmpOut, Oy, Ox, 0, TabOut).

%movePawn(Tab, _, _, _, _, Tab).




% ****************************************************************
% Inicializa dinamicamente do o tabuleiro
% initDynBoard(+Side, -Board)
% ****************************************************************

initDynBoard_line(BaseNumber, BaseNumber, _, Line, Line).

initDynBoard_line(BaseNumber, Side, V, LineIn, LineOut) :-
		N is BaseNumber + 1,
		initDynBoard_line(N, Side, V, [V|LineIn], LineOut).
		
                
initDynBoard_col(BaseNumber, BaseNumber, Board, Board).

initDynBoard_col(BaseNumber, Side, BoardIn, BoardOut) :-
		L is BaseNumber + 1,
		(
				( L =:= 1 ; L =:= 2 ) ->
						X is 2 ;
						( ( L =:= Side ; L =:= Side - 1 ) ->
								X is 1 ;
								X is 0
						)
		),
		initDynBoard_line( 0, Side, X, [], Y ),
		initDynBoard_col(L, Side, [Y|BoardIn], BoardOut).

initDynBoard(Side, Board) :-
		Side > 4,
		Side < 20,
		initDynBoard_col(0, Side, [], Board).
        
        
% ****************************************************************
% ****************************************************************      

checkPoss_prior(X, X, _, _, _, _, 0):- !.
checkPoss_prior(_, _, 1, _, 2, 0, 3):- !.
checkPoss_prior(_, _, 1, _, 2, 1, 4):- !.
checkPoss_prior(_, _, S, S, 1, 0, 3):- !.
checkPoss_prior(_, _, S, S, 1, 2, 4):- !.
checkPoss_prior(_, _, _, _, 1, 2, 2):- !.
checkPoss_prior(_, _, _, _, 2, 1, 2):- !.
checkPoss_prior(_, _, _, _, _, 0, 1):- !.
checkPoss_prior(_, _, _, _, _, _, 0):- !.


checkPoss(Board, Side, Player, Ox, Dx, Dy, ListIn, ListOut) :-
		Dx > 0,
		Dx =< Side,
		Dy > 0,
		Dy =< Side,
		getPawn(Board, Dx, Dy, Pawn),
		checkPoss_prior(Ox, Dx, Dy, Side, Player, Pawn, Prior),
		append( ListIn, [Prior], ListOut ).
		
checkPoss(_, _, _, _, _, _, ListIn, ListOut) :-
		append( ListIn, [0], ListOut ).
		

% ****************************************************************
% ****************************************************************   
		
getPossiblePlays_int(Board, Side, Player, Ox, Dy, Plays) :-
		Dx1 is Ox-1,
		Dx2 is Ox+1,
		checkPoss(Board, Side, Player, Ox, Dx1, Dy, [], Lista1),
		checkPoss(Board, Side, Player, Ox, Ox , Dy, Lista1, Lista2),
		checkPoss(Board, Side, Player, Ox, Dx2, Dy, Lista2, Plays).

		
		
% getPossiblePlays/7		

getPossiblePlays(Board, Side, 1, Ox, Oy, Dy, Plays) :-
		Dy is Oy + 1,
		getPossiblePlays_int(Board, Side, 1, Ox, Dy, Plays).
		
getPossiblePlays(Board, Side, 2, Ox, Oy, Dy, Plays) :-
		Dy is Oy - 1,
		getPossiblePlays_int(Board, Side, 2, Ox, Dy, Plays).
		
getPossiblePlays(_, _, _, _, _, _, [0, 0, 0]).
		

% getPossiblePlays/5
getPossiblePlays(Board, Side, X, Y, Plays) :-
		getPawn(Board, X, Y, Pawn),
		getPossiblePlays(Board, Side, Pawn, X, Y, _, Plays).
		

% ****************************************************************
% ****************************************************************

getMoves([], _, _, _, _, Moves, Moves).

getMoves([P|Tail], Inc,  Ox,Oy, Dy,  MovesIn, MovesOut) :-
		P > 0,
		Inc2 is Inc + 1,
		NewX is Ox + Inc - 1,
		getMoves(Tail, Inc2, Ox,Oy, Dy, [P-Ox-Oy-NewX-Dy|MovesIn], MovesOut).			

getMoves([_|Tail], Inc, Ox,Oy, Dy, MovesIn, MovesOut) :-
		Inc2 is Inc + 1,
		getMoves(Tail, Inc2, Ox,Oy, Dy, MovesIn, MovesOut).
		
		
% ****************************************************************
% ****************************************************************
	
addMoves(Board, Side, Player, Player, X, Y, MovesIn, MovesOut) :-
		getPossiblePlays(Board, Side, Player, X, Y, Y2, Plays),
		getMoves(Plays, 0, X, Y, Y2, MovesIn, MovesOut).

addMoves(_, _, _, _, _, _, Moves, Moves).		


% ****************************************************************
% ****************************************************************

checkNextMoves_line([], _, _, _, _, _, Moves, Moves).
checkNextMoves_line([Cell|LineTail], Board, Side, X, Y, Player, MovesIn, MovesOut) :-
		X1 is X + 1,
		addMoves(Board, Side, Cell, Player, X, Y, MovesIn, MovesOutTemp),
		checkNextMoves_line(LineTail, Board, Side, X1, Y, Player, MovesOutTemp, MovesOut).
	

% checkNextMoves/7
checkNextMoves([], _, _, _, _, Moves, Moves).
checkNextMoves([Line|BoardTail], Board, Side, Y, Player, MovesIn, MovesOut) :-
		Y1 is Y + 1,
		checkNextMoves_line(Line, Board, Side, 1, Y, Player, MovesIn, MovesOutTemp),
		checkNextMoves(BoardTail, Board, Side, Y1, Player, MovesOutTemp, MovesOut).

% checkNextMoves/4
checkNextMoves(Board, Side, Player, MovesOut) :-
		checkNextMoves(Board, Board, Side, 1, Player, [], MovesOut).


% ****************************************************************
% ****************************************************************

% getMaxPriority/3

getMaxPriority([], Priority, Priority).

getMaxPriority([Prior-_-_-_-_|Tail], PriorIn, PriorOut) :-
		Prior > PriorIn,
		getMaxPriority(Tail, Prior, PriorOut).

getMaxPriority([_|Tail], PriorIn, PriorOut) :-
		getMaxPriority(Tail, PriorIn, PriorOut).

		
% getMaxPriority/2
getMaxPriority(List, Priority) :-
		getMaxPriority(List, 0, Priority).
		

% ****************************************************************
% ****************************************************************

% buildMaxPriorityList/6

buildMaxPriorityList([], _, Total, Total, List, List).

buildMaxPriorityList([Pri-Ox-Oy-Dx-Dy|MovesTail], Priority, TotalIn, TotalOut, ListIn, ListOut) :-
		Pri = Priority,
		TotalIn2 is TotalIn + 1,
		buildMaxPriorityList(MovesTail, Priority, TotalIn2, TotalOut, [Ox-Oy-Dx-Dy|ListIn], ListOut).

buildMaxPriorityList([_|MovesTail], Priority, TotalIn, TotalOut, ListIn, ListOut) :-
		buildMaxPriorityList(MovesTail, Priority, TotalIn, TotalOut, ListIn, ListOut).

% buildMaxPriorityList/4
buildMaxPriorityList(Moves, Priority, Total, ListOut) :-
		buildMaxPriorityList(Moves, Priority, 0, Total, [], ListOut).


% ****************************************************************
% ****************************************************************

% getMoveNumber/7

getMoveNumber([Ox-Oy-Dx-Dy|_], PickNumber, PickNumber, Ox, Oy, Dx, Dy).

getMoveNumber([_|MovesTail], N, PickNumber, Ox, Oy, Dx, Dy) :-
		N < PickNumber,
		N2 is N + 1,
		getMoveNumber(MovesTail, N2, PickNumber, Ox, Oy, Dx, Dy).

% getMoveNumber/6
getMoveNumber(List, PickNumber, Ox, Oy, Dx, Dy) :-
		getMoveNumber(List, 0, PickNumber, Ox, Oy, Dx, Dy).


% ****************************************************************
% ****************************************************************

pickNextMove(Board, Side, Player, Ox, Oy, Dx, Dy ) :-
		checkNextMoves(Board, Side, Player, Moves),
		getMaxPriority(Moves, Priority),
		buildMaxPriorityList(Moves, Priority, Total, List),
		random(0, Total, Random),
		getMoveNumber(List, Random, Ox, Oy, Dx, Dy).
		
		
% ****************************************************************
% ****************************************************************

checkAbort('sair') :-
		abort.
checkAbort('Sair') :-
		abort.
checkAbort('SAIR') :-
		abort.
checkAbort(_).

readIntegerAbort(Text, Value) :-
		write(Text),
		read(Input),
		checkAbort(Input),
		integer(Input),
		Value = Input.
		
readXY(X,Y) :-
		readIntegerAbort('X = ', X),
		readIntegerAbort('Y = ', Y).
		
printPlayer(Player) :-
		nl, nl,
		write('[Jogador: '),
		write(Player),
		write(']'), nl.

checkCorrectPlayer(Player, 0) :- !,
		write('Casa inv�lida, tente novamente!'), nl,
		Pawn \= 0.
		
checkCorrectPlayer(Player, Pawn) :-
		Player \= Pawn,
		!,
		write('Esse pe�o n�o � seu, tente novamente!'), nl,
		Player = Pawn.
		
checkCorrectPlayer(Pawn, Pawn).

% ****************************************************************
% ****************************************************************

verificaVencedor(Board, _, _, _) :-
		isWinner(Board, Player),
		nl,nl,
		write('Jogo terminado! Jogador ['),
		write(Player),
		write('] venceu o jogo'), nl.
	
verificaVencedor(Board, Side, GameMode, Player) :-
		printPlayer(Player),
		play(Board, Side, GameMode, Player).
	
% ****************************************************************
% ****************************************************************

transitPlay(BoardIn, Side, GameMode, Player, Ox, Oy, Dx, Dy) :-
		movePawn(BoardIn, Ox, Oy, Dx, Dy, Board),
		write('Jogador ['), write(Player), write('] moveu a pe�a ['),
		write(Ox), write(','), write(Oy), write('] para ['),
		write(Dx), write(','), write(Dy), write(']'), nl, nl,
		printBoard(Board),
		switchPlayer(Player, NextPlayer),
		verificaVencedor(Board, Side, GameMode,NextPlayer).

transitPlay(Board, Side, GameMode, 1, _, _, _, _) :- !,
		write('Jogada Inv�lida, tente outra vez!'), nl,
		play(Board, Side, GameMode, 1).


% ****************************************************************
% ****************************************************************

play(Board, Side, 2, 2) :- !,
		pickNextMove(Board, Side, 2, Ox, Oy, Dx, Dy ),
		transitPlay(Board, Side, 2, 2, Ox, Oy, Dx, Dy ).

play(Board, Side, GameMode, Player) :- !,
		repeat,
		nl,
		write('Pe�a a mover?'), nl,
		readXY( Ox, Oy ), nl,
		getPawn(Board, Ox, Oy, Pawn),
		checkCorrectPlayer(Player, Pawn),
		repeat,
		write('Casa de destino?'), nl,	
		readXY( Dx, Dy ), nl,
		transitPlay(Board, Side, GameMode, Player, Ox, Oy, Dx, Dy ).
	
play(Board, Side, GameMode) :-
		play(Board, Side, GameMode, 1).


% ****************************************************************
% ****************************************************************
	
init :-
		repeat,
		readIntegerAbort('Lado do tabuleiro [5,19] = ', Side), nl,
		Side >= 5,
		Side =< 19,
		initDynBoard(Side, Board),
		repeat,
		write('Modo de jogo:'), nl,
		write('1 - Humano/Humano'), nl,
		write('2 - Humano/Computador'), nl,
		readIntegerAbort('> ', Mode), nl,
		Mode > 0,
		Mode < 3,
		printBoard(Board),
		printPlayer(1),
		play(Board, Side, Mode).

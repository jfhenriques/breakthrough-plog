
% **********************************************************************
% **********************************************************************

% Inicializa o tabuleiro.

%midBoard([
%           [ 1, 1, 1, 1, 1, 1, 1, 1 ],
%           [ 1, 1, 1, 1, 1, 0, 1, 1 ],
%           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
%           [ 0, 0, 0, 0, 0, 1, 0, 0 ],
%           [ 0, 0, 0, 0, 2, 2, 0, 0 ],
%           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
%           [ 2, 2, 2, 2, 2, 0, 0, 2 ],
%           [ 2, 2, 2, 2, 2, 2, 2, 2 ]
%          ]).

%finalBoard([
%           [ 1, 1, 1, 1, 1, 1, 1, 1 ],
%           [ 1, 1, 1, 0, 1, 0, 1, 0 ],
%           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
%           [ 0, 0, 2, 0, 1, 1, 0, 0 ],
%           [ 0, 0, 0, 0, 0, 0, 1, 0 ],
%           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
%           [ 2, 2, 0, 2, 2, 1, 2, 0 ],
%           [ 2, 2, 2, 2, 2, 2, 2, 2 ]
%          ]).

%finalBoard2([
%           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
%           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
%           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
%           [ 0, 0, 2, 0, 0, 0, 0, 0 ],
%           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
%           [ 0, 0, 0, 0, 0, 1, 0, 0 ],
%           [ 2, 2, 0, 2, 2, 0, 2, 0 ],
%           [ 2, 2, 2, 2, 2, 2, 2, 2 ]
%          ]).

%board4x4([
%                  [ 1, 1, 1, 1],
%          [ 0, 0, 0, 0],
%          [ 0, 0, 0, 0],
%          [ 2, 2, 2, 2]
%         ]).

%board3x3([
 %             [ 1, 1, 1],
 %         [ 0, 0, 0],
 %         [ 2, 2, 2]
 %        ]).

%board8x8([
%           [ 1, 1, 1, 1, 1, 1, 1, 1 ],
%           [ 1, 1, 1, 1, 1, 1, 1, 1 ],
%           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
%           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
%           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
%           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
%           [ 2, 2, 2, 2, 2, 2, 2, 2 ],
%           [ 2, 2, 2, 2, 2, 2, 2, 2 ]
%          ]).


% **********************************************************************
% **********************************************************************

% Imprime a peça do jogador correspondente.
writePlayer(1) :-
        write(1).
writePlayer(2) :-
        write('2').
writePlayer(0) :-
        write(' ').


% **********************************************************************
% Imprime o número da coluna ou da linha, com um espaço à esqueda
% ou à direita, de forma ao tabuleiro ser disposto correctamente com
% linhas ou colounas de número superior a 9.
% printSpaceNumberL(+N)
% printSpaceNumberR(+N)
% **********************************************************************

printSpaceNumberL(N) :-
                write(N),
                (N < 10 ->
                        write(' ') ;
                        write('')
                ).
printSpaceNumberR(N) :-
                (N < 10 ->
                        write(' ') ;
                        write('')
                ),
                write(N).


% **********************************************************************
% printRow(+[H|T])
% **********************************************************************

% Critério de paragem.
% Imprime também a borda direita da última célula de cada linha.
printRow([]).

% Imprime a borda esquerda da célula, e a respectivo
% peça do jogador (cabeça da lista).
% Por fim chama recursivamente a mesma função com
% a cauda da lista, até encontrar o critério de paragem.
printRow([H|T]) :-
        write('| '),
        writePlayer(H),
        write(' '),
        printRow(T).


% **********************************************************************
% **********************************************************************

% Imprime a linha de separação horizontal.
printHorizSepp(S,S) :- !.

printHorizSepp(S, N) :-
                N1 is N+1,
                printHorizSepp(S, N1),
                write('--- ').


printHorizSep(S)   :-
                write('    '),
                printHorizSepp(S, 0).

% Imprime os números das colunas.
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


% **********************************************************************
% **********************************************************************

% Critério de paragem.
printFullRow([], _, _).


% Imprime o número da linha no ínicio e no fim de cada iteração,
% bem como o separador horizontal. Chama também o predicado que irá
% imprimir todas as células de cada linha, com a respectiva peça.
% Por fim chama recursivamente a mesma função com a cauda da
% lista (contendo as restantes células da linha correspondente),
% e o número actual da linha, até atingir o critério de paragem,
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


% **********************************************************************
% **********************************************************************

% Regra a ser usada quando é passado um tabuleiro vazio.
printBoard([]).

% Imprime o tabuleiro, chamando o predicado que imprime o número
% das colunas (no início e no fim do tabuleiro),
% e de seguida o predicado que imprime cada linha individual.
% É imprimido também o separador horizontal do tabuleiro.
printBoard([H|T]) :-
        length([H|T], S),
        printColNumbers([H|T]),
        nl,
        printFullRow([H|T], 1, S),
        printHorizSep(S),
        nl,
        printColNumbers([H|T]).


% **********************************************************************
% getPawn(+Tabuleiro, +X, +Y, -Jogador)
% Retorna o número do jogador na posição (X,Y) do Tabuleiro.
% Recorre a getPawnPos() para percorrer o tabuleiro.
% **********************************************************************

% Condição de paragem, encontrou a casa.
% Retorna InTabH
getPawnPos([Player|_], Dest, Dest, Player) :- !.

% Percorre o tabuleiro
getPawnPos([_|InTabT], Dest, BaseIncrement, Value) :-
                Increment is BaseIncrement + 1,
                getPawnPos(InTabT, Dest, Increment, Value).

% Verifica se X e Y estão dentro dos valores permitidos
% Recorre a getPawnPos para percorrer o tabuleiro e encontra o jogador
getPawn(InTab, X, Y, Player) :-
                X > 0,
                Y > 0,
				length(InTab, Len),
                X =< Len,
                Y =< Len,
                getPawnPos(InTab, Y, 1, L),
                getPawnPos(L, X, 1, Player).

        
% **********************************************************************
% getll(Linha,Coluna,Tabuleiro,Valor)
% Obter o valor da casa
% **********************************************************************

% Obtem o valor de determinada posição N numa Lista.
getl(Lista,N,Valor) :-
                getl(Lista,N,1,Valor).

getl([H|_],N,N,H) :- !.

getl([_|R],N,Nactual,Valor) :-
        NR is Nactual+1,
        getl(R,N,NR,Valor).

% **********************************************************************
% Obtem o valor de determinada posição (Linha,Coluna) num Tabuleiro
% **********************************************************************
getll(Linha,Coluna,Tabuleiro,Valor) :-
        % Obtemos primeiro uma Linha completa
        getl(Tabuleiro,Linha,Lista),
        % Obtemos o valor da coluna na Linha obtida
        getl(Lista,Coluna,Valor).

                 
                 
% **********************************************************************
% substll(Tabuleiro,Linha, Coluna, Valor, Resultado)
% Substitui a peça de uma Casa por outra
% **********************************************************************

substl(Lista,N,Valor,Resultado):-
                substl(Lista,N,1,Valor,[],Resultado).

substl([],_,_,_,NovaLista,NovaLista).

substl([_|R],N,N,Valor,NovaLista,Resultado):- !,
        append(NovaLista,[Valor],NovaNovaLista),
        NR is N+1,
        substl(R,N,NR,Valor,NovaNovaLista,Resultado).

substl([H|R],N,Nactual,Valor,NovaLista,Resultado):-
        append(NovaLista,[H],NovaNovaLista),
        NR is Nactual+1,
        substl(R,N,NR,Valor,NovaNovaLista,Resultado).

substll(Tabuleiro,Linha, Coluna, Valor, Resultado):-
        getl(Tabuleiro,Linha,Lista),
        substl(Lista,Coluna,Valor,NovaLista),
        substl(Tabuleiro,Linha,NovaLista,Resultado).

% **********************************************************************
% Verificação de um vencedor
% **********************************************************************

checkForInvasion([]).
checkForInvasionOfP1([]).

% Verificar se há peças do jogador 1 na última linha
% (Último predicado a ser executado)
checkForInvasionOfP1([InTabT], Player):-
        member(1, InTabT),
        Player is 1,
        !.

% Corrre resto do tabuleiro recursivamente.
checkForInvasionOfP1([_|InTabT], Player):-
        checkForInvasionOfP1(InTabT, Player).

% Verificar se há peças do jogador 2 na primeira linha
% (Primeiro predicado a ser executado).
checkForInvasion([InTabH|_], Player):-
        member(2, InTabH),
        Player is 2,
        !.

% Se não encontrar peças do jogador 2, corre
% recursivamente o resto do tabuleiro até chegar à ultima linha.
checkForInvasion([_|InTabT], Player):-
        checkForInvasionOfP1(InTabT, Player).

% Verifica se existe pelo menos 1 peça do jogador P
hasPieces([], _).
hasPieces([H|T], P) :-
        \+(member(P, H)),
        hasPieces(T, P).


% Predicado principal
% O jogador é vencedor se 1 de 2 casos acontecer:
% 1. tiver chegado à base adversária
% 2. o adversário não tiver peças.
isWinner(Tab, 1) :-
        checkForInvasion(Tab, 1);
        hasPieces(Tab, 2).

isWinner(Tab, 2) :-
        checkForInvasion(Tab, 2);
        hasPieces(Tab, 1).

% **********************************************************************
% Validação de uma jogada
% **********************************************************************

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


% **********************************************************************
% Move player
% **********************************************************************

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

% **********************************************************************
% Inicializa dinamicamente do o tabuleiro
% initDynBoard(+Side, -Board)
% **********************************************************************

initDynBoard_line(BaseNumber, BaseNumber, _, Line, Line).
%append(LineIn, [], LineOut).

initDynBoard_line(BaseNumber, Side, V, LineIn, LineOut) :-
		N is BaseNumber + 1,
		initDynBoard_line(N, Side, V, [V|LineIn], LineOut).
		
                
initDynBoard_col(BaseNumber, BaseNumber, Board, Board).
%append(BoardIn, [], BoardOut).

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
        
        
% **********************************************************************
% **********************************************************************        

checkPoss_prior(_, _, 1, 0, 1):- !.
checkPoss_prior(_, _, 2, 0, 1):- !.
checkPoss_prior(X, X, 1, 2, 0):- !.
checkPoss_prior(X, X, 2, 1, 0):- !.
checkPoss_prior(_, _, 1, 2, 2):- !.
checkPoss_prior(_, _, 2, 1, 2):- !.
checkPoss_prior(_, _, _, _, 0):- !.

checkPoss(Board, Side, Player, Ox, X, Y, ListIn, ListOut) :-
		( ( X > 0,
			X =< Side,
			Y > 0,
			Y =< Side,
			getPawn(Board, X, Y, Pawn)
		  )
		  -> checkPoss_prior(Ox, X, Player, Pawn, Prior)
		   ; Prior is 0
		),
		append( ListIn, [Prior], ListOut ).
		

		
getPossiblePlays_int(Board, Side, Player, X, Y, Plays) :-
		X1 is X-1,
		X2 is X+1,
		checkPoss(Board, Side, Player, X, X1, Y, [], L1),
		checkPoss(Board, Side, Player, X, X , Y, L1, L2),
		checkPoss(Board, Side, Player, X, X2, Y, L2, Plays).

		
		
% getPossiblePlays/7		

getPossiblePlays(Board, Side, 1, X, Y, Y2, Plays) :-
		Y2 is Y + 1,
		getPossiblePlays_int(Board, Side, 1, X, Y2, Plays).
		
getPossiblePlays(Board, Side, 2, X, Y, Y2, Plays) :-
		Y2 is Y - 1,
		getPossiblePlays_int(Board, Side, 2, X, Y2, Plays).
		
getPossiblePlays(_, _, _, _, _, _, [0, 0, 0]).
		

% getPossiblePlays/5
getPossiblePlays(Board, Side, X, Y, Plays) :-
		getPawn(Board, X, Y, Pawn),
		getPossiblePlays(Board, Side, Pawn, X, Y, _, Plays).
		

        
% **********************************************************************
% **********************************************************************

getFirstMaxPriorityMove([], _, Pri, Ox, Oy, Dx, Dy, Max) :-
		Pri > 0,
		Max = Pri-Ox-Oy-Dx-Dy.
		
getFirstMaxPriorityMove([P|Tail], Inc, PriIn, Ox, Oy, Dx, Dy, Max) :-
		Inc2 is Inc + 1,
		( ( P > 0, P > PriIn) ->
			( X is Ox + Inc - 1,
			  Pri is P ) ;
			( X is Dx,
			  Pri is PriIn )
		),
		getFirstMaxPriorityMove(Tail, Inc2, Pri, Ox, Oy, X, Dy, Max).	
		
		
addMove(Board, Side, Player, Player, X, Y, MovesIn, MovesOut) :-
		getPossiblePlays(Board, Side, Player, X, Y, Y2, Plays),
		getFirstMaxPriorityMove(Plays, 0, 0, X, Y, 0, Y2, Move),
		append(MovesIn, [Move], MovesOut).

addMove(_, _, _, _, _, _, Moves, Moves).		


checkNextMoves_line([], _, _, _, _, _, Moves, Moves).
checkNextMoves_line([Cell|LineTail], Board, Side, X, Y, Player, MovesIn, MovesOut) :-
		X1 is X + 1,
		addMove(Board, Side, Cell, Player, X, Y, MovesIn, MovesOutTemp),
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


% **********************************************************************
% **********************************************************************

pickNextMove([Pri-Ox-Oy-Dx-Dy|MovesTail], PriIn-_-_-_-_, OxOut, OyOut, DxOut, DyOut ) :-
		Pri > PriIn,
		pickNextMove(MovesTail, Pri-Ox-Oy-Dx-Dy, OxOut, OyOut, DxOut, DyOut ).

pickNextMove([_|MovesTail], PriIn-OxIn-OyIn-DxIn-DyIn, OxOut, OyOut, DxOut, DyOut ) :-
		pickNextMove(MovesTail, PriIn-OxIn-OyIn-DxIn-DyIn, OxOut, OyOut, DxOut, DyOut ).
		
pickNextMove([], _-Ox-Oy-Dx-Dy, Ox, Oy, Dx, Dy ) :- !.
		
pickNextMove(Board, Side, Player, Ox, Oy, Dx, Dy ) :-
		checkNextMoves(Board, Side, Player, Moves),
		pickNextMove(Moves, 0-0-0-0-0, Ox, Oy, Dx, Dy ).
		
		
% **********************************************************************
% **********************************************************************

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
	
checkCorrectPlayer(Player, Pawn) :-
		Player \= Pawn,
		write('Esse peão não é seu, tente novamente!'), nl,
		Player = Pawn.
		
checkCorrectPlayer(Pawn, Pawn).

% **********************************************************************
% **********************************************************************

verificaVencedor(Board, _, _, _) :-
		isWinner(Board, Player),
		write('Jogo terminado! Venceu o jogador ['),
		write(Player),
		write(']'), nl.
	
verificaVencedor(Board, Side, GameMode, Player) :-
		printPlayer(Player),
		play(Board, Side, GameMode, Player).
	
% **********************************************************************
% **********************************************************************

transitPlay(BoardIn, Side, GameMode, Player, Ox, Oy, Dx, Dy) :-
		movePawn(BoardIn, Ox, Oy, Dx, Dy, Board),
		write('Jogador ['), write(Player), write('] moveu a peça ['),
		write(Ox), write(','), write(Oy), write('] para ['),
		write(Dx), write(','), write(Dy), write(']'), nl, nl,
		printBoard(Board),
		switchPlayer(Player, NextPlayer),
		verificaVencedor(Board, Side, GameMode,NextPlayer).

transitPlay(Board, Side, GameMode, 1, _, _, _, _) :- !,
		write('Jogada Inválida, tente outra vez!'), nl,
		play(Board, Side, GameMode, 1).


% **********************************************************************
% **********************************************************************

play(Board, Side, 2, 2) :- !,
		pickNextMove(Board, Side, 2, Ox, Oy, Dx, Dy ),
		transitPlay(Board, Side, 2, 2, Ox, Oy, Dx, Dy ).

play(Board, Side, GameMode, Player) :- !,
		repeat,
		nl,
		write('Peça a mover?'), nl,
		readXY( Ox, Oy ), nl,
		getPawn(Board, Ox, Oy, Pawn),
		checkCorrectPlayer(Player, Pawn),
		repeat,
		write('Casa de destino?'), nl,	
		readXY( Dx, Dy ), nl,
		transitPlay(Board, Side, GameMode, Player, Ox, Oy, Dx, Dy ).
	
play(Board, Side, GameMode) :-
		play(Board, Side, GameMode, 1).


% **********************************************************************
% **********************************************************************
	
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
		
% **********************************************************************
% **********************************************************************


	
% Inicializa e imprime tabuleiro no estado inicial.

%init :-
%	repeat,
%	write('Lado do tabuleiro [5,19] = '), read(L),
%	integer(L),
%	L >= 5,
%	L =< 19,
%	initDynBoard(L, A),
%	nl,
%	printBoard(A),
%	move(A, 1).

%move(A, _):-
%	nl,
%	isWinner(A, X),
%	write('O jogo terminou! Venceu o Jogador '),
%	write(X),
%	!.
        
%move(A, P) :- 
%	nl,
%	write('Jogador '), write(P), nl,
%	repeat,
%	write('Peça a mover?'), nl,
%	write('X = '), read(Ox),
%	write('Y = '), read(Oy),nl,
%	integer(Ox),
%	integer(Oy),
%	getPawn(A, Ox, Oy, P1),
%	P1 = P,
%	write('Casa de destino?'), nl,
%	write('X = '), read(Dx),
%	write('Y = '), read(Dy),nl,
%	movePawn(A, Ox, Oy, Dx, Dy, Tab),
%	printBoard(Tab),
%	switchPlayer(P, NP),
%	move(Tab, NP).



% *********************************************************************
% Testes
% *********************************************************************

%getPlayer(J, X, Y):-
%	board8x8(A),
%	getPawn(A, X, Y, J).

%verificaVencedor(P):-
%	finalBoard(A),
%	isWinner(A, P).

%validaJogada(Ox, Oy, Dx, Dy):-
%	midBoard(A),
%	getPawn(A, Ox, Oy, P),
%	checkMove(A, Ox, Oy, Dx, Dy, P).

%movePawn(Ox, Oy, Dx, Dy):-
%	midBoard(A),
%	movePawn(A, Ox, Oy, Dx, Dy, Tab),
%	printBoard(Tab).

% *********************************************************************
% Outro código
% *********************************************************************

% count(+Tabuleiro, +Jogador, +Numero)
% Retorna o número de peças do Jogador presentes
% no Tabuleiro.

%count([], _, 0).

%count([H|T], H, N):-
%       count(T, H, N1),
%       N is N1+1.

%count([H|T], P, N):-
%       is_list(H),
%       count(H, P, N1),
%       count(T, P, N2),
%       N is N1+N2.

%count([H|T], P, N):-
%       not(is_list(H)),
%       count(T, P, N).

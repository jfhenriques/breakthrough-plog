
% **********************************************************************
% **********************************************************************

% Inicializa o tabuleiro.
initBoard([
           [ 1, 1, 1, 1, 1, 1, 1, 1 ],
           [ 1, 1, 1, 1, 1, 1, 1, 1 ],
           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
           [ 2, 2, 2, 2, 2, 2, 2, 2 ],
           [ 2, 2, 2, 2, 2, 2, 2, 2 ]
          ]).

midBoard([
           [ 1, 1, 1, 1, 1, 1, 1, 1 ],
           [ 1, 1, 1, 1, 1, 0, 1, 1 ],
           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
           [ 0, 0, 0, 0, 0, 1, 0, 0 ],
           [ 0, 0, 0, 0, 2, 2, 0, 0 ],
           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
           [ 2, 2, 2, 2, 2, 0, 0, 2 ],
           [ 2, 2, 2, 2, 2, 2, 2, 2 ]
          ]).

finalBoard([
           [ 1, 1, 1, 1, 1, 1, 1, 1 ],
           [ 1, 1, 1, 0, 1, 0, 1, 0 ],
           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
           [ 0, 0, 2, 0, 1, 1, 0, 0 ],
           [ 0, 0, 0, 0, 0, 0, 1, 0 ],
           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
           [ 2, 2, 0, 2, 2, 1, 2, 0 ],
           [ 2, 2, 2, 2, 2, 2, 2, 2 ]
          ]).

finalBoard2([
           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
           [ 0, 0, 2, 0, 0, 0, 0, 0 ],
           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
           [ 0, 0, 0, 0, 0, 1, 0, 0 ],
           [ 2, 2, 0, 2, 2, 0, 2, 0 ],
           [ 2, 2, 2, 2, 2, 2, 2, 2 ]
          ]).


smallBoard([
           [1,1,1],
           [0,0,0],
           [2,2,2]
           ]).
% **********************************************************************
% **********************************************************************

% Imprime a pe�a do jogador correspondente.
writePlayer(1) :-
        write('1').
writePlayer(2) :-
        write('2').
writePlayer(0) :-
        write(' ').


% **********************************************************************
% **********************************************************************

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


% **********************************************************************
% **********************************************************************

% Imprime a linha de separa��o horizontal.
printHorizSep   :-
        write('   --- --- --- --- --- --- --- ---').

% Imprime os n�meros das colunas.
printColNumbers :-
        write('    1   2   3   4   5   6   7   8').


% **********************************************************************
% **********************************************************************

% Crit�rio de paragem.
printFullRow([], _).

% Imprime o n�mero da linha no �nicio e no fim de cada itera��o,
% bem como o separador horizontal. Chama tamb�m o predicado que ir�
% imprimir todas as c�lulas de cada linha, com a respectiva pe�a.
% Por fim chama recursivamente a mesma fun��o com a cauda da
% lista (contendo as restantes c�lulas da linha correspondente),
% e o n�mero actual da linha, at� atingir o crit�rio de paragem,
% ou seja, quando a a cauda for uma lista vazia.
printFullRow([H|T], N) :-
        N1 is N+1,
        printHorizSep,
        nl,
        write(N),
        write(' '),
        printRow(H),
        write('| '),
        write(N),
        nl,
        printFullRow(T, N1).


% **********************************************************************
% **********************************************************************

% Regra a ser usada quando � passado um tabuleiro vazio.
printBoard([]).

% Imprime o tabuleiro, chamando o predicado que imprime o n�mero
% das colunas (no in�cio e no fim do tabuleiro),
% e de seguida o predicado que imprime cada linha individual.
% � imprimido tamb�m o separador horizontal do tabuleiro.
printBoard([H|T]) :-
        printColNumbers,
        nl,
        printFullRow([H|T], 1),
        printHorizSep,
        nl,
        printColNumbers.


% **********************************************************************
% getPawn(+Tabuleiro, +X, +Y, -Jogador)
% Retorna o n�mero do jogador na posi��o (X,Y) do Tabuleiro.
% Recorre a getPawnPos() para percorrer o tabuleiro.
% **********************************************************************

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
   X < 20,
   Y < 20,
   getPawnPos(InTab, Y, 1, L),
   getPawnPos(L, X, 1, Player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Obter valor de casa
% getll(Linha,Coluna,Tabuleiro,Valor)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Obtem o valor de determinada posi��o N numa Lista.
getl(Lista,N,Valor):-
          getl(Lista,N,1,Valor).

getl([H|_],N,N,H):-!.

getl([_|R],N,Nactual,Valor):-
          NR is Nactual+1,
          getl(R,N,NR,Valor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Obtem o valor de determinada posi��o (Linha,Coluna) num Tabuleiro
getll(Linha,Coluna,Tabuleiro,Valor):-
          % Obtemos primeiro uma Linha completa
          getl(Tabuleiro,Linha,Lista),
          % Obtemos o valor da coluna na Linha obtida
          getl(Lista,Coluna,Valor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Substitui a pe�a de uma Casa por outra
% substll(Tabuleiro,Linha, Coluna, Valor, Resultado)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
% Verifica��o de um vencedor
% **********************************************************************

checkForInvasion([]).
checkForInvasionOfP1([]).

% Verificar se h� pe�as do jogador 1 na �ltima linha
% (�ltimo predicado a ser executado)
checkForInvasionOfP1([InTabT], Player):-
        member(1, InTabT),
        Player is 1,
        !.

% Corrre resto do tabuleiro recursivamente.
checkForInvasionOfP1([_|InTabT], Player):-
        checkForInvasionOfP1(InTabT, Player).

% Verificar se h� pe�as do jogador 2 na primeira linha
% (Primeiro predicado a ser executado).
checkForInvasion([InTabH|_], Player):-
        member(2, InTabH),
        Player is 2,
        !.

% Se n�o encontrar pe�as do jogador 2, corre
% recursivamente o resto do tabuleiro at� chegar � ultima linha.
checkForInvasion([_|InTabT], Player):-
        checkForInvasionOfP1(InTabT, Player).

% Verifica se existe pelo menos 1 pe�a do jogador P
hasPieces([], _).
hasPieces([H|T], P) :-
             not(member(P, H)),
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

% **********************************************************************
% Valida��o de uma jogada
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
	write('Quer capturar a pe�a advers�ria? (y/n)'),
	nl,
        read(Ans),
	Ans = 'y',
	substll(Tab, Dy, Dx, Me, TabTmpOut),
	substll(TabTmpOut, Oy, Ox, 0, TabOut).


%movePawn( _, _,_, _,_, [], _ ).

%movePawn( Player, L, Ox,Oy, Dx,Dy, [iLinha_H|iLinha_T], Tab_out ) :-
%	not(L = Oy ; L = Dy),
%	L2 is L + 1,
%	append( Tab_out, iLinha_H, newTab ),
%	movePawn( Player, L2, Ox,Oy, Dx,Dy, new_Tab, newTab ).


%movePawn( Player, L, Ox,Oy, Dx,Dy, [_|iLinha_T], Tab_out ) :-
%	L = Oy; L = Dy,
%	( L = Oy ->
%		( NP is 0,
%		  NX is Ox );
%		( NP is Player,
%		  NX is Dx )
%	),
%	movePawn_linha( NP, 1, NX, [iCol_H|iCol_T], Linha),
%	L2 = L + 1,
%	append( Tab_out, Linha, new_Tab ),
%	movePawn( Player, L2, Ox,Oy, Dx,Dy, iLinha_T, new_Tab ).



%movePawn_linha( _, _, _, [], _ ).

%movePawn_linha( Player, C, X, [iCol_H|iCol_T], Linha) :-
%	not(C = X),
%	C2 is C + 1,
%	append(Linha, iCol_H, new_Linha),
%	movePawn_linha( Player, C2, X, iCol_T, new_Linha).

%movePawn_linha( Player, C, X, [_|iCol_T], Linha) :-
%	C = X,
%	C2 is C + 1,
%	append(Linha, Player, new_Linha),
%	movePawn_linha( Player, C2, X, iCol_T, new_Linha).




% **********************************************************************
% Capture player
% **********************************************************************

% Captura a pe�a na casa de destino
%capturePawn( [ Dx, Dy ] ).



% **********************************************************************
% **********************************************************************

% Inicializa e imprime tabuleiro no estado inicial.
init :-
        initBoard(A),
        printBoard(A),
	move(A, 1). %TODO gerar numero aleatorio


move(A, _):-
	isWinner(A, 1),
	nl,
	write('O jogo terminou! Venceu o Jogador 1.'),
        !.

move(A, _):-
	isWinner(A, 2),
	nl,
	write('O jogo terminou! Venceu o Jogador 2.'),
        !.

move(A, P) :-
	not(isWinner(A, 1)),
	not(isWinner(A, 2)),
	nl,nl,
	write('Jogador '), write(P), nl,
	write('Pe�a a mover?'), nl,
	write('X ='), nl, read(Ox),nl,
	write('Y ='), nl, read(Oy),nl,
	write('Casa de destino?'), nl,
	write('X ='), nl, read(Dx),nl,
	write('Y ='), nl, read(Dy),nl,
	getPawn(A, Ox, Oy, P),
	movePawn(A, Ox, Oy, Dx, Dy, Tab),
	repeat,
	printBoard(Tab),
	switchPlayer(P, NP),
	move(Tab, NP).




% *********************************************************************
% Testes
% *********************************************************************

getPlayer(J, X, Y):-
       initBoard(A),
       getPawn(A, X, Y, J).

verificaVencedor(P):-
        finalBoard(A),
        isWinner(A, P).

validaJogada(Ox, Oy, Dx, Dy):-
	midBoard(A),
	getPawn(A, Ox, Oy, P),
	checkMove(A, Ox, Oy, Dx, Dy, P).

movePawn(Ox, Oy, Dx, Dy):-
	midBoard(A),
	movePawn(A, Ox, Oy, Dx, Dy, Tab),
	printBoard(Tab).

% *********************************************************************
% Outro c�digo
% *********************************************************************

% count(+Tabuleiro, +Jogador, +Numero)
% Retorna o n�mero de pe�as do Jogador presentes
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


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

finalBoard([
           [ 1, 1, 1, 1, 1, 1, 1, 1 ],
           [ 1, 1, 1, 0, 1, 0, 1, 0 ],
           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
           [ 0, 0, 2, 0, 1, 1, 0, 0 ],
           [ 0, 0, 0, 0, 0, 0, 1, 0 ],
           [ 0, 0, 0, 0, 0, 0, 0, 0 ],
           [ 2, 2, 0, 2, 2, 2, 2, 0 ],
           [ 2, 2, 2, 2, 2, 2, 1, 2 ]
          ]).


smallBoard([
	    [1,1,1,1],
	    [0,0,0,0],
	    [0,0,0,0],
	    [2,2,2,2]
	   ]
	  ).

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
% **********************************************************************

% Inicializa e imprime tabuleiro no estado inicial.
init :-
        initBoard(A),
        printBoard(A).


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
   X < 20,
   Y < 20,
   getPawnPos(InTab, Y, 1, L),
   getPawnPos(L, X, 1, Player).

printPlayer(J):-
       initBoard(A),
       getPawn(A, 6, 7, J).

% Verificar se h� pe�as do jogador 2 primeira linha
% Verificar se h� pe�as do jogador 1 �ltima linha

checkForInvasion([]).
checkForInvasionOfP1([]).

checkForInvasionOfP1([InTabT], Player):-
	member(1, InTabT),
	Player is 1,
	!.

checkForInvasionOfP1([_|InTabT], Player):-
	checkForInvasionOfP1(InTabT, Player).

checkForInvasion([InTabH|_], Player):-
	member(2, InTabH),
	Player is 2,
	!.

checkForInvasion([_|InTabT], Player):-
	checkForInvasionOfP1(InTabT, Player).

checkWinner(Tab, Player) :-
	checkForInvasion(Tab, Player).
%falta fazer a contagem de pe�as de cada jogador,
%que � a segunda forma de determinar o vencedor.

verificaVencedor(P):-
	finalBoard(A),
	checkWinner(A, P).

% Move C1R1 para C2R2
%movePawn(+[Ox,Oy], +[Dx,D2], +Tabuleiro_in, -Tabuleiro_out).

%   movePawn([Ox,Oy], [Dx,Dy], [iTab_H|iTab_T], Tab_out):-


% Captura a pe�a na casa de destino
%capturePawn( [ Dx, Dy ] ).

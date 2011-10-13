
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

% Imprimie a linha de separa��o horizontal.
printHorizSep   :-
        write('   --- --- --- --- --- --- --- ---').

% Imprimie os n�meros das colunas.
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
% **********************************************************************
getPawnPos([InTabH|_], Oy, Oy, InTabH) :- !.

getPawnPos([_|InTabT], Oy, Y, Linha) :-
   Y1 is Y + 1,
   getPawnPos(InTabT, Oy, Y1, Linha).

getPawn(InTab, X, Y, Jogador) :-
   X < 20,
   Y < 20,
   getPawnPos(InTab, Y, 1, L),
   getPawnPos(L, X, 1, Jogador).


printPlayer(J):-
       initBoard(A),
       getPawn(A, 6, 7, J).


% Move C1R1 para C2R2
%movePawn(+[Ox,Oy], +[Dx,D2], +Tabuleiro_in, -Tabuleiro_out).

%   movePawn([Ox,Oy], [Dx,Dy], [iTab_H|iTab_T], Tab_out):-


% Captura a pe�a na casa de destino
%capturePawn( [ Dx, Dy ] ).
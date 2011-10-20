
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
           [1,1],
           [0,0],
           [2,0]
           ]).
% **********************************************************************
% **********************************************************************

% Imprime a peça do jogador correspondente.
writePlayer(1) :-
        write('1').
writePlayer(2) :-
        write('2').
writePlayer(0) :-
        write(' ').


% **********************************************************************
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
printHorizSep   :-
        write('   --- --- --- --- --- --- --- ---').

% Imprime os números das colunas.
printColNumbers :-
        write('    1   2   3   4   5   6   7   8').


% **********************************************************************
% **********************************************************************

% Critério de paragem.
printFullRow([], _).

% Imprime o número da linha no ínicio e no fim de cada iteração,
% bem como o separador horizontal. Chama também o predicado que irá
% imprimir todas as células de cada linha, com a respectiva peça.
% Por fim chama recursivamente a mesma função com a cauda da
% lista (contendo as restantes células da linha correspondente),
% e o número actual da linha, até atingir o critério de paragem,
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

% Regra a ser usada quando é passado um tabuleiro vazio.
printBoard([]).

% Imprime o tabuleiro, chamando o predicado que imprime o número
% das colunas (no início e no fim do tabuleiro),
% e de seguida o predicado que imprime cada linha individual.
% É imprimido também o separador horizontal do tabuleiro.
printBoard([H|T]) :-
        printColNumbers,
        nl,
        printFullRow([H|T], 1),
        printHorizSep,
        nl,
        printColNumbers.


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
   X < 20,
   Y < 20,
   getPawnPos(InTab, Y, 1, L),
   getPawnPos(L, X, 1, Player).

% **********************************************************************
% checkWinner(+Tabuleiro, -Jogador)
% Verifica se houve um vencedor e retorna-o
% **********************************************************************

checkForInvasion([]).
checkForInvasionOfP1([]).

% Verificar se há peças do jogador 1 na última linha
checkForInvasionOfP1([InTabT], Player):-
        member(1, InTabT),
        Player is 1,
        !.

% Corrre resto do tabuleiro recursivamente.
checkForInvasionOfP1([_|InTabT], Player):-
        checkForInvasionOfP1(InTabT, Player).

% Verificar se há peças do jogador 2 na primeira linha
checkForInvasion([InTabH|_], Player):-
        member(2, InTabH),
        Player is 2,
        !.

% Se não encontrar peças do jogador 2, corre
% recursivamente o resto do tabuleiro até chegar à ultima linha.
checkForInvasion([_|InTabT], Player):-
        checkForInvasionOfP1(InTabT, Player).

hasPieces([], _).
hasPieces([H|T], P) :-
             not(member(P, H)),
             hasPieces(T, P).

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
% Move player
% **********************************************************************

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

% Captura a peça na casa de destino
%capturePawn( [ Dx, Dy ] ).



% **********************************************************************
% **********************************************************************

% Inicializa e imprime tabuleiro no estado inicial.
init :-
        initBoard(A),
        printBoard(A).
		
		
		
% *********************************************************************
% TESTES
% *********************************************************************

printPlayer(J):-
       initBoard(A),
       getPawn(A, 6, 7, J).

verificaVencedor(P):-
        finalBoard(A),
        isWinner(A, P).
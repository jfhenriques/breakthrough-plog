
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
printRow([])    :- 
	write('| ').

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

% Imprimie a linha de separação horizontal.
printHorizSep   :-
	write('   --- --- --- --- --- --- --- ---').

% Imprimie os números das colunas.
printColNumbers :-
	write('    1   2   3   4   5   6   7   8').


% **********************************************************************
% **********************************************************************
	
% Critério de paragem.
printFullRow([], _).

% Imprime o número da linha no ínicio e no fim de cada iteração,
% bem como o separador horizontal. Chama também o predicado que irá
% imprimir todas as celulas de cada linha, com a respectiva peça.
% Por fim chama recursivamente a mesma função com a cauda da
% lista, e o número actual da linha, até atingir o critério de paragem,
% ou seja, quando a a cauda for uma lista vazia.
printFullRow([H|T], N) :-
	N1 is N+1,
	printHorizSep,
	nl,
	write(N),
	write(' '),
	printRow(H),
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
% **********************************************************************

% Inicializa e imprime tabuleiro no estado inicial.
init :-
	initBoard(A),
	printBoard(A).
	
	
% **********************************************************************
% **********************************************************************
	
% Move C1R1 para C2R2
%movePawn([Ox,Oy], [Dx,D2]).
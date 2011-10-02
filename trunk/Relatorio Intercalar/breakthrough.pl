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


% Imprime a peça do jogador correspondente
writePlayer(1) :-
	write('1').
writePlayer(2) :-
	write('2').
writePlayer(0) :-
	write(' ').

% Imprime elementos da sublista
printRow([])    :-
	write('| ').
printRow([H|T]) :-
	write('| '),
	writePlayer(H),
	write(' '),
	printRow(T).

% Imprimie a linha de separação horizontal
printHorizSep   :-
	write('   --- --- --- --- --- --- --- ---').
% Imprimie os números das colunas
printColNumbers :-
	write('    1   2   3   4   5   6   7   8').

%Acrescenta número da linha no início e no fim da sublista
printFullRow([], _).
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

%Imprime tabuleiro,  com as linhas e colunas numeradas
printBoard([]).
printBoard([H|T]) :-
	printColNumbers,
	nl,
	printFullRow([H|T], 1),
	printHorizSep,
	nl,
	printColNumbers.

%Imprime tabuleiro inicial
init :-
	initBoard(A),
	printBoard(A).
	
	
	
% Move C1R1 para C2R2
%movePawn([Ox,Oy], [Dx,D2]).
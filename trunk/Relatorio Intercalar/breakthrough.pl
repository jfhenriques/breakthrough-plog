initBoard([
	   [1, 1, 1, 1, 1, 1, 1, 1],
	   [1, 1, 1, 1, 1, 1, 1, 1],
	   [0, 0, 0, 0, 0, 0, 0, 0],
	   [0, 0, 0, 0, 0, 0, 0, 0],
	   [0, 0, 0, 0, 0, 0, 0, 0],
	   [0, 0, 0, 0, 0, 0, 0, 0],
	   [2, 2, 2, 2, 2, 2, 2, 2],
	   [2, 2, 2, 2, 2, 2, 2, 2]
	  ]).


writePlayer(1) :-
	write('1').
writePlayer(2) :-
	write('2').
writePlayer(0) :-
	write(' ').

% Imprime elementos da sublista
printRow([]).
printRow([H|T]) :-
	write('| '),
	writePlayer(H),
	write(' '),
	printRow(T).
printHorizSep :-
	write('   --- --- --- --- --- --- --- ---').
printColLetters :-
	write('    a   b   c   d   e   f   g   h').

%Acrescenta número da linha no início e no fim da sublista
printFullRow([], _).
printFullRow([H|T], N):-
	N1 is N+1,
	%write(' | | | | | | | | |'),
	printHorizSep,
	nl,
	write(N),
	write(' '),
	printRow(H),
	write('|'),
	write(' '),
	write(N),
	nl,
	printFullRow(T, N1).

%Imprime tabuleiro, com colunas numeradas
printBoard([]).
printBoard([H|T]):-
	%write('   a   b c d e f g h'),
	%write('   ------------------------------'),
	%nl,
	%write(' -----------------------------------'),
	printColLetters,
	nl,
	printFullRow([H|T], 1),
	%write(' -----------------------------------'),
	%nl,
	%write(' a b c d e f g h').
	printHorizSep,
	nl,
	printColLetters.

%Imprime tabuleiro inicial
init:-
	initBoard(A),
	printBoard(A).



















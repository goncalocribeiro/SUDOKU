:- include('SUDOKU').
:- include('exemplos').

% N_Puz e o puzzle resultante de tirar o numero Num da posicao Pos do puzzle Puz
% Sug: Utilizar puzzle_ref e puzzle_muda
tira_num_aux(Num, Puz, Pos, N_Puz):- 
	puzzle_ref(Puz, Pos, Cont_Pos),
	member(Num, Cont_Pos),
	subtract(Cont_Pos, [Num], N_Cont_Pos),
	puzzle_muda_propaga(Puz, Pos, N_Cont_Pos, N_Puz), !.
tira_num_aux(_,Puz,_,Puz).

% N_Puz e o puzzle resultante de tirar o numero Num de todas as posicoes Posicoes do puzzle Puz
% Sug: Utilizar percorre_muda_Puz
tira_num(Num, Puz, Posicoes, N_Puz):-
	percorre_muda_Puz(Puz, tira_num_aux(Num), Posicoes, N_Puz).


% Faz o mesmo que puzzle_muda mas no caso de Cont ser uma lista unitaria, propaga a mudanca, isto e, retira o numero em Cont de todas as linhas e colunas relacionadas com Pos
% Sug: Utilizar puzzle_ref, puzzle_muda e posicoes_relacionadas
puzzle_muda_propaga(Puz, Pos, Cont, N_Puz):-
	length(Cont, 1), !,
	posicoes_relacionadas(Pos, Posicoes),
	nth1(1, Cont, N),
	puzzle_muda(Puz, Pos, [N], N_Puz_1),
	tira_num(N, N_Puz_1, Posicoes, N_Puz).
	
puzzle_muda_propaga(Puz, Pos, Cont, N_Puz):-
	puzzle_muda(Puz, Pos, Cont, N_Puz).


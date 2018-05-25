%---------------------------------------------------------------------
%                   Andre Mendonca - 82304
%---------------------------------------------------------------------

:- include('SUDOKU').

%---------------------------------------------------------------------
% 		PREDICADOS AUXILIARES
%---------------------------------------------------------------------

%---------------------------------------------------------------------
% membro/2
%
% - Membro(Elem,Lst) em que devolve True caso Elem esteja contido em Lst
% e false caso contrario
%
% - Predicado auxiliar de tira_num_aux e possibilidades
%---------------------------------------------------------------------

membro(X,[X|_]).   		 
membro(X,[_|Y]):- membro(X,Y).

%---------------------------------------------------------------------
% ocorrencias/3
%
% - ocorrencias(Elem,Lst,Num) em que Num e o numero de vezes em que Elem 
%   aparece em Lst
%
% - Predicado auxiliar de so_aparece_uma_vez/4
%---------------------------------------------------------------------

ocorrencias(_, [], 0) :- !. /* lista vazia, caso de paragem */

ocorrencias(X, [X|T], N) :- /* se X estiver no inicio da lista */
    ocorrencias(X, T, N2), 
    N is N2 + 1.     

ocorrencias(X, [Y|T], N) :- /* se X nao estiver no inicio da lista */
    X \= Y,          
    ocorrencias(X, T, N). 

%---------------------------------------------------------------------
% escolhe_posicao
%
% - escolhe_posicao(Puz,Pos) em que Pos e uma posicao cujo conteudo e  
%   uma lista nao unitaria
%
% - Predicado auxiliar de resolve/2
%---------------------------------------------------------------------

escolhe_posicao(Puz,Pos):- 
	todas_posicoes(LPos),
	length(LPos,Limit),
	escolhe_posicao(Puz,Pos,LPos,Limit).

escolhe_posicao(_,Pos,Pos,0):- !.

escolhe_posicao(Puz,Pos,[Cabeca|Cauda],Limit):- 
	puzzle_ref(Puz,Cabeca,Cont),
	length(Cont,Dim),
	Dim =:= 1,!,
	Less is Limit - 1,
	escolhe_posicao(Puz,Pos,Cauda,Less).

escolhe_posicao(_,Pos,[Cabeca|_],_):- Pos = Cabeca.

%---------------------------------------------------------------------
% 	PREDICADOS PARA PROPAGACAO DE MUDANCAS
%---------------------------------------------------------------------

%---------------------------------------------------------------------
% tira_num_aux/4
%
% - tira_num_aux(Num,Puz,Pos,N_Puz) em que N_Puz e o puzzle resultante
%   de tirar o numero Num da posicao Pos de Puz
%
% - Predicado auxiliar de tira_num/4
%---------------------------------------------------------------------

tira_num_aux(Num,Puz,Pos,N_Puz):-
	puzzle_ref(Puz,Pos,Cont),
	membro(Num,Cont),
	delete(Cont,Num,Newcont),
	puzzle_muda_propaga(Puz,Pos,Newcont,N_Puz),!.

tira_num_aux(_,Puz,_,Puz).


%---------------------------------------------------------------------
% tira_num/4
%
% - tira_num(Num,Puz,Posicoes,N_Puz) em que N_Puz e o puzzle resultante
% de tirar o numero Num de Posicoes em Puz
%
% - Predicado auxiliar de puzzle_muda_propaga/4
%---------------------------------------------------------------------

tira_num(Num,Puz,Posicoes,N_Puz):- percorre_muda_Puz(Puz,tira_num_aux(Num),Posicoes,N_Puz).

%---------------------------------------------------------------------
% puzzle_muda_propaga/4
%
% - puzzle_muda_propaga(Puz,Pos,Cont,N_Puz) retira o numero em Cont de
%   de todas as posicoes relacionadas com Pos
%
% - Predicado auxiliar de tira_num_aux, inicializa_aux e inspecciona_num
%---------------------------------------------------------------------

puzzle_muda_propaga(Puz,Pos,[Cabeca|Cauda],N_Puz):- 
	Cauda == [],!,
	Cont = [Cabeca|Cauda],
	posicoes_relacionadas(Pos,LRel),
	nth1(1,Cont,Val),
	puzzle_muda(Puz,Pos,[Val],P2),
	tira_num(Val,P2,LRel,N_Puz).

puzzle_muda_propaga(Puz,Pos,Cont,N_Puz):-  puzzle_muda(Puz,Pos,Cont,N_Puz).

%---------------------------------------------------------------------
% 	PREDICADOS PARA INICIALIZACAO DE PUZZLES
%---------------------------------------------------------------------

%---------------------------------------------------------------------
% possibilidades/3
% 
% - possibilidades(Pos,Puz,Poss) em que Poss e a lista de numeros 
%   possiveis para a posicao Pos do puzzle Puz
%
% - Predicado auxiliar de inicializa_aux/3
%---------------------------------------------------------------------

possibilidades(Pos,Puz,Poss):- 
	puzzle_ref(Puz,Pos,Cont),
	length(Cont,X),
	X \= 1,!,
	numeros(Lst),
	length(Lst,Limit),
	possibilidades(Pos,Puz,[],Poss,Lst,Limit).

possibilidades(Pos,Puz,Poss):- 
	puzzle_ref(Puz,Pos,Cont),
	Poss = Cont.

possibilidades(_,_,Poss,Poss,[],0):- !.

possibilidades(Pos,Puz,R,Poss,[CabecaN|CaudaN],Limit):- 
	posicoes_relacionadas(Pos,LRel),
	conteudos_posicoes(Puz,LRel,LCont),
	membro([CabecaN],LCont),!,
	Less is Limit - 1,
	possibilidades(Pos,Puz,R,Poss,CaudaN,Less).

possibilidades(Pos,Puz,R,Poss,[CabecaN|CaudaN],Limit):- 
	Less is Limit - 1,
	sort([CabecaN|R],NewList),
	possibilidades(Pos,Puz,NewList,Poss,CaudaN,Less).

%---------------------------------------------------------------------
% inicializa_aux/3
%
% - inicializa_aux(Puz,Pos,N_Puz) em que N_Puz e o puzzle resultante
%   de colocar na posicao Pos do puzzle Puz a lista com os numeros
%   posiveis para essa posicao
%
% - Predicado auxiliar de inicializa/3
%---------------------------------------------------------------------

inicializa_aux(Puz,Pos,N_Puz):- 
	puzzle_ref(Puz,Pos,Cont),
	length(Cont,X),
	X \= 1,!,
	possibilidades(Pos,Puz,Poss),
	puzzle_muda_propaga(Puz,Pos,Poss,N_Puz).

inicializa_aux(Puz,_,Puz):- !.

%---------------------------------------------------------------------
% inicializa/3
%
% - inicializa(Puz,N_Puz) em que N_Puz e o puzzle resultante
%   de inicializar o puzzle Puz
%---------------------------------------------------------------------

inicializa(Puz,N_Puz):- 
	todas_posicoes(AllPositions),
	percorre_muda_Puz(Puz,inicializa_aux,AllPositions,N_Puz).

%---------------------------------------------------------------------
% 	PREDICADOS PARA INSPECCAO DE PUZZLES
%---------------------------------------------------------------------

%---------------------------------------------------------------------
% so_aparece_uma_vez/4
%
% - so_aparece_uma_vez(Puz,Num,Posicoes,Pos_Num) em que Num so aparece 
%   numa das posicoes da lista Posicoes do puzzle Puz, e que essa
%   posicao e a Pos_Num
%
% - Predicado auxiliar de inspecciona_num e grupo_correcto
%---------------------------------------------------------------------

so_aparece_uma_vez(Puz,Num,Posicoes,Pos_Num):- 
	conteudos_posicoes(Puz,Posicoes,LCont),
	flatten(LCont,JuntaCont), /* Transforma uma Lista de listas numa lista sem listas com elementos unitarios */
	ocorrencias(Num,JuntaCont,N_oc),
	N_oc =:= 1,!,
	puzzle_ref(LCont,(X,_),Num), /* X e a linha referente ao conteudo Num da lista de conteudos */
	nth1(X,Posicoes,(Linha,Coluna)),
	Pos_Num = (Linha,Coluna),!.

%---------------------------------------------------------------------
% inspecciona_num/4
%
% - inspecciona_num(Posicoes,Puz,Num,N_Puz) em que N_Puz e o resultado
%   de inspecionar o grupo cujas posicoes sao Posicoes, para Num
%
% - Predicado auxiliar de inspecciona_grupo/3
%---------------------------------------------------------------------

inspecciona_num(Posicoes,Puz,Num,N_Puz):- 
	not(so_aparece_uma_vez(Puz,Num,Posicoes,_)),!,
	Puz = N_Puz.

inspecciona_num(Posicoes,Puz,Num,N_Puz):- 
	so_aparece_uma_vez(Puz,Num,Posicoes,Pos_Num),
	puzzle_ref(Puz,Pos_Num,Cont),
	length(Cont,X),
	X =\= 1,!,
	puzzle_muda_propaga(Puz,Pos_Num,[Num],N_Puz).

inspecciona_num(_,Puz,_,N_Puz):- Puz = N_Puz.
								 

%---------------------------------------------------------------------
% inspecciona_grupo/3
%
% - inspecciona_grupo(Puz,Gr,N_Puz) inspeciona o grupo cujas posicoes
%   cujas posicoes sao as da lista Gr, do puzzle Puz para cada um dos 
%   numeros possiveis, sendo o resultado o puzzle N_Puz
%
% - Predicado auxiliar de inspecciona/2
%---------------------------------------------------------------------

inspecciona_grupo(Puz,Gr,N_Puz):- 
	numeros(LNum),
	length(LNum,Limit),
	inspecciona_grupo(Puz,Gr,N_Puz,LNum,Limit).

inspecciona_grupo(N_Puz,_,N_Puz,_,0):- !.

inspecciona_grupo(Puz,Gr,N_Puz,[Cabeca|Cauda],Limit):- 
	inspecciona_num(Gr,Puz,Cabeca,NewPuz),
	Less is Limit - 1,
	inspecciona_grupo(NewPuz,Gr,N_Puz,Cauda,Less).

%---------------------------------------------------------------------
% inspecciona/2
%
% - inspecciona(Puz,Gr,N_Puz) inspeciona cada um dos grupos do
%   puzzle Puz, para cada um dos numeros possiveis, sendo o resultado 
%   o puzzle N_Puz 
%---------------------------------------------------------------------

inspecciona(Puz,N_Puz):- 
	grupos(LGroup),
	length(LGroup,Limit),
	inspecciona(Puz,N_Puz,LGroup,Limit).

inspecciona(N_Puz,N_Puz,_,0):- !.

inspecciona(Puz,N_Puz,[Cabeca|Cauda],Limit):- 
	inspecciona_grupo(Puz,Cabeca,NewPuz),
	Less is Limit - 1,
	inspecciona(NewPuz,N_Puz,Cauda,Less).

%---------------------------------------------------------------------
% 	PREDICADOS PARA VERIFICACAO DE SOLUCOES
%---------------------------------------------------------------------

%---------------------------------------------------------------------
% grupo_correcto/3
%
% - grupo_correcto(Puz,Nums,Gr) em que Puz e um puzzle, significa que 
%   o grupo de Puz cujas posicoes sao as da lista Gr esta correto,
%   isto e, que contem todos os numerosda lista Nums, sem repeticoes
%
% - Predicado auxiliar de solucao
%---------------------------------------------------------------------

grupo_correcto(Puz,Nums,Gr):- 
	conteudos_posicoes(Puz,Gr,LCont),
	grupo_correcto(Puz,Nums,LCont), !.

grupo_correcto(_,[],_):- !.

grupo_correcto(Puz,[Cabeca|Cauda],LCont):- 
	so_aparece_uma_vez(Puz,Cabeca,LCont,_),!,
	grupo_correcto(Puz,Cauda,LCont).

%---------------------------------------------------------------------
% solucao
%
% - solucao(Puz) em que Puz e uma solucao, isto e, que todos os seus
%   grupos contem todos os numeros possiveis, sem repeticoes
%---------------------------------------------------------------------

solucao(Puz):- 
   grupos(LGroup),
   length(LGroup,Limit),
   numeros(LNum),
   solucao(Puz,LGroup,LNum,Limit).

solucao(_,_,_,0):- !.

solucao(Puz,[Cabeca|Cauda],LNum,Limit):- 
	grupo_correcto(Puz,LNum,Cabeca),
	Less is Limit - 1,
	solucao(Puz,Cauda,LNum,Less).

%---------------------------------------------------------------------
% resolve/2
%
% - resolve(Puz,Sol) em que o puzzle Sol e solucao do puzzle Puz 
%---------------------------------------------------------------------

resolve(Puz,Sol):- 
	inicializa(Puz,P1),
	escolhe_posicao(P1,Pos),
	puzzle_ref(P1,Pos,Cont),
	puzzle_muda_propaga(P1,Pos,Cont,P2),
	inspecciona(P2,P3),
	solucao(P3),!,
	P2 = Sol.






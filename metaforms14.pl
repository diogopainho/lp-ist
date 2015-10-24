%Funcao comparePiece2 e a funcao que verifica se as pecas estao no
% tabuleiro fazendo uma vericacao semelhante a um equal

comparePiece2(_,[]).
comparePiece2(peca(A,B),[peca(X,Y)|T]):-
	((
    (nonvar(A),nonvar(B), nonvar(X), nonvar(Y), A==X,B==Y);
	(var(A),nonvar(B),var(X), nonvar(Y), B==Y);
	(nonvar(A),nonvar(X),var(B),var(Y), A==X));
	comparePiece2(peca(A,B), T)).


%%%%%% Pistas %%%%%%


trioLeft(Peca, Linha, Coluna, Tabuleiro) :-
	(coloca(Peca, Linha, Coluna, Tabuleiro);
	coloca(Peca, Linha, middle, Tabuleiro)).

trioRight(Peca, Linha, Coluna, Tabuleiro) :-
	(coloca(Peca, Linha, Coluna, Tabuleiro);
	coloca(Peca, Linha, middle, Tabuleiro)).

cobra(Peca, Linha, Coluna, Tabuleiro) :-
	(coloca(Peca, Linha, Coluna, Tabuleiro);
	coloca(Peca, Linha, middle, Tabuleiro)).

tSimples(Peca, Linha, Coluna, Tabuleiro) :-
	(coloca(Peca, Linha, Coluna, Tabuleiro);
	coloca(Peca, center, Coluna, Tabuleiro)).

tLeft(Peca, Linha, Coluna, Tabuleiro) :-
	(coloca(Peca, Linha, Coluna, Tabuleiro);
	coloca(Peca, Linha, middle, Tabuleiro)).


tRight(Peca, Linha, Coluna, Tabuleiro) :-
	(coloca(Peca, Linha, Coluna, Tabuleiro);
	coloca(Peca, Linha, middle, Tabuleiro)).

tInvertido(Peca, Linha, Coluna, Tabuleiro) :-
	(coloca(Peca, Linha, Coluna, Tabuleiro);
	coloca(Peca, center, coluna, Tabuleiro)).

cantoTopLeft(Peca, Linha, Coluna, Tabuleiro) :-
	(coloca(Peca, Linha, Coluna, Tabuleiro);
	coloca(Peca, top, middle, Tabuleiro);
	coloca(Peca, center, middle, Tabuleiro);
	coloca(Peca, center, left, Tabuleiro)).

cantoTopRight(Peca, Linha, Coluna, Tabuleiro) :-
	(coloca(Peca, Linha, Coluna, Tabuleiro);
	coloca(Peca, top, middle, Tabuleiro);
	coloca(Peca, center, middle, Tabuleiro);
	coloca(Peca, center, right, Tabuleiro)).

cantoBottomLeft(Peca, Linha, Coluna, Tabuleiro) :-
	(coloca(Peca, Linha, Coluna, Tabuleiro);
	coloca(Peca, Linha, middle, Tabuleiro);
	coloca(Peca, center, Coluna, Tabuleiro);
	coloca(Peca, center, middle, Tabuleiro)).


cantoBottomRight(Peca, Linha, Coluna, Tabuleiro) :-
	(coloca(Peca, Linha, Coluna, Tabuleiro);
	coloca(Peca, Linha, middle, Tabuleiro);
	coloca(Peca, center, Coluna, Tabuleiro);
	coloca(Peca, center, middle, Tabuleiro)).


diagonalGrave(Peca, Linha, Coluna, Tabuleiro) :-
	(coloca(Peca, Linha, Coluna, Tabuleiro);
	coloca(Peca, Linha, middle, Tabuleiro);
	coloca(Peca, center, Coluna, Tabuleiro);
	coloca(Peca, center, middle, Tabuleiro)).


diagonalAguda(Peca, Linha, Coluna, Tabuleiro) :-
	(coloca(Peca, Linha, Coluna, Tabuleiro);
	coloca(Peca, Linha, middle, Tabuleiro);
	coloca(Peca, center, Coluna, Tabuleiro);
	coloca(Peca, center, middle, Tabuleiro)).

% Funcao pesquisaCor que verifica se existem pecas com Cor e sem forma
% no tabuleiro e devolve-as num inteiro N, usando a primitiva lenght que
% devolve o comprimento da lista
%
pesquisaCor(Cor, Tabuleiro, N):-
findall(X,member(peca(X,Cor), Tabuleiro),K),
length(K,N).


% Funcao pesquisaForma que verifica se existem pecas com Forma e sem
% Cor no tabuleiro e devolve-as num inteiro N, usando a primitiva lenght que
% devolve o comprimento da lista


pesquisaForma(Forma, Tabuleiro, N):-
findall(X,member(peca(Forma,X), Tabuleiro),K),
	length(K,N).

% Funcao verifica que efectua uma verificacao no tabuleiro das pecas,
% utilizada no predicado check para verificar se as pecas correspondem
% aos resultados do desafio

verifica(Forma, Cor, Tabuleiro, Tabuleiro1):-
(comparePiece2(peca(Forma, Cor), Tabuleiro)
-> Tabuleiro1 = Tabuleiro
; descobreCor(peca(Forma, Cor), Tabuleiro, Tabuleiroaux),
  Tabuleiro1 = Tabuleiroaux
).

% Funcao que insere uma peca no tabuleiro com base na Cor
%
descobreCor(peca(Forma, Cor), Tabuleiro , Tabuleiro1):-
(   pesquisaCor(Cor, Tabuleiro, N),
 length(N,3)
->  insere(peca(Forma,Cor), _, Cor, Tabuleiro, Tabuleiro1)
; descobreForma(peca(Forma, Cor), Tabuleiro , Tabuleiro1)
).

% Funcao que insere uma peca no tabuleiro com base na Forma
%

descobreForma(peca(Forma, Cor), Tabuleiro , Tabuleiro1):-
(   pesquisaForma(Forma, Tabuleiro, N),
 length(N,3)
->  insere(peca(Forma,Cor), Forma, _, Tabuleiro, Tabuleiro1)
; insere(peca(Forma,Cor), _, _, Tabuleiro, Tabuleiro1)
).



% Funcao inserenovazio que insere a peca em espacos vazios
%
inserenovaziofinal(_,_,[],R, Final):-
	Final = R.
inserenovaziofinal(_,_,[H|T],R, Final):-
	append(R, [H], R2),
	inserenovaziofinal(_,_,T,R2, Final).
inserenovazio(_, _, [], [], []).
inserenovazio(A, B, [H|T], Resultado, Final):-
	(   var(H)
	  -> inserenovaziofinal(A, B, [B|T], Resultado, Final)
	;append(Resultado, [H], R2),
	inserenovazio(A,B,T,R2, Final)
	).

%Funcao insere que insere uma peca no Tabuleiro
%
insere(peca(Forma, Cor), FormaAlvo, CorAlvo, Tabuleiro, TabuleiroFinal):-
(   var(FormaAlvo)
->  replace(peca(_, CorAlvo),peca(Forma,Cor),Tabuleiro, [], TabuleiroFinal)
;     replace(peca(FormaAlvo,_),peca(Forma,Cor),Tabuleiro, [], TabuleiroFinal)
).

% Funcao comparPiece que serve para compara pecas e verificar se sao
% iguais ou nao
comparePiece(peca(A,B),peca(X,Y)):-
	(nonvar(A),nonvar(B), nonvar(X), nonvar(Y), A==X,B==Y);
	(var(A),nonvar(B),var(X), nonvar(Y), B==Y);
	(nonvar(A),nonvar(X),var(B),var(Y), A==X).


% Funcao replace que substitui uma dada peca por outra
%
replacefinal(_,_,[],R, Final):-
	Final = R.
replacefinal(_,_,[H|T],R, Final):-
	append(R, [H], R2),
	replacefinal(_,_,T,R2, Final).
replace(_, _, [], [], []).
replace(A, B, [H|T], Resultado, Final):-
	(   nonvar(H),
	    comparePiece(H, A)
	-> replacefinal(A, B, [B|T], Resultado, Final)
	;append(Resultado, [H], R2),
	replace(A,B,T,R2, Final)
	).




% Predicado Check que verifica a igualdade dos tabuleiros para os
% desafios

check(Tabuleiro,TabuleiroFinal):-
	verifica(quadrado, vermelho, Tabuleiro, Tabuleiro1),
	verifica(quadrado,azul, Tabuleiro1, Tabuleiro2),
	verifica(quadrado,amerelo,Tabuleiro2, Tabuleiro3),
	verifica(triangulo,vermelho,Tabuleiro3, Tabuleiro4),
	verifica(triangulo,azul,Tabuleiro4, Tabuleiro5),
	verifica(triangulo, amarelo, Tabuleiro5, Tabuleiro6),
	verifica(circulo, vermelho, Tabuleiro6, Tabuleiro7),
	verifica(circulo, azul, Tabuleiro7, Tabuleiro8),
	verifica(circulo, amarelo, Tabuleiro8, TabuleiroFinal).

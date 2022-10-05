%103210 Jose Santos Corte

:- [codigo_comum].

%Projeto

%2.1
extrai_ilhas_linha(N_L, Lista, Ilhas) :-
    findall(ilha(H,(N_L,N_C)),(nth1(N_C, Lista, H), H \== 0), Ilhas).

%2.2
ilhas(Puz, Ilhas) :-
    findall(Ilhas_Lista,(nth1(Linha, Puz, Linha_Lista), extrai_ilhas_linha(Linha, Linha_Lista, Ilhas_Lista)), Ilhas_Lista),
    append(Ilhas_Lista,Ilhas).

%2.3
get_last([], []).
get_last(List, List) :- length(List, 1), !.
get_last([_|T], Res) :- get_last(T, Res).

get_first([], []) :- !.
get_first([H|_], [H]).

vizinhas_aux(Ilhas, Y_Ilha, X_Ilha, Vizinhas) :-
    findall(Ilhas_Vizinhas, (member(Ilhas_Vizinhas, Ilhas),
    arg(2, Ilhas_Vizinhas, Cords_Comp), Cords_Comp = (Y_Comp, X_Comp),
    X_Comp == X_Ilha, Y_Comp < Y_Ilha), Vizinhas_X_Sup),

    get_last(Vizinhas_X_Sup, X_Sup),

    findall(Ilhas_Vizinhas, (member(Ilhas_Vizinhas, Ilhas),
    arg(2, Ilhas_Vizinhas, Cords_Comp), Cords_Comp = (Y_Comp, X_Comp),
    X_Comp == X_Ilha, Y_Comp > Y_Ilha), Vizinhas_X_Inf),
    
    get_first(Vizinhas_X_Inf, X_Inf),

    findall(Ilhas_Vizinhas, (member(Ilhas_Vizinhas, Ilhas),
    arg(2, Ilhas_Vizinhas, Cords_Comp), Cords_Comp = (Y_Comp, X_Comp),
    Y_Comp == Y_Ilha, X_Comp < X_Ilha), Vizinhas_Y_Left),

    get_last(Vizinhas_Y_Left, Y_Left),

    findall(Ilhas_Vizinhas, (member(Ilhas_Vizinhas, Ilhas),
    arg(2, Ilhas_Vizinhas, Cords_Comp), Cords_Comp = (Y_Comp, X_Comp),
    Y_Comp == Y_Ilha, X_Comp > X_Ilha), Vizinhas_Y_Right),

    get_first(Vizinhas_Y_Right, Y_Right),

    append(X_Sup, Y_Left, Vizinhas_1),    
    append(Vizinhas_1, Y_Right, Vizinhas_2),   
    append(Vizinhas_2, X_Inf, Vizinhas).   

vizinhas(Ilhas, Ilha, Vizinhas) :- 
    arg(2, Ilha, Cords),
    Cords = (Y_Ilha, X_Ilha),
    vizinhas_aux(Ilhas, Y_Ilha, X_Ilha, Vizinhas).

%2.4
estado(Ilhas, Estado) :-
    findall([Ilha, Vizinhas, []], (member(Ilha, Ilhas), 
    vizinhas(Ilhas, Ilha, Vizinhas)), Estado).

%2.5
%Nao estao na mesma linha
posicoes_entre(Pos1, Pos2, _) :-
    Pos1 = (X1, Y1),
    Pos2 = (X2, Y2),
    X1 \== X2,
    Y1 \== Y2,
    false.

%Mesmo X, Y1 < Y2
posicoes_entre(Pos1, Pos2, Posicoes) :-
    Pos1 = (X1, Y1),
    Pos2 = (X2, Y2),
    X1 == X2,
    Y1 < Y2,
    findall((X1, Y),(between(Y1, Y2, Y), Y1 \== Y, Y2 \== Y),Posicoes).

%Mesmo X, Y1 > Y2
posicoes_entre(Pos1, Pos2, Posicoes) :-
    Pos1 = (X1, Y1),
    Pos2 = (X2, Y2),
    X1 == X2,
    Y1 > Y2,
    findall((X1, Y),(between(Y2, Y1, Y), Y1 \== Y, Y2 \== Y),Posicoes).

%Mesmo Y, X1 > X2
posicoes_entre(Pos1, Pos2, Posicoes) :-
    Pos1 = (X1, Y1),
    Pos2 = (X2, Y2),
    X1 < X2,
    Y1 == Y2,
    findall((X, Y1),(between(X1, X2, X), X1 \== X, X2 \== X),Posicoes).

%Mesmo Y, X1 < X2
posicoes_entre(Pos1, Pos2, Posicoes) :-
    Pos1 = (X1, Y1),
    Pos2 = (X2, Y2),
    X1 > X2,
    Y1 == Y2,
    findall((X, Y1),(between(X2, X1, X), X1 \== X, X2 \== X),Posicoes).

%2.6
cria_ponte(Pos1, Pos2, Ponte) :-
    Pos1 = (X1, Y1),
    Pos2 = (X2, Y2),
    X1 == X2,
    Y1 < Y2,
    Ponte = ponte(Pos1, Pos2).

cria_ponte(Pos1, Pos2, Ponte) :-
    Pos1 = (X1, Y1),
    Pos2 = (X2, Y2),
    X1 == X2,
    Y1 > Y2,
    Ponte = ponte(Pos2, Pos1).

cria_ponte(Pos1, Pos2, Ponte) :-
    Pos1 = (X1, Y1),
    Pos2 = (X2, Y2),
    X1 < X2,
    Y1 == Y2,
    Ponte = ponte(Pos1, Pos2).

cria_ponte(Pos1, Pos2, Ponte) :-
    Pos1 = (X1, Y1),
    Pos2 = (X2, Y2),
    X1 > X2,
    Y1 == Y2,
    Ponte = ponte(Pos2, Pos1).

%2.7
elementos_comuns([], _, []).
elementos_comuns([H|T], Lst2, [H|Lst1]):- member(H, Lst2), !, elementos_comuns(T, Lst2, Lst1).
elementos_comuns([_|T], Lst2, Lst1):-  elementos_comuns(T, Lst2, Lst1).

caminho_livre_aux(Posicoes, Comp) :- 
    elementos_comuns(Posicoes, Comp, List),
    List == [],
    true.

caminho_livre_aux(Posicoes, Comp) :- 
    elementos_comuns(Posicoes, Comp, List),
    List \== [],
    false.

caminho_livre(Pos1, Pos2, _, I, Vz) :-
    arg(2, I, Cords_Ilha),
    arg(2, Vz, Cords_Vizinha),
    Pos1 == Cords_Ilha,
    Pos2 == Cords_Vizinha,
    true.

caminho_livre(Pos1, Pos2, _, I, Vz) :-
    arg(2, I, Cords_Ilha),
    arg(2, Vz, Cords_Vizinha),
    Pos2 == Cords_Ilha,
    Pos1 == Cords_Vizinha,
    true.

caminho_livre(_, _, Posicoes, I, Vz) :-
    arg(2, I, Cords_Ilha),
    arg(2, Vz, Cords_Vizinha),
    posicoes_entre(Cords_Ilha, Cords_Vizinha, Comp),
    caminho_livre_aux(Posicoes, Comp).

%2.8
actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada) :-
    Entrada = [I, Vizinhas, Pontes],
    findall(Vz, (member(Vz, Vizinhas), 
    caminho_livre(Pos1, Pos2, Posicoes, I, Vz)), Restantes),
    Nova_Entrada = [I, Restantes, Pontes].

%2.9
actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_Estado) :-
    posicoes_entre(Pos1, Pos2, Posicoes),
    findall(Nova_Entrada, (member(Entrada_Antiga, Estado), 
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada_Antiga, Nova_Entrada)),
    Novo_Estado).

%2.10
ilhas_terminadas(Estado, Ilhas_term) :-
    findall(ilha(N,Pos), 
    (member(Entrada, Estado), Entrada = [ilha(N,Pos), _, Pontes],
    N \== 'X', length(Pontes, N)), Ilhas_term).


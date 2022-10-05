eliminaNumeros([],[]).
eliminaNumeros(ListaInicial,ListaSemNumeros) :-
    exclude(number,ListaInicial,ListaSemNumeros).
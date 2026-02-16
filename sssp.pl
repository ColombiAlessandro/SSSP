:- dynamic graph/1.

:- dynamic arc/4.

:- dynamic vertex/2.

new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)), !.

new_vertex(G, V) :- vertex(G, V), !.
new_vertex(G, V) :- assert(vertex(G, V)), !.

vertices(G, []).
vertices(G, [H|T]) :- vertex(G, H), vertices(G, T).   

list_vertices(G) :- listing(vertex(G, K)).  %questo predicato con listing stampa sia regole che fatti

list_vertices_2(G) :- vertex(G, V),
		      writeln(vertex(G, V)),
		      fail. %predicato aggiuntivo che stampa solo i fatti tramite writeln

new_arc(G, U, V, Weight) :- arc(G, U, V, Weight), !.
new_arc(G, U, V, Weight) :- arc(G, V, U, Weight), !.
new_arc(G, U, V, Weight) :- arc(G, U, V, K), K\=Weight, retract(arc(G, U, V, K)), new_arc(G, U, V, Weight), !.
new_arc(G, U, V, Weight) :- assert(arc(G, U, V, Weight)), !.

new_arc(G, U, V) :- new_arc(G, U, V, 1), !.

arcs(G, []).
arcs(G, [H|T]) :- arc(G, H), arcs(G, T).

neighbors(G, V, V).
neighbors(G, V, [H|T]) :- vertex(G, V), arc(G, V, H, K), neighbors(G, V, T).
neighbors(G, V, [V|T]) :- vertex(G, V), neighbors(G, V, T).

list_arcs(G) :- listing(arc(G, U, V, J)).

list_graph(G) :- list_arcs(G), list_vertices(G).

delete_graph(G) :- retractall(vertex(G, _)), retractall(arc(G, _, _, _)). 

:- dynamic heap/2.
:- dynamic heap_entry/4.

new_heap(H) :- heap(H,_S), !.
new_heap(H) :- assert(heap(H, 0)), !. 

heap_size(H, S) :- heap(H, S).

empty(H) :- heap(H,S), S = 0.

not_empty(H) :- \+ empty(H).


heapify(H, P) :- 
	Ps is P*2, 
	\+ heap_entry(H, Ps, _, _),
	Pd is Ps + 1, \+ heap_entry(H, Pd, _, _).
heapify(H, P) :- Ps is P*2, heap_entry(H, Ps, Ks, Vs), Pd is Ps + 1, \+ heap_entry(H, Pd, Kd, Vd), heap_entry(H, P, K, V), heapify(H, Ps), K =< Ks.
heapify(H, P) :- Ps is P*2, heap_entry(H, Ps, Ks, Vs), Pd is Ps + 1, \+ heap_entry(H, Pd, Kd, Vd), heap_entry(H, P, K, V), heapify(H, Ps), K > Ks, swap(H, K, Ks).
heapify(H, P) :- Ps is P*2, heap_entry(H, Ps, Ks, Vs), Pd is Ps + 1, heap_entry(H, Pd, Kd, Vd), heap_entry(H, P, K, V), heapify(H, Ps), heapify(H, Pd), K =< Ks, K =< Kd.
heapify(H, P) :- Ps is P*2, heap_entry(H, Ps, Ks, Vs), Pd is Ps + 1, heap_entry(H, Pd, Kd, Vd), heap_entry(H, P, K, V), heapify(H, Ps), heapify(H, Pd), K > Ks, swap(H, K, Ks), K =< Kd.
heapify(H, P) :- Ps is P*2, heap_entry(H, Ps, Ks, Vs), Pd is Ps + 1, heap_entry(H, Pd, Kd, Vd), heap_entry(H, P, K, V), heapify(H, Ps), heapify(H, Pd), K =< Ks, K > Kd, swap(H, K, Kd).

insert(H, K, V) :- heap_size(H, S), retract(heap(H,S)), S1 is S + 1, assert(heap(H,S1)), assert(heap_entry(H, S1, K, V)), heapify(H, 1), !. 

list_heap(H) :- listing(heap_entry(H, P, K, V)).

head(H, K, V) :- heap_entry(H,1,K,V). 

extract(H, K, V) :- heap_entry(H, P, K, V), heap_size(H, S), S1 is S-1, retract(heap(H, S)), assert(heap(H, S1)), P1 is P+1, retract(heap_entry(H, P, K, V))	, adapt(H, P1), heapify(H, P1).

adapt(H, P) :- heap_size(H, S), P =< S, heap_entry(H, P, K, V), retract(heap_entry(H, P, K, V)), P1 is P-1, assert(heap_entry(H, P1, K, V)), P2 is P + 1, adapt(H, P2). 
adapt(H, P) :- heap_size(H, S), P>S.
modify_key(H, NewKey, OldKey, V) :- retract(heap_entry(H, P, OldKey, V)), assert(heap_entry(H, P, NewKey, V)). 

swap(H, K1, K2) :- retract(heap_entry(H, P1, K1, V1)), retract(heap_entry(H, P2, K2, V2)), assert(heap_entry(H, P2, K1, V1)), assert(heap_entry(H, P1, K2, V2)), heapify(H, 1).

delete_heap(H) :- retractall(heap_entry(H, _, _, _)), retract(heap(H, _)).





















?- new_heap(heap).
?- insert(heap, 10, a).
?- insert(heap, 20, b).
?- insert(heap, 7, c).
?- insert(heap, 5, d).
?- insert(heap, 1, e).























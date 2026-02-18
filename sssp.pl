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

list_vertices_2(G) :- 
			  		vertex(G, V),
		      		writeln(vertex(G, V)),
		      		fail. %predicato aggiuntivo che stampa solo i fatti tramite writeln

new_arc(G, U, V, Weight) :- arc(G, U, V, Weight), !.
new_arc(G, U, V, Weight) :- arc(G, V, U, Weight), !.
new_arc(G, U, V, Weight) :- 
	arc(G, U, V, K), K\=Weight, 
	retract(arc(G, U, V, K)), 
	new_arc(G, U, V, Weight), !.

new_arc(G, U, V, Weight) :- assert(arc(G, U, V, Weight)), !.

new_arc(G, U, V) :- new_arc(G, U, V, 1), !.

arcs(G, L) :- findall(arc(G, U, V, K), arc(G, U, V, K), L).

neighbors(G, V, Ln) :-
    vertex(G, V),
    arcs(G, La),
    findall(N, member(arc(G,V,N,_), La), Ln).

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


heapify(H, P) :- 		%caso heapify per nodo foglia
	Ps is P*2, 
	\+ heap_entry(H, Ps, _, _),
	Pd is Ps + 1, 
	\+ heap_entry(H, Pd, _, _).
	
heapify(H, P) :- 		%caso heapify per nodo con solo figlio sinistro senza scambio
	Ps is P*2, 
	heap_entry(H, Ps, Ks, Vs), 
	Pd is Ps + 1, 
	\+ heap_entry(H, Pd, Kd, Vd), 
	heap_entry(H, P, K, V), 
	heapify(H, Ps), 
	heap_entry(H, Ps, Ksn, Vsn),
	K =< Ksn.
	
heapify(H, P) :- 		%caso heapify per nodo con solo figlio sinistro con scambio
	Ps is P*2, 
	heap_entry(H, Ps, Ks, Vs), 
	Pd is Ps + 1, 
	\+ heap_entry(H, Pd, Kd, Vd), 
	heap_entry(H, P, K, V),
	heap_entry(H, Ps, Ksn, Vsn),
	K > Ksn, 
	swap(H, K, Ksn),
	heapify(H, Ps).
	
heapify(H, P) :- %caso heapify per nodo con entrambi i figli senza scambio
	
	Ps is P*2, 
	heap_entry(H, Ps, Ks, Vs), 
	Pd is Ps + 1, 
	heap_entry(H, Pd, Kd, Vd), 
	heap_entry(H, P, K, V), 
	heapify(H, Ps), 
	heapify(H, Pd), 
	heap_entry(H, Ps, Ksn, Vsn),
	heap_entry(H, Pd, Kdn, Vdn),
	K =< Ksn, 
	K =< Kdn.
	
heapify(H, P) :- 		%caso heapify per nodo con entrmabi i figli e scambio sul figlio sinistro
	Ps is P*2, 
	heap_entry(H, Ps, Ks, Vs), 
	Pd is Ps + 1, 
	heap_entry(H, Pd, Kd, Vd), 
	heap_entry(H, P, K, V), 
	heap_entry(H, Ps, Ksn, Vsn),
	heap_entry(H, Pd, Kdn, Vdn),
	K > Ksn, 
	swap(H, K, Ksn), 
	K =< Kdn,
	heapify(H, Ps), 
	heapify(H, Pd).
	
heapify(H, P) :- 		%caso heapify per nodo con entrmabi i figli e scambio sul figlio destro
	Ps is P*2, 
	heap_entry(H, Ps, Ks, Vs), 
	Pd is Ps + 1, 
	heap_entry(H, Pd, Kd, Vd), 
	heap_entry(H, P, K, V),
	heap_entry(H, Ps, Ksn, Vsn),
	heap_entry(H, Pd, Kdn, Vdn),
	K =< Ksn, 
	K > Kdn, 
	swap(H, K, Kdn),
	heapify(H, Ps), 
	heapify(H, Pd).
	
heapify(H, P) :- 
	Ps is P*2, 
	heap_entry(H, Ps, Ks, Vs), 
	Pd is Ps + 1, 
	heap_entry(H, Pd, Kd, Vd), 
	heap_entry(H, P, K, V),
	heap_entry(H, Ps, Ksn, Vsn),
	heap_entry(H, Pd, Kdn, Vdn),
	K > Ksn,
	K > Kdn,
	Kdn < Ksn,
	swap(H, K, Kdn),
	heapify(H, Ps), 
	heapify(H, Pd).
	
heapify(H, P) :- 
	Ps is P*2, 
	heap_entry(H, Ps, Ks, Vs), 
	Pd is Ps + 1, 
	heap_entry(H, Pd, Kd, Vd), 
	heap_entry(H, P, K, V),
	heap_entry(H, Ps, Ksn, Vsn),
	heap_entry(H, Pd, Kdn, Vdn),
	K > Ksn,
	K > Kdn, 
	Ksn < Kdn,
	swap(H, K, Ksn),
	heapify(H, Ps), 
	heapify(H, Pd).

insert(H, K, V) :- 
	heap_size(H, S), 
	retract(heap(H,S)), 
	S1 is S + 1, 
	assert(heap(H,S1)), 
	assert(heap_entry(H, S1, K, V)), 
	heapify(H, 1), !. 

list_heap(H) :- listing(heap_entry(H, P, K, V)).

head(H, K, V) :- heap_entry(H,1,K,V). 

extract(H, K, V) :- 
	heap_entry(H, P, K, V), 
	P = 1,
	heap_size(H, S), 
	S1 is S-1, 
	retract(heap(H, S)), 
	assert(heap(H, S1)), 
	P1 is P+1, 
	retract(heap_entry(H, P, K, V)), 
	\+ adapt(H, P1),
	heapify(H, 1),
	!.

adapt(H, P) :- 
	heap_size(H, S), 
	heap_entry(H, P, K, V), 
	P1 is P-1,
	P2 is P+1,
	P1>0,
	retract(heap_entry(H, P, K, V)), 
	assert(heap_entry(H, P1, K, V)),  
	adapt(H, P2).

modify_key(H, NewKey, OldKey, V) :- 
	retract(heap_entry(H, P, OldKey, V)), 
	assert(heap_entry(H, P, NewKey, V)). 

swap(H, K1, K2) :- 
	retract(heap_entry(H, P1, K1, V1)), 
	retract(heap_entry(H, P2, K2, V2)), 
	assert(heap_entry(H, P2, K1, V1)), 
	assert(heap_entry(H, P1, K2, V2)).

delete_heap(H) :- retractall(heap_entry(H, _, _, _)), retract(heap(H, _)).

:- dynamic distance/3.

:- dynamic visited/2.

:- dynamic previous/3.


change_distance(G, V, NewDist) :- 
	retractall(distance(G, V, _)),
	assert(distance(G, V; NewDist)).

change_previous(G, V, U) :-
	retractall(previous(G, V, _)),
	assert(previous(G, V, U)).
	
dijkstra_sssp(G, Source):-
	new_heap(dijkheap),
	neighbors(G, S, L1),
	source_weights(G, S, L1, dijkheap),
	visit(G, dijkheap),
	!.
	
	

source_weights(G, S, [], _).
source_weights(G, S, [H|T], Heap):- 
	arc(G, S, H, K),
	assert(distance(G, H, K)),
	insert(Heap, K, distance(G, H, K)),
	assert(previous(G, H, S)),
	source_weights(G, S, T, Heap).

visit(G, Heap) :- 
	empty(Heap).
	
visit(G, Heap) :-
	not_empty(Heap),
	extract(Heap, K, V),
	arg(2, V, N),
	assert(visited(G, N)),
	neighbors(G, N, Lv),
	node_weights(G, N, Lv, Heap, K),
	visit(G, Heap).

node_weights(G, N, [], _, K).	
	
node_weights(G, N, [H|T], Heap, Kp):- 
	\+ visited(G, H),
	arc(G, N, H, K),
	Ktot is K + Kp,
	assert(distance(G, H, Ktot)),
	assert(previous(G, H, N)),
	insert(Heap, Ktot, distance(G, H, Ktot)),
	node_weights(G, S, T, Heap, Kp).	
	
node_weights(G, N, [H|T], Heap, Kp):- 
	visited(G, H),
	arc(G, N, H, K),
	Ktot is K + Kp,
	distance(G, H, Kpre),
	Kpre > Ktot,
	change_distance(G, H, Ktot),
	change_previous(G, H, N),
	insert(Heap, Ktot, distance(G, H, Ktot)),
	node_weights(G, S, T, Heap, Kp).	
	
node_weights(G, N, [H|T], Heap, Kp):- 
	visited(G, H),
	arc(G, N, H, K),
	Ktot is K + Kp,
	distance(G, H, Kpre),
	Kpre< Ktot,
	node_weights(G, S, T, Heap, Kp).


?- new_graph(grafo).
?- new_vertex(grafo, a).
?- new_vertex(grafo, b).
?- new_vertex(grafo, c).
?- new_vertex(grafo, d).
?- new_vertex(grafo, e).
?- new_arc(grafo, a, b, 1).
?- new_arc(grafo, a, c, 3).
?- new_arc(grafo, b, d, 3).
?- new_arc(grafo, c, e, 1).
?- new_arc(grafo, e, d, 1).
?- new_arc(grafo, b, e, 5).

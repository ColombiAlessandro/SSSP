:- dynamic graph/1.

:- dynamic arc/4.

:- dynamic vertex/2.

new_graph(G) :- graph(G), !.				%inserisce il grafo nella base dati prolog se non presente
new_graph(G) :- assert(graph(G)), !.

new_vertex(G, V) :- vertex(G, V), !.		%inserisce il vertice V relativo al grafo G nella base dati Prolog se non già presente
new_vertex(G, V) :- assert(vertex(G, V)), !.

vertices(G, L) :-
    graph(G),
    findall(V, vertex(G, V), L). 

list_vertices(G) :- listing(vertex(G, K)).  %questo predicato con listing stampa sia regole che fatti

list_vertices_2(G) :- 
	vertex(G, V),
	writeln(vertex(G, V)),
	fail. %predicato aggiuntivo che stampa solo i fatti tramite writeln

new_arc(G, U, V, Weight) :- 
				graph(G),
				vertex(G, V),
				vertex(G, U),
				arc(G, U, V, Weight), !.		
new_arc(G, U, V, Weight) :- 
				graph(G),
				vertex(G, V),
				vertex(G, U),
				arc(G, V, U, Weight), !.		
new_arc(G, U, V, Weight) :- %se esiste già un percorso dal nodo U al nodo V il valore del peso viene aggiornato. 
	graph(G),
	vertex(G, V),
	vertex(G, U),
	arc(G, U, V, K), K\=Weight, 	
	retract(arc(G, U, V, K)), 
	new_arc(G, U, V, Weight), !.

new_arc(G, U, V, Weight) :- 
	graph(G),
	vertex(G, U),
	vertex(G, V),
	assert(arc(G, U, V, Weight)), !.		%inserisce un nuovo arco dal vertice U al vertice V del grafo G con peso Weight nella base dati Prolog

new_arc(G, U, V) :- 
	graph(G),
	vertex(G, U),
	vertex(G, V),
	new_arc(G, U, V, 1), !.

arcs(G, L) :- findall(arc(G, U, V, K), arc(G, U, V, K), L).			%restiuisce una lista di tutti gli archi del grafo G

neighbors(G, V, Ln) :-		%restituisce una lista di tutti e soli i vicini di un nodo V, ovvero tutti i nodi collegati direttamente a V
    vertex(G, V),
    arcs(G, La),
    findall(N, member(arc(G,V,N,_), La), Ln).

list_arcs(G) :- graph(G), listing(arc(G, U, V, J)).		%stampa a video tutti gli archi del grafo G 

list_graph(G) :- graph(G), list_arcs(G), list_vertices(G).		%stampa a video vertici e archi del grafo G

delete_graph(G) :- graph(G), retractall(vertex(G, _)), retractall(arc(G, _, _, _)), retract(graph(G)). 		%elimina tutti gli elementi del grafo G ed il grafo stesso

:- dynamic heap/2.
:- dynamic heap_entry/4.

new_heap(H) :- heap(H,_S), !.
new_heap(H) :- assert(heap(H, 0)), !.		%creazione di un nuovo heap nella base dati prolog

heap_size(H, S) :- heap(H, S).		%restituisce dimensione dello heap H

empty(H) :- heap(H,S), S = 0.		%predicato vero qwuando heap H è vuoto

not_empty(H) :- \+ empty(H).		%predicato vero quando heap H non è vuoto


heapify(H, P) :- %caso heapify per nodo foglia
	heap(H, _),
	Ps is P*2, 
	\+ heap_entry(H, Ps, _, _),
	Pd is Ps + 1, 
	\+ heap_entry(H, Pd, _, _).
	
heapify(H, P) :- 		%caso heapify per nodo con solo figlio sinistro senza scambio
	heap(H, _),
	Ps is P*2, 
	heap_entry(H, Ps, Ks, Vs), 
	Pd is Ps + 1, 
	\+ heap_entry(H, Pd, Kd, Vd), 
	heap_entry(H, P, K, V), 
	heapify(H, Ps), 
	heap_entry(H, Ps, Ksn, Vsn),
	K =< Ksn.
	
heapify(H, P) :- 		%caso heapify per nodo con solo figlio sinistro con scambio
	heap(H, _),
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
	heap(H, _),
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
	heap(H, _),
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
	heap(H, _),
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
	
heapify(H, P) :- 		%caso heapify per nodo con entrambi i figli, figlio destro minore del figlio sinistro, quindi scambio con il destro
	heap(H, _),
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
	
heapify(H, P) :- 		%caso heapify per nodo con entrmbi i figli, figlio sinistro minore del figlio destro, quindi scambio con il sinistro
	heap(H, _),
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

insert(H, K, V) :- 		%inseriemtno di un nuovo nodo nello heap, con aumento dimensione totale heap e heapify dello heap
	heap(H, _),
	heap_size(H, S), 
	retract(heap(H,S)), 
	S1 is S + 1, 
	assert(heap(H,S1)), 
	assert(heap_entry(H, S1, K, V)), 
	heapify(H, 1), !. 

list_heap(H) :- heap(H, _), listing(heap_entry(H, P, K, V)).		%elencazione di tutti i nodi dello heap

head(H, K, V) :- heap(H, _), heap_entry(H,1,K,V).		%visualizzazione della radice dello heap

extract(H, K, V) :- 		%estrazione della radice dallo heap, con dimiunzione della dimensione totale, adattamwnto delle posizioni dei nodi sottostanti e heapify dello heap
	heap(H, _),
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

adapt(H, P) :- 		%aggiustamento posizioni degli elementi colpiti dalla rimozione della radice
	heap(H, _),
	heap_size(H, S), 
	heap_entry(H, P, K, V), 
	P1 is P-1,
	P2 is P+1,
	P1>0,
	retract(heap_entry(H, P, K, V)), 
	assert(heap_entry(H, P1, K, V)),  
	adapt(H, P2).

modify_key(H, NewKey, OldKey, V) :- 
	heap(H, _),
	heap_entry(H, P, OldKey, V),
	retract(heap_entry(H, P, OldKey, V)), 
	assert(heap_entry(H, P, NewKey, V)). 

swap(H, K1, K2) :- 
	heap(H, _),
	retract(heap_entry(H, P1, K1, V1)), 
	retract(heap_entry(H, P2, K2, V2)), 
	assert(heap_entry(H, P2, K1, V1)), 
	assert(heap_entry(H, P1, K2, V2)).

delete_heap(H) :- heap(H, _), retractall(heap_entry(H, _, _, _)), retract(heap(H, _)).		%eliminazione di uno heap e di tutti i suoi nodi

:- dynamic distance/3.

:- dynamic visited/2.

:- dynamic previous/3.


change_distance(G, V, NewDist) :- %cambio distanza dalla sorgente
	graph(G),
	vertex(G, V),
	retract(distance(G, V, _)),
	assert(distance(G, V, NewDist)).

change_previous(G, V, U) :- %cambio nodo precdente nel cammino dalla radice
	graph(G),
	vertex(G, V),
	vertex(G, U),
	retract(previous(G, V, _)),
	assert(previous(G, V, U)).
	
dijkstra_sssp(G, Source):- 			%chiamata per applicare algorimo di Dijkstra, con creazione dello heap, elencazione dei vicini alla sorgente e loro distanze, e visita di tutto lo heap
	graph(G),
	vertex(G, Source),
	retractall(distance(G,_,_)),
	retractall(previous(G,_,_)),
	retractall(visited(G,_)),
	new_heap(dijkheap),
	assert(distance(G, Source, 0)),
	neighbors(G, Source, L1),
	source_weights(G, Source, L1, dijkheap),
	visit(G, dijkheap),
	!.
	
	

source_weights(G, S, [], _).
source_weights(G, S, [H|T], Heap):- 		%calcolo dei pesi di ogni nodo adiacente alla sorgente
	graph(G),
	vertex(G, S),
	arc(G, S, H, K),
	assert(distance(G, H, K)),
	insert(Heap, K, distance(G, H, K)),
	assert(visited(G, H)),
	assert(previous(G, H, S)),
	source_weights(G, S, T, Heap).

visit(G, Heap) :-		%termino la visita dello heap quando lo stesso è vuoto
	empty(Heap).
visit(G, Heap) :-		%finchè ci sono elementi nello heap estraggo la testa, calcolo i suoi vicini e il loro peso e ricorsivamente eseguo sugli altri vicini
	graph(G),
	not_empty(Heap),
	extract(Heap, K, V),
	arg(2, V, N),
	neighbors(G, N, Lv),
	node_weights(G, N, Lv, Heap, K),
	visit(G, Heap),
	!.

node_weights(G, N, [], _, K).	
node_weights(G, N, [H|T], Heap, Kp):- 		%se il nodo non è ancora stato visitato inserisco nello heap i dati, assumo diatanza e predecessore ed eseguo chiamata ricorsiva
	graph(G),
	vertex(G, N),
	\+ visited(G, H),
	assert(visited(G, H)),
	arc(G, N, H, K),
	Ktot is K + Kp,
	assert(distance(G, H, Ktot)),
	assert(previous(G, H, N)),
	insert(Heap, Ktot, distance(G, H, Ktot)),
	node_weights(G, N, T, Heap, Kp).
	
node_weights(G, N, [H|T], Heap, Kp):- 		%nel caso in cui la distanza calcolata precedentemente sia maggiore della distanza da percorrere con il nuovo nodo cambio la distanza dala radice e il predecessore, inserisco nuova entry nello heap e eseguo chiamata ricorsiva
	graph(G),
	vertex(G, N),
	visited(G, H),
	arc(G, N, H, K),
	Ktot is K + Kp,
	distance(G, H, Kpre),
	Kpre > Ktot,
	change_distance(G, H, Ktot),
	change_previous(G, H, N),
	insert(Heap, Ktot, distance(G, H, Ktot)),
	node_weights(G, N, T, Heap, Kp).
	
node_weights(G, N, [H|T], Heap, Kp):- 		%nel caso in cui la distanza calcolata precedentemente sia minore della distanza da percorrere con il nuovo nodo non modifico nulla ed eseguo la chiamata ricorsiva
	graph(G),
	vertex(G, N),
	visited(G, H),
	arc(G, N, H, K),
	Ktot is K + Kp,
	distance(G, H, Kpre),
	Kpre< Ktot,
	node_weights(G, N, T, Heap, Kp).

shortest_path(G, Source, V, Path):-
	graph(G),
	vertex(G, Source),
	vertex(G, V),
	path(G, Source, V, List),
	reverse(List, Path),
	!.
	
path(G, Source, Source, []). 

path(G, Source, V, [H|T]):- 
	graph(G),
	vertex(G, Source),
	vertex(G, V),
	previous(G, V, Pred),
	arc(G, Pred, V, K),
	H = arc(G, Pred, V, K),
	path(G, Source, Pred, T).
	


?- new_graph(grafo).
?- new_vertex(grafo, a).
?- new_vertex(grafo, b).
?- new_vertex(grafo, c).
?- new_vertex(grafo, d).
?- new_vertex(grafo, e).
?- new_vertex(grafo, f).
?- new_vertex(grafo, g).
?- new_vertex(grafo, h).
?- new_vertex(grafo, i).
?- new_arc(grafo, a, b, 1).
?- new_arc(grafo, a, c, 3).
?- new_arc(grafo, b, d, 3).
?- new_arc(grafo, c, e, 1).
?- new_arc(grafo, e, d, 1).
?- new_arc(grafo, f, e, 5).
?- new_arc(grafo, g, e, 1).
?- new_arc(grafo, h, d, 1).
?- new_arc(grafo, i, e, 5).
?- new_arc(grafo, e, f, 1).
?- new_arc(grafo, f, g, 1).
?- new_arc(grafo, a, i, 5).

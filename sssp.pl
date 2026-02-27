:- dynamic graph/1.

:- dynamic arc/4.

:- dynamic vertex/2.

%inserisce il grafo nella base dati prolog se non presente
new_graph(G) :- graph(G), !.    
new_graph(G) :- assert(graph(G)), !.

%inserisce il vertice V relativo al grafo G nella base dati Prolog se non già presente
new_vertex(G, V) :- vertex(G, V), !.            
new_vertex(G, V) :- assert(vertex(G, V)), !.

vertices(G, L) :-
  graph(G),
  findall(V, vertex(G, V), L). 

list_vertices(G) :- 
  listing(vertex(G, K)).  %questo predicato con listing stampa sia regole che fatti

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

%se esiste già un percorso da U a V il valore del peso viene aggiornato.
new_arc(G, U, V, Weight) :- 
  graph(G),
  vertex(G, V),
  vertex(G, U),
  arc(G, U, V, K), K\=Weight,         
  retract(arc(G, U, V, K)), 
  new_arc(G, U, V, Weight), !.

%crea un nuovo arco da U a V del grafo G con peso Weight nella base dati Prolog
new_arc(G, U, V, Weight) :- 
  graph(G),
  vertex(G, U),
  vertex(G, V),
  assert(arc(G, U, V, Weight)), !.    

%se il peso non è definito mette valore default 1
new_arc(G, U, V) :- 
  graph(G),
  vertex(G, U),
  vertex(G, V),
  new_arc(G, U, V, 1), !.

%restituisce una lista di tutti gli archi del grafo G
arcs(G, L) :-
  findall(arc(G, U, V, K), arc(G, U, V, K), L).  

%restituisce una lista di tutti e soli i vicini di un nodo V
neighbors(G, V, Ln) :-  
  vertex(G, V),
  arcs(G, La),
  findall(N, member(arc(G,V,N,_), La), Ln).

%stampa a video tutti gli archi del grafo G 
list_arcs(G) :-
  graph(G),
  listing(arc(G, U, V, J)).           

%stampa a video vertici e archi del grafo G
list_graph(G) :-
  graph(G),
  list_arcs(G),
  list_vertices(G).           

%elimina tutti gli elementi del grafo G ed il grafo stesso
delete_graph(G) :-
  graph(G),
  retractall(vertex(G, _)),
  retractall(arc(G, _, _, _)),
  retract(graph(G)).          

:- dynamic heap/2.
:- dynamic heap_entry/4.

%creazione di un nuovo heap nella base dati prolog
new_heap(H) :- heap(H,_S), !.
new_heap(H) :- assert(heap(H, 0)), !.           

%restituisce dimensione dello heap H
heap_size(H, S) :- heap(H, S).          

%predicato vero quando heap H è vuoto
empty(H) :- heap(H,S), S = 0.           

%predicato vero quando heap H non è vuoto
not_empty(H) :- \+ empty(H).           

%caso heapify per nodo foglia
heapify(H, P) :- 
  heap(H, _),
  Ps is P*2, 
  \+ heap_entry(H, Ps, _, _),
  Pd is Ps + 1, 
  \+ heap_entry(H, Pd, _, _).

%caso heapify per nodo con solo figlio sinistro senza scambio
heapify(H, P) :-                
  heap(H, _),
  Ps is P*2, 
  heap_entry(H, Ps, Ks, Vs), 
  Pd is Ps + 1, 
  \+ heap_entry(H, Pd, Kd, Vd), 
  heap_entry(H, P, K, V), 
  heapify(H, Ps), 
  heap_entry(H, Ps, Ksn, Vsn),
  K =< Ksn.

%caso heapify per nodo con solo figlio sinistro con scambio
heapify(H, P) :-               
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

%caso heapify per nodo con entrambi i figli senza scambio
heapify(H, P) :- 
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

%caso heapify per nodo con entrambi i figli e scambio sul figlio sinistro
heapify(H, P) :-               
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

%caso heapify per nodo con entrambi i figli e scambio sul figlio destro
heapify(H, P) :-                
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

%caso heapify per nodo con entrambi i figli, figlio destro minore del figlio sinistro, quindi scambio con il destro
heapify(H, P) :-                
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

%caso heapify per nodo con entrambi i figli, figlio sinistro minore del figlio destro, quindi scambio con il sinistro
heapify(H, P) :-                
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

%inserimento di un nuovo nodo nello heap, con aumento dimensione totale heap e heapify dello heap
insert(H, K, V) :-             
  heap(H, _),
  heap_size(H, S), 
  retract(heap(H,S)), 
  S1 is S + 1, 
  assert(heap(H,S1)), 
  assert(heap_entry(H, S1, K, V)), 
  heapify(H, 1), !. 

%elencazione di tutti i nodi dello heap
list_heap(H) :- heap(H, _), listing(heap_entry(H, P, K, V)).            

%consultazione della radice dello heap
head(H, K, V) :- heap(H, _), heap_entry(H,1,K,V).               

%estrazione della radice dallo heap, diminuzione della dimensione, adattamento delle posizioni e heapify dello heap
extract(H, K, V) :-             
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

%aggiustamento posizioni degli elementi colpiti dalla rimozione della radice
adapt(H, P) :-  
  heap(H, _),
  heap_size(H, S), 
  heap_entry(H, P, K, V), 
  P1 is P-1,
  P2 is P+1,
  P1>0,
  retract(heap_entry(H, P, K, V)), 
  assert(heap_entry(H, P1, K, V)),  
  adapt(H, P2).

%sostituzione della chiave nella entry dello heap
modify_key(H, NewKey, OldKey, V) :- 
  heap(H, _),
  heap_entry(H, P, OldKey, V),
  retract(heap_entry(H, P, OldKey, V)), 
  assert(heap_entry(H, P, NewKey, V)). 

%scambio due entry dello heap
swap(H, K1, K2) :- 
  heap(H, _),
  retract(heap_entry(H, P1, K1, V1)), 
  retract(heap_entry(H, P2, K2, V2)), 
  assert(heap_entry(H, P2, K1, V1)), 
  assert(heap_entry(H, P1, K2, V2)).

%eliminazione di uno heap e di tutti i suoi nodi
delete_heap(H) :- heap(H, _), retractall(heap_entry(H, _, _, _)), retract(heap(H, _)).         

:- dynamic distance/3.

:- dynamic visited/2.

:- dynamic previous/3.

%cambio distanza dalla sorgente
change_distance(G, V, NewDist) :- 
  graph(G),
  vertex(G, V),
  retract(distance(G, V, _)),
  assert(distance(G, V, NewDist)).

%cambio nodo precedente nel cammino dalla radice
change_previous(G, V, U) :- 
  graph(G),
  vertex(G, V),
  vertex(G, U),
  retract(previous(G, V, _)),
  assert(previous(G, V, U)).

%Applica algoritmo di Dijkstra: creazione dello heap, elencazione dei vicini alla sorgente e distanze, e visita dello heap
dijkstra_sssp(G, Source):-      
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

%calcolo dei pesi di ogni nodo adiacente alla sorgente
source_weights(G, S, [], _).
source_weights(G, S, [H|T], Heap):-     
  graph(G),
  vertex(G, S),
  arc(G, S, H, K),
  assert(distance(G, H, K)),
  insert(Heap, K, distance(G, H, K)),
  assert(visited(G, H)),
  assert(previous(G, H, S)),
  source_weights(G, S, T, Heap).

%termino la visita dello heap quando lo stesso è vuoto
visit(G, Heap) :-       
  empty(Heap).
%finchè heap non vuoto estraggo la testa, calcolo i suoi vicini e il loro peso e ricorsivamente eseguo sugli altri vicini
visit(G, Heap) :-       
  graph(G),
  not_empty(Heap),
  extract(Heap, K, V),
  arg(2, V, N),
  neighbors(G, N, Lv),
  node_weights(G, N, Lv, Heap, K),
  visit(G, Heap),
  !.

node_weights(G, N, [], _, K).   
%se il nodo non è ancora stato visitato inserisco nello heap i dati, annoto distanza e precedente ed eseguo chiamata ricorsiva
node_weights(G, N, [H|T], Heap, Kp):-   
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

%Se la distanza calcolata precedentemente è maggiore della distanza da percorrere cambio la distanza dalla radice e il precedente, inserisco nuova entry nello heap e eseguo chiamata ricorsiva
node_weights(G, N, [H|T], Heap, Kp):-   
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

%Se la distanza calcolata precedentemente è minore della distanza da percorrere con il nuovo nodo non modifico nulla ed eseguo la chiamata ricorsiva
node_weights(G, N, [H|T], Heap, Kp):-   
  graph(G),
  vertex(G, N),
  visited(G, H),
  arc(G, N, H, K),
  Ktot is K + Kp,
  distance(G, H, Kpre),
  Kpre< Ktot,
  node_weights(G, N, T, Heap, Kp).

%chiamata che restituisce in Path il cammino minimo da Source a V
shortest_path(G, Source, V, Path):-
  graph(G),
  vertex(G, Source),
  vertex(G, V),
  path(G, Source, V, List),
  reverse(List, Path),
  !.

%Se i due nodi sono uguali non faccio nulla
path(G, Source, Source, []). 

%Se i nodi sono diversi assegno ricorsivamente l'arco alla testa della lista
path(G, Source, V, [H|T]):- 
  graph(G),
  vertex(G, Source),
  vertex(G, V),
  previous(G, V, Pred),
  arc(G, Pred, V, K),
  H = arc(G, Pred, V, K),
  path(G, Source, Pred, T).

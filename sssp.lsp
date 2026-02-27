;Colombi Alessandro, mat. XXXXXX
;Garancini Andrea, mat. 928256
;Gaspani Sebastiano, mat. XXXXXX

;;; definizione tabelle hash
(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))

;;funzione is-graph se il grafo esiste lo restituisce, altrimenti restituisce NIL
(defun is-graph (graph-id)
  (gethash graph-id *graphs*))

;;creo il grafo se non esiste, altrimenti restituisco il suo identificatore
(defun new-graph (graph-id)
  (or 	(gethash graph-id *graphs*)
    (setf (gethash graph-id *graphs*) graph-id)))

;;rimuovo dalle tabelle hash tutti i riferimenti al grafo passato per parametro
(defun delete-graph (graph-id)
  (when (null (nth-value 1 (gethash graph-id *graphs*)))
    (error "Il grafo non esiste"))
  (remhash graph-id *graphs*)
  
  (maphash (lambda (key value)
             (when (string= graph-id (second key))
               (remhash key *vertices*)))
    *vertices*)
  
  (maphash (lambda (key value)
             (when (equal graph-id (second key))
               (remhash key *arcs*)))
    *arcs*))

;;creo nuovo vertice per il grafo passato come parametro
(defun new-vertex (graph-id vertex-id)
  (when (null (nth-value 1 (gethash graph-id *graphs*)))
    (error "Il grafo non esiste"))
  (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
    (list 'vertex graph-id vertex-id)))

;;metto i vertici del grafo in una lista e la restituisco
(defun graph-vertices (graph-id)
  (when (null (nth-value 1 (gethash graph-id *graphs*)))
    (error "Il grafo non esiste"))
  (let ((lista nil))
    (maphash (lambda (key value)
               (let ((tmp (second key)))
                 (when (equal tmp graph-id)
                   (push key lista))))
      *vertices*)
    lista)) 


;;creo arco che collega i vertici del grafo passati come parametro ed il suo peso
(defun new-arc (graph-id vertex-id vertex2-id &optional (weight 1)) 
  (when (or (null (nth-value 1 (gethash graph-id *graphs*)))
	  (null (nth-value 1 (gethash (list 'vertex graph-id vertex-id) *vertices*)))
          (null (nth-value 1 (gethash (list 'vertex graph-id vertex2-id) *vertices*))))
    (error "Errore, elementi non presenti"))
  (setf (gethash (list 'arc graph-id vertex-id vertex2-id weight) *arcs*)
    (list 'arc graph-id vertex-id vertex2-id weight)))

;;;metto gli archi del grafo in una lista e la restituisco
(defun graph-arcs (graph-id)
  (when (null (nth-value 1 (gethash graph-id *graphs*)))
    (error "Il grafo non esiste"))
  (let ((lista nil))
    (maphash (lambda (key value)
               (let ((tmp (second key)))
                 (when (equal tmp graph-id)
                   (push key lista))))
      *arcs*)
    lista))

;;;restituisce la lista degli archi uscenti
(defun graph-vertex-neighbors (graph-id vertex-id)
  (when (or (null (nth-value 1 (gethash graph-id *graphs*)))
          (null (nth-value 1 (gethash (list 'vertex graph-id vertex-id) *vertices*))))
    (error "Errore, elementi non presenti"))
  (let ((lista nil))
    (maphash (lambda (key value)
               (let ((tmp-graph (second key))
                      (tmp-vertex (third key)))
                 (when (and (equal tmp-graph graph-id)
                         (equal tmp-vertex vertex-id))
                   (push key lista))))
      *arcs*)
    lista))

;;metto in una lista vertici e archi di un grafo e la restituisco
(defun graph-print (graph-id)
  (when (null (nth-value 1 (gethash graph-id *graphs*)))
    (error "Grafo non presente"))
  (let((lista nil)) 
    (push (graph-vertices graph-id) lista)
    (push (graph-arcs graph-id) lista)
    lista))

;;creo un nuovo heap se non presente, imposto size a 0 e creo array aggiustabile
(defun new-heap (heap-id &optional (initial-capacity 1))
  (or (gethash heap-id *heaps*)
    (setf (gethash heap-id *heaps*)
      (list 'heap heap-id 0
	(make-array initial-capacity :adjustable t)))))		

;;rimuovo heap dalla hash table
(defun heap-delete (heap-id)
  (when (null (nth-value 1 (gethash heap-id *heaps*)))
    (error "Heap non presente"))
  (remhash heap-id *heaps*))

;;se la size dello heap è 0 vuol dire che è vuoto
(defun heap-empty (heap-id)
  (when (null (nth-value 1 (gethash heap-id *heaps*)))
    (error "Heap non presente"))
  (= 0 (third (gethash heap-id *heaps*))))

;;se la size non è 0 vuol dire che è non vuoto
(defun heap-not-empty (heap-id)
  (when (null (nth-value 1 (gethash heap-id *heaps*)))
    (error "Heap non presente"))
  (not (heap-empty heap-id)))

;;funzione per inserimento elemento in heap
(defun heap-insert (heap-id K V)
  (when (null (nth-value 1 (gethash heap-id *heaps*)))
    (error "Heap non presente"))
  (let ((value (gethash heap-id *heaps*)))
    (when value	
      (let ((size  (third value))
             (array (fourth value)))

        (when (>= size (length array))  ;se size > dimensione array allargo di una cella
          (setf array (adjust-array array (+ 1 (length array))))
          (setf (fourth value) array))	;assegno array allungato alla stessa variabile precedente

        (setf (aref array size) (list K V))	;inserisco in ultima posizione il nuovo elemento
        (incf (third value))			;incremento valore di size
        (heapify-up array size)))))		;ripristino struttura heap

;;funzione ricorsiva che sistema ordine del minheap dopo inserimento
(defun heapify-up (heap i)
  (when (> i 0)		;se non sono alla radice faccio heapify
    (let* ((parent (floor (- i 1) 2))
            (current (aref heap i))
            (par (aref heap parent)))
      (when (< (first current) (first par))	;scmabio e faccio heapify se chiave figlio < chiave genitore
        (rotatef (aref heap i) (aref heap parent))
        (heapify-up heap parent)))))

;;heapify down usata per extract radice per sistemare heap
(defun heapify (heap i heap-id)	
  (let* ((len (third (gethash heap-id *heaps*)))
          (posMin i)	
          (l (+ (* 2 i) 1))
          (r (+ (* 2 i) 2)))
    (when (< l len)
      (when (< (first (aref heap l))
              (first (aref heap posMin)))
        (setf posMin l)))
    (when (< r len)
      (when (< (first (aref heap r))
              (first (aref heap posMin)))
        (setf posMin r)))
    (when (/= i posMin)
      (rotatef (aref heap i) (aref heap posMin))
      (heapify heap posMin heap-id))))

;;funzione che stampa contenuto della hash table per un heap dato
(defun heap-print (heap-id)
  (when (null (nth-value 1 (gethash heap-id *heaps*)))
    (error "Heap non presente"))
  (gethash heap-id *heaps*))

;;restituisce valore della radice dello heap
(defun heap-head (heap-id)
  (when (null (nth-value 1 (gethash heap-id *heaps*)))
    (error "Heap non presente"))
  (setf tmp (gethash heap-id *heaps*))
  (setf arr (fourth tmp))
  (aref arr 0))

;;funzione che estrae la radice e la restituisce
(defun heap-extract (heap-id)
  (when (null (nth-value 1 (gethash heap-id *heaps*)))
    (error "Heap non presente"))
  (let* ((tmp (gethash heap-id *heaps*))
          (arr (fourth tmp))
          (size (third tmp)))
    (let ((root (aref arr 0)))
      (setf (aref arr 0) (aref arr (1- size)))
      (decf (third tmp))	;decremento size dello heap
      (setf (fourth tmp) (adjust-array (fourth tmp) (third tmp))) ;Riduco dimensione array
      (heapify arr 0 heap-id)	;heapify per sistemare heap
      root)))	;restituzione elemento estratto

;;funzione ricorsiva che cerca la chiave di un elemento nello heap e la modifica 
(defun heap-modify-key (heap-id new-key old-key value)
  (when (null (nth-value 1 (gethash heap-id *heaps*)))
    (error "Heap non presente"))
  (let* ((heap (gethash heap-id *heaps*))
          (arr  (fourth heap))
          (size (third heap)))
    (ricerca-e-modifica heap-id arr size new-key old-key value 0)))

(defun ricerca-e-modifica (heap-id arr size new-key old-key value i)
  (when (< i size)	;se indice è corretto
    (let ((current (aref arr i)))
      (if (and (= (first current) old-key)	;se la chiave è quella che sto cercando
            (eq (second current) value))	; e anche il valore è corretto
        (progn	;restituisce ultimo valore al suo interno
          (setf (first current) new-key)	;cambio chiave
          (if (< new-key old-key)
            (heapify-up arr i)	;faccio heapify up o down a seconda dei casi
            (heapify arr i heap-id))
          t); restituisco T
        (ricerca-e-modifica heap-id arr size new-key old-key value (1+ i))))))	;chiamata ricorsiva

;;restituisco distanza presente nella tabella hash
(defun sssp-dist (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *distances*))	

;;setto la nuova distanza nella tabella hash
(defun sssp-change-dist (graph-id vertex-id new-dist)
  (setf (gethash (list graph-id vertex-id) *distances*) new-dist)
  nil)	

;;restituisco T se nodo visitato e NIL se non visitato
(defun sssp-visited (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *visited*))	

;;restituisco il predecessore del nodo passato come parametro
(defun sssp-previous (graph-id V)
  (gethash (list graph-id V) *previous*)) 

;;modifico il predecessore nella tabella hash
(defun sssp-change-previous (graph-id V U)
  (setf (gethash (list graph-id V) *previous*) U)
  nil)	

;;segno nodo come visitato
(defun sssp-set-visited (graph-id vertex-id)
  (setf (gethash (list graph-id vertex-id) *visited*) t)
  nil)	

;;funzione che resetta le strutture dati e assegna i valori iniziali per Dijkstra
(defun sssp-init (graph-id source) 
  (when (null (nth-value 1 (gethash graph-id *graphs*)))
    (error "Grafo non presente"))
  (when (null (nth-value 1 (gethash (list 'vertex graph-id source) *vertices*)))
    (error "Vertice non presente"))
  (clrhash *distances*)
  (clrhash *previous*)
  (clrhash *visited*)
  (mapc (lambda (v)	;applico funzione su ogni elemento della lista ed ignoro risultato
          (let ((vertex (third v)))
            (sssp-change-dist graph-id vertex most-positive-fixnum) ;uso costante come distanza infinita
            (setf (gethash (list 'vertex graph-id vertex) *visited*) nil)))
    (graph-vertices graph-id))
  (sssp-change-dist graph-id source 0))	;imposto distanza della sorgente da sè stessa a 0

;;funzione che esegue Dijkstra
(defun sssp-dijkstra (graph-id source)
  (when (null (nth-value 1 (gethash graph-id *graphs*)))
    (error "Grafo non presente"))
  (when (null (nth-value 1 (gethash (list 'vertex graph-id source) *vertices*)))
    (error "Vertice non presente"))
  (sssp-init graph-id source)
  (let ((heap-id 'dijkstra-heap))
    (new-heap heap-id)	;creo heap per Dijkstra
    (heap-insert heap-id 0 source)	;inserisco nello heap la sorgente con chiave 0
    (dijkstra-visit graph-id heap-id)	;chiamo la funzione per la visita
    (heap-delete heap-id)	;alla fine della computazione, avendo i dati nelle tabelle hash, elimino lo heap
    nil))

;;funzione che visita il grafo
(defun dijkstra-visit (graph-id heap-id)
  (when (null (nth-value 1 (gethash graph-id *graphs*)))
    (error "Grafo non presente"))
  (when (null (nth-value 1 (gethash heap-id *heaps*)))
    (error "Heap non presente"))
  (unless (heap-empty heap-id)	;se lo heap non è vuoto proseguo
    (let* ((entry (heap-extract heap-id)) ;creo variabili locali
            (dist (first entry))
            (u    (second entry)))
      (unless (sssp-visited graph-id u)	;se il nodo non è ancora visitato lo visito
        (sssp-set-visited graph-id u)
        (aggiorna graph-id u dist heap-id))) ;chiamo funzione che aggiorna le distanze nello heap e nelle tabelle hash
    (dijkstra-visit graph-id heap-id))) ;richiamo ricorsivamente la funzione sul resto dello heap

;;funzione che aggiornai valori nello heap e nelle hash tables
(defun aggiorna (graph-id u dist heap-id)
  (when (null (nth-value 1 (gethash graph-id *graphs*)))
    (error "Grafo non presente"))
  (when (null (nth-value 1 (gethash heap-id *heaps*)))
    (error "Heap non presente"))
  (mapc 
    (lambda (arc)
      (let* ((v (fourth arc))
              (w (fifth arc))
              (alt (+ dist w))	;calcolo della nuova distanza tramite il nuovo percorso
              (old (sssp-dist graph-id v)))
	(when (and (not (sssp-visited graph-id v))
                (< alt old)) ;se il nodo non è visitato e la distanza passando per il nuovo vertice è minore aggiorno le tabelle hash e re-inserisco il nodo nello heap
          (sssp-change-dist graph-id v alt)
          (sssp-change-previous graph-id v u)
          (heap-insert heap-id alt v))))   (graph-vertex-neighbors graph-id u)))

;;funzione che restituisce la lista di nodi che formano lo shortest path
(defun path (graph-id source v) 
  (when (null (nth-value 1 (gethash graph-id *graphs*)))
    (error "Grafo non presente"))
  (when (null (nth-value 1 (gethash (list 'vertex graph-id source) *vertices*)))
    (error "Sorgente non presente"))
  (if (null v)
    nil
    (if (equal v source) 
      (list source)
      (append (path graph-id
                source
                (sssp-previous graph-id v))
        (list v))))) ;restituisco la lista finita

;;funzione che calcola il percorso minimo tra due nodi
(defun sssp-shortest-path (graph-id source target)
  (when (null (nth-value 1 (gethash graph-id *graphs*)))
    (error "Grafo non presente"))
  (when (null (nth-value 1 (gethash (list 'vertex graph-id source) *vertices*)))
    (error "Sorgente non corretta"))
  (when (null (nth-value 1 (gethash (list 'vertex graph-id target) *vertices*)))
    (error "Destinazione non corretta"))
  (path-to-arcs graph-id (path graph-id source target) (graph-arcs graph-id)))

;;funzione che converte da nodi ad archi
(defun path-to-arcs (graph-id vertices arcs) 
  (cond
    ((or (null vertices) (null (cdr vertices)))
      nil) ;se ho 0 o 1 nodo non ho archi e restituisco nil
    (t
      (cons (find-arc graph-id (car vertices) (cadr vertices) arcs) ;concateno l'arco e chiamo ricorsivamente la funzione
	(path-to-arcs graph-id (cdr vertices) arcs)))))

;;funzione che cerca arco in hash table
(defun find-arc (graph-id u v arcs)
  (cond
    ((null arcs) nil) ;se non ho archi restituisco NIL
    ((and (equal (second (car arcs)) graph-id)
       (equal (third (car arcs)) u)
       (equal (fourth (car arcs)) v))
      (car arcs))
    (t (find-arc graph-id u v (cdr arcs))))) ;se i dati sono corretti richiamo la funzione sul resto della lista

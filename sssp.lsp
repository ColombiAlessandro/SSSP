
; definizione tabelle hash
(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))

;funzione is-graph se il grafo esiste lo restituisce, altrimenti restituisce NIL
(defun is-graph (graph-id)
	(gethash graph-id *graphs*))
	
;creo il grafo se non esiste, altirmenti lo restituisco
(defun new-graph (graph-id)
	(or 	(gethash graph-id *graphs*)
		(setf (gethash graph-id *graphs*) graph-id)))

;rimuovo dalle tabelle hash tutti i riferimenti al grafo passato per parametro
(defun delete-graph (graph-id)
  (remhash graph-id *graphs*)
  
  (maphash (lambda (key value)		;rimuovo vertici
             (when (string= graph-id (second key))
               (remhash key *vertices*)))
           *vertices*)
  
  (maphash (lambda (key value)		;rimuovo archi
             (when (string= graph-id (second key))
               (remhash key *arcs*)))
           *arcs*))
	
;creo nuovo vertice per il grafo passato come parametro
(defun new-vertex (graph-id vertex-id)
	(setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
	(list 'vertex graph-id vertex-id)))
	
;metto i vertici del grafo in una lista e la restituisco
(defun graph-vertices (graph-id)
  (let ((lista nil))
    (maphash (lambda (key value)
               (let ((tmp (second key)))
                 (when (string= tmp graph-id)
                   (push key lista))))
             *vertices*)
    lista)) 
    

;creo arco che collega i vertici del grafo passati come parametro ed il suo peso
(defun new-arc (graph-id vertex-id vertex2-id &optional (weight 1)) 
	(setf (gethash (list 'arc graph-id vertex-id vertex2-id weight) *arcs*)
	(list 'arc graph-id vertex-id vertex2-id weight)))

;metto gli archi del grafo in una lista e la restituisco
(defun graph-arcs (graph-id)
  (let ((lista nil))
    (maphash (lambda (key value)
               (let ((tmp (second key)))
                 (when (string= tmp graph-id)
                   (push key lista))))
             *arcs*)
    lista))

;calcolo i vicini del nodo passato, li metto in una lista e li restituisco
(defun graph-vertex-neighbors (graph-id vertex-id)
  (let ((lista nil))
    (maphash (lambda (key value)
               (let ((tmp-graph (second key))
                     (tmp-vertex (third key)))
                 (when (and (string= tmp-graph graph-id)
                            (string= tmp-vertex vertex-id))
                   (push key lista))))
             *arcs*)
    lista))
    
;metto in una lista vertici e archi di una grafo e la restiuisco
(defun graph-print (graph-id)
	(let((lista nil)) 
		(push (graph-vertices graph-id) lista)
		(push (graph-arcs graph-id) lista)
		lista))
		
;creo un nuovo heap se non persente, imposto size a 0 e creo array aggiustabile
(defun new-heap (heap-id &optional (initial-capacity 1))
	(or (gethash heap-id *heaps*)
	    (setf (gethash heap-id *heaps*)
	    (list 'heap heap-id 0
	    (make-array initial-capacity :adjustable t)))))		
		
;rimuovo heap dlala hash table
(defun heap-delete (heap-id)
	(remhash heap-id *heaps*))
	
;se la size dello heap è 0 vuol dire che è vuoto
(defun heap-empty (heap-id)
	(= 0 (third (gethash heap-id *heaps*))))
	
;se la size non è 0 vuol dire che è non vuoto
(defun heap-not-empty (heap-id)
	(not (heap-empty heap-id)))

;funzione per inserimento elemento in heap
(defun heap-insert (heap-id K V)
  (let ((value (gethash heap-id *heaps*)))
    (when value		;faccio il seguito se esiste value
      (let ((size  (third value))
            (array (fourth value)))

        (when (>= size (length array))  ;se size ha superato la dimensione dell'array allargo array di una cella per permettere inserimento
          (setf array (adjust-array array (+ 1 (length array))))
          (setf (fourth value) array))	;assegno array allungato alla quarta posizione della descrizione dello heap

        (setf (aref array size) (list K V))	;inserisco in ultima posizione il nuovo elemento come lista formata dalla coppia chiave valore
        (incf (third value))			;incremento valore di size per tenere traccia degli elementi nello heap

        (heapify-up array size)))))		;sistemo heap portando in alto il nuovo elemento
   
;funzione per gestire heap
(defun heapify-up (heap i)
  (when (> i 0)		;se non sono alla radice faccio heapify
    (let* ((parent (floor (- i 1) 2))
           (current (aref heap i))
           (par (aref heap parent))) ;calcolo indici degli elementi
      (when (< (first current) (first par))	;se la chiave dell'elemento puntato è minore della chiave del genitore li scambio e faccio heapify a partire dal valore scambiato
        (rotatef (aref heap i) (aref heap parent))
        (heapify-up heap parent)))))
        
	
(defun heapify (heap i heap-id)	;heapify down usata per extract radice per sistemare heap
  (let* ((len (third (gethash heap-id *heaps*)))	;calcolo indici utili
         (posMin i)	;parto a considerare il minimo valore nella radice
         (l (+ (* 2 i) 1))
         (r (+ (* 2 i) 2)))
	;salvo il valore minore tra i due figli
    (when (< l len)
      (when (< (first (aref heap l))
               (first (aref heap posMin)))
        (setf posMin l)))

    (when (< r len)
      (when (< (first (aref heap r))
               (first (aref heap posMin)))
        (setf posMin r)))
;se ho fatto un salvataggio scambio la radice con il minimo e faccio heapify a partire dalla posizione in cui ho scambiato
    (when (/= i posMin)
      (rotatef (aref heap i) (aref heap posMin))
      (heapify heap posMin heap-id))))
     
;funzone che stampa contenuto della hash table per un heap dato
(defun heap-print (heap-id)
	(gethash heap-id *heaps*))
	
(defun heap-head (heap-id)
	(setf tmp (gethash heap-id *heaps*))
	(setf arr (fourth tmp))
	(aref arr 0)); essendo ultimo valore valutato CLISP lo restiuisce in automatico

(defun heap-extract (heap-id)
;inizializzo variabili utili per la funzione
  (let* ((tmp (gethash heap-id *heaps*))
         (arr (fourth tmp))
         (size (third tmp)))
    (let ((root (aref arr 0)))	;salvo localmente il valore estratto
      (setf (aref arr 0) (aref arr (1- size)))	;prendo ultimo elemento dello heap e lo metto al posro della radice
      (decf (third tmp))	;decremento size dello heap
      (setf (fourth tmp) (adjust-array (fourth tmp) (third tmp)))	;allineo dimensione array con size dello heap (elimino ultimo valore inutile dell'array)
      (heapify arr 0 heap-id)	;heapify per sistemare heap
      root)))	;restituzione elemento estratto
      

      
      
(defun heap-modify-key (heap-id new-key old-key value)
;inizializzo variabili utili alla funzione
  (let* ((heap (gethash heap-id *heaps*))
         (arr  (fourth heap))
         (size (third heap)))
    (ricerca-e-modifica heap-id arr size new-key old-key value 0)))	;chiamo funzione ricorsiva che cerca la chiave e la cambia partendo dalla radice

(defun ricerca-e-modifica (heap-id arr size new-key old-key value i)
  (when (< i size)	;se indice è corretto
    (let ((current (aref arr i)))
      (if (and (= (first current) old-key)	;se la chiave è quella che sto cercando
               (eq (second current) value))	; e anche il valore è corretto
          (progn	;comando che serve per dire a CLISP di restituire ultimo valore al suo interno
            (setf (first current) new-key)	;cambio chiave
            (if (< new-key old-key)
                (heapify-up arr i)	;se la nuova chiave è minore della precedente devo spostare la nuova chiave verso l'alto, altrimenti verso il basso
                (heapify arr i heap-id))
            t); restituisco T
          (ricerca-e-modifica heap-id arr size new-key old-key value (1+ i))))))	;se non ho trovato chiave chiamo ricordsivamente la funzione sul prossimo elemento
          
          
        
(new-graph 'grafo)
(new-vertex 'grafo 'a)
(new-vertex 'grafo 'b)
(new-vertex 'grafo 'c)
(new-vertex 'grafo 'd)
(new-vertex 'grafo 'e)
(new-arc 'grafo 'a 'b 1)
(new-arc 'grafo 'a 'c 3)
(new-arc 'grafo 'b 'e 5)
(new-arc 'grafo 'c 'e 1)
(new-arc 'grafo 'b 'd 3)
(new-arc 'grafo 'e 'd 1)
(new-heap 'heap)
(heap-insert 'heap 5 'a)
(heap-insert 'heap 7 'b)
(heap-insert 'heap 2 'c)
(heap-insert 'heap 1 'd)
(heap-insert 'heap 10 'e)
(heap-insert 'heap 4 'f)
(heap-insert 'heap 6 'g)
(heap-insert 'heap 8 'h)

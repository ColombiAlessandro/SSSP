(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))

(defun is-graph (graph-id)
	(gethash graph-id *graphs*))
	
(defun new-graph (graph-id)
	(or 	(gethash graph-id *graphs*)
		(setf (gethash graph-id *graphs*) graph-id)))
		
(defun delete-graph (graph-id)
  (remhash graph-id *graphs*)
  
  (maphash (lambda (key value)
             (when (string= graph-id (second key))
               (remhash key *vertices*)))
           *vertices*)
  
  (maphash (lambda (key value)
             (when (string= graph-id (second key))
               (remhash key *arcs*)))
           *arcs*))
	
(defun new-vertex (graph-id vertex-id)
	(setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
	(list 'vertex graph-id vertex-id)))
	
	
(defun graph-vertices (graph-id)
  (let ((lista nil))
    (maphash (lambda (key value)
               (let ((tmp (second key)))
                 (when (string= tmp graph-id)
                   (push key lista))))
             *vertices*)
    lista)) 
    
    
(defun new-arc (graph-id vertex-id vertex2-id &optional (weight 1)) 
	(setf (gethash (list 'arc graph-id vertex-id vertex2-id weight) *arcs*)
	(list 'arc graph-id vertex-id vertex2-id weight)))

(defun graph-arcs (graph-id)
  (let ((lista nil))
    (maphash (lambda (key value)
               (let ((tmp (second key)))
                 (when (string= tmp graph-id)
                   (push key lista))))
             *arcs*)
    lista))

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
    
(defun graph-print (graph-id)
	(let((lista nil)) 
		(push (graph-vertices graph-id) lista)
		(push (graph-arcs graph-id) lista)
		lista))
		
(defun new-heap (heap-id &optional (initial-capacity 1))
	(or (gethash heap-id *heaps*)
	    (setf (gethash heap-id *heaps*)
	    (list 'heap heap-id 0
	    (make-array initial-capacity :adjustable t)))))		
		
(defun heap-delete (heap-id)
	(remhash heap-id *heaps*))
	
(defun heap-empty (heap-id)
	(= 0 (third (gethash heap-id *heaps*))))
	
(defun heap-not-empty (heap-id)
	(not (heap-empty heap-id)))

(defun heap-insert (heap-id K V)
  (let ((value (gethash heap-id *heaps*)))
    (when value
      (let ((size  (third value))
            (array (fourth value)))

        (when (>= size (length array))
          (setf array (adjust-array array (+ 1 (length array))))
          (setf (fourth value) array))

        (setf (aref array size) (list K V))
        (incf (third value))

        (heapify-up array size)))))
   
(defun heapify-up (heap i)
  (when (> i 0)
    (let* ((parent (floor (- i 1) 2))
           (current (aref heap i))
           (par (aref heap parent)))
      (when (< (first current) (first par))
        (rotatef (aref heap i) (aref heap parent))
        (heapify-up heap parent)))))
        
	
(defun heapify (heap i heap-id)
  (let* ((len (third (gethash heap-id *heaps*)))
         (posMin i)
         (min (aref heap i))
         (l (+ (* 2 i) 1))
         (r (+ (* 2 i) 2)))

    (when (< l len)
      (let ((vall (aref heap l)))
        (when (< (first vall) (first min))
          (setf min vall
                posMin l))))

    (when (< r len)
      (let ((valr (aref heap r)))
        (when (< (first valr) (first min))
          (setf min valr
                posMin r))))

    (when (/= i posMin)
      (rotatef (aref heap i) (aref heap posMin))
      (heapify heap posMin heap-id))))
      
      
(defun heap-print (heap-id)
	(gethash heap-id *heaps*))
	
(defun heap-head (heap-id)
	(setf tmp (gethash heap-id *heaps*))
	(setf arr (fourth tmp))
	(aref arr 0))

(defun heap-extract (heap-id)
	(setf tmp (gethash heap-id *heaps*))
	(setf arr (fourth tmp))
	(aref arr 0)
	(setf (aref arr 0) nil)
	
	(heapify arr 0 heap-id))
	
		
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

;;;; cl-ring.lisp
(in-package #:cl-ring)

(defclass node ()
  ((id :accessor id :initarg :id)
   (neighbors :accessor neighbors :initform nil)
   (data :reader data :initform (make-hash-table :test 'equal))))

(defmethod hash (thing) (hash (write-to-string thing)))
(defmethod hash ((str string))
  (coerce 
   (ironclad:digest-sequence 
    :sha256 (ironclad:ascii-string-to-byte-array str))
   'list))

(defun mk-node ()
  (let ((n (make-instance 'node)))
    (setf (id n) (hash n))
    n))

(defmethod id-of ((n node)) (id n))
(defmethod id-of (entry) (hash (write-to-string entry)))

(defun distance (id-a id-b)
  (sqrt (loop for a in id-a for b in id-b sum (expt (- a b) 2))))

(defmethod closest-to ((n node) id)
  (first (sort (cons n (copy-list (neighbors n))) #'< 
	       :key (lambda (n) (distance (id n) id)))))

(defmethod introduce! ((a node) (b node))
  (push a (neighbors b))
  (push b (neighbors a))
  nil)

(defmethod join ((old node) (new node))
  (unless (zerop (distance (id old) (id new)))
    (let ((next (closest-to old (id new))))
      (if (eq next old)
	  (introduce! old new)
	  (join next new)))))

(defmethod store! ((n node) entry)
  (setf (gethash (id-of entry) (data n)) entry))

(defmethod network-store ((n node) entry)
  (let ((next (closest-to n (id-of entry))))
    (if (eq n next)
	(store! n entry)
	(network-store next entry))))

(defmethod seek ((n node) id)
  (let ((next (closest-to n id)))
    (if (eq n next)
	n
	(seek next id))))

(defmethod query ((n node) id)
  (multiple-value-bind (val found?) (gethash id (data n))
    (if found?
	(values val n)
	(let ((next (closest-to n id)))
	  (unless (eq n next)
	    (query next id))))))

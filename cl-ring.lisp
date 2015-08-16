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

;;;;;;;;;; Logging
(defparameter *log* nil)

(defmethod join :before (old new)
  (push `(join ,(id-of old) ,(id-of new)) *log*))
(defmethod introduce! :before (a b)
  (push `(introduce! ,(id-of a) ,(id-of b)) *log*))
(defmethod store! :before (node entry)
  (push `(store! ,(id-of node) ,(id-of entry) ,entry) *log*))
(defmethod network-store :before (node entry)
  (push `(network-store ,(id-of node) ,(id-of entry)) *log*))
(defmethod seek :before (node id)
  (push `(seek ,(id-of node) ,id) *log*))
(defmethod query :before (node id)
  (push `(query ,(id-of node) ,id) *log*))

;;;;;;;;;; Testing
(defparameter *origin* nil)

(defun origin! ()
  (let ((n (mk-node)))
    (setf *origin* n
	  *log* `((origin ,(id-of n)))))
  nil)

(defun run-test ()
  (origin!)
  (loop repeat 30 do (join *origin* (mk-node)))
  (loop for str in (list
		    "Hello world!"
		    "This is a test"
		    "Blah blah blah"
		    "This is just another test"
		    "Testing, one two, testing"
		    "This is just another test on Earth")
     do (network-store *origin* str)))

;;;;;;;;;; Server
(define-handler (cl-ring/source :close-socket? nil) ()
  (subscribe! :cl-notebook-updates sock))

(define-file-handler "static" :stem-from "static")

(define-handler (root) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head
      (:link :rel "stylesheet" :href "/static/css/test.css" :type "text/css" :media "screen")
      (:script :type "text/javascript" :src "/static/js/d3.min.js")
      (:script :type "text/javascript" :src "/static/js/graph-render.js"))
     (:body))))

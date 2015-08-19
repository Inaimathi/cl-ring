;;;; cl-ring.lisp
(in-package #:cl-ring)

(defclass node ()
  ((id :accessor id :initarg :id)
   (neighbors :accessor neighbors :initform nil)
   (data :reader data :initform (make-hash-table :test 'equal))))

(defmethod digest (thing) (digest (write-to-string thing)))
(defmethod digest ((str string))
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence 
    :sha256 (ironclad:ascii-string-to-byte-array str))))

(defun mk-node ()
  (let ((n (make-instance 'node)))
    (setf (id n) (digest n))
    n))

(defmethod id-of ((n node)) (id n))
(defmethod id-of (entry) (digest (write-to-string entry)))

(defun distance (id-a id-b)
  (sqrt (loop for a in id-a for b in id-b sum (expt (- a b) 2))))

(defmethod drop-common-prefix ((a string) (b string))
  (loop for i from 0 for ca across a for cb across b
     unless (eql ca cb)
     return (values (subseq a i) (subseq b i) i)))

(defun hex-digit (str ix)
  (parse-integer str :start ix :end (+ 1 ix) :radix 16))

(defmethod closest-to ((n node) (id string))
  (let ((res nil))
    (loop for neigh in (neighbors n)
       do (multiple-value-bind (my-id neigh-id i) (drop-common-prefix (id-of n) (id-of neigh))
	    (let ((a (hex-digit my-id 0))
		  (b (hex-digit neigh-id 0))
		  (c (hex-digit id i)))
	      (flet ((f (a b) (min (abs (- a b)) (mod (- a b) 16))))
		(when (> (f a c) (f b c))
		  (push neigh res))))))
    (if (null res)
	n
	(nth (random (length res)) res))))

(defun neighbor-list (id depth)
  (loop for i from 0 to depth
     append (mapcar 
	     (lambda (delta)
	       (let ((id-copy (copy-seq id)))
		 (setf (aref id-copy i) 
		       (char (format nil "~(~x~)" (mod (+ (hex-digit id-copy i) delta) 16)) 0))
		 id-copy))
	     '(1 -1))))

(defmethod introduce! ((a node) (b node))
  (unless (eq a b)
    (pushnew a (neighbors b) :test #'eq)
    (pushnew b (neighbors a) :test #'eq))
  nil)

(defmethod connect! ((start node) (n node) (id string))
  (let ((next start))
    (loop for neigh = (closest-to next id)
       do (if (eq next neigh)
	      (return (introduce! next n))
	      (setf next neigh)))))

(defmethod join ((old node) (new node))
  (loop for neigh in (neighbor-list (id-of new) 4)
     do (connect! old new neigh)))

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

;;;;;;;;;; Testing
(defparameter *origin* nil)
(defparameter *nodes* nil)

(defmethod join :before (old new)
  (push new *nodes*)
  (publish-update! :add-node :node-name (id-of new)))
(defmethod introduce! :before (a b)
  (publish-update! :add-edge :from (id-of a) :to (id-of b)))

;;;;; TODO
; store!
; network-store
; seek
; query

(defun origin! ()
  (let ((n (mk-node)))
    (setf *origin* n
	  *nodes* (list n))
    (publish-update! :add-node :node-name (id-of n)))
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
(defun publish-update! (event-type &rest k/v-pairs)
  (let ((hash (make-hash-table)))
    (loop for (k v) on k/v-pairs by #'cddr
       do (setf (gethash k hash) v))
    (setf (gethash :event-type hash) event-type)
    (publish! :cl-ring-updates (json:encode-json-to-string hash))))

(define-handler (cl-ring/source :close-socket? nil) ()
  (subscribe! :cl-ring-updates sock))

(define-file-handler "static" :stem-from "static")

(define-handler (root) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head
      (:link :rel "stylesheet" :href "/static/css/test.css" :type "text/css" :media "screen")
      (:script :type "text/javascript" :src "/static/js/d3.min.js")
      (:script :type "text/javascript" :src "/static/js/cola.v3.min.js")
      (:script :type "text/javascript" :src "/static/js/graph-render.js"))
     (:body))))

(define-handler (test) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head
      (:script :type "text/javascript" :src "/static/js/d3.min.js")
      (:script :type "text/javascript" :src "/static/js/cola.v3.min.js")
      (:script :type "text/javascript" :src "/static/js/test.js"))
     (:body))))

(let ((thread))
  (defun start! ()
    (setf thread
	  (bt:make-thread 
	   (lambda () (house:start 4141)))))

  (defun stop! ()
    (when (and thread (bt:thread-alive-p thread))
      (bt:destroy-thread thread))))

;;;;;;;;;; Fuck it
(defmethod graph->dot ((nodes list) &optional (stream *standard-output*))
  (let ((explored (make-hash-table))
	(names (make-hash-table))
	(ct 0))
    (flet ((explored? (thing) (gethash thing explored))
	   (explore! (thing) 
	     (setf (gethash thing explored) t))
	   (label-of (node) 
	     (subseq (id-of node) 0 6))
	   (name-of (thing)
	     (or (gethash thing names)
		 (setf (gethash thing names)
		       (format nil "node~a" (incf ct))))))
      (format stream "graph G {~%~{  ~a;~%~}}"
	      (loop for n in nodes
		 unless (explored? n)
		 collect (format nil "~a [label=~s]" (name-of n) (label-of n)) into nodes
		 append (progn (explore! n)
			       (loop for neigh in (neighbors n)
				  unless (explored? neigh)
				  collect (format nil "~a -- ~a" (name-of n) (name-of neigh))))
		 into edges
		 finally (return (append nodes edges)))))))

(defmethod graph->png ((nodes list) (fname string))
  (with-open-file (s "tmp.dot" :direction :output :if-does-not-exist :create)
    (graph->dot nodes s))
  (trivial-shell:shell-command (format nil "fdp -Tpng tmp.dot -o ~s" fname)))

;;;;;;;;;; Util
(defun hash (&rest k/v-pairs)
  (alexandria:plist-hash-table k/v-pairs))

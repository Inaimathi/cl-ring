;;;; cl-ring.lisp
(in-package #:cl-ring)

(defclass node ()
  ((id :accessor id :initarg :id)
   (neighbors :accessor neighbors :initform nil)
   (data :reader data :initform (make-hash-table :test 'equal))))

(defmethod digest (thing) (digest (write-to-string thing)))
(defmethod digest ((str string))
  (coerce 
   (ironclad:digest-sequence 
    :sha256 (ironclad:ascii-string-to-byte-array str))
   'list))

(defun mk-node ()
  (let ((n (make-instance 'node)))
    (setf (id n) (digest n))
    n))

(defmethod id-of ((n node)) (id n))
(defmethod id-of (entry) (digest (write-to-string entry)))

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

;;;;;;;;;; Testing
(defparameter *origin* nil)

(defmethod join :before (old new)
  (publish-update! :add-node :node-name (name-of new)))
(defmethod introduce! :before (a b)
  (let ((n-a (name-of a))
	(n-b (name-of b)))
    (publish-update! :add-edge :from n-a :to n-b)
    (publish-update! :add-edge :from n-b :to n-a)))

;;;;; TODO
; store!
; network-store
; seek
; query

(defmethod name-of ((n node))
  (ironclad:byte-array-to-hex-string
   (coerce (id-of n) '(vector (unsigned-byte 8)))))

(defun origin! ()
  (let ((n (mk-node)))
    (setf *origin* n)
    (publish-update! :add-node :node-name (name-of n)))
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
      (:script :type "text/javascript" :src "/static/js/graph-render.js"))
     (:body))))

(let ((thread))
  (defun start! ()
    (setf thread
	  (bt:make-thread 
	   (lambda () (house:start 4141)))))

  (defun stop! ()
    (when (and thread (bt:thread-alive-p thread))
      (bt:destroy-thread thread))))

;;;;;;;;;; Util
(defun hash (&rest k/v-pairs)
  (alexandria:plist-hash-table k/v-pairs))

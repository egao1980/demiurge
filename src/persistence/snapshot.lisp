(defpackage #:demiurge/src/persistence/snapshot
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:read-section #:write-section #:list-sections
                #:blackboard-lock #:make-blackboard)
  (:export #:save-blackboard #:load-blackboard #:snapshot-to-file #:restore-from-file))

(in-package #:demiurge/src/persistence/snapshot)

(defun serialize-section-value (value)
  "Convert a section value to a serializable form."
  (typecase value
    ((or string number keyword symbol) value)
    (list (mapcar #'serialize-section-value value))
    (hash-table
     (let ((ht (make-hash-table :test 'equal)))
       (setf (gethash "__type" ht) "hash-table")
       (maphash (lambda (k v)
                  (setf (gethash (format nil "~A" k) ht) (serialize-section-value v)))
                value)
       ht))
    (t (format nil "~S" value))))

(defun save-blackboard (bb &optional (stream *standard-output*))
  "Serialize blackboard sections to JSON on STREAM."
  (let ((snapshot (make-hash-table :test 'equal)))
    (dolist (key (list-sections bb))
      (setf (gethash (format nil "~A" key) snapshot)
            (serialize-section-value (read-section bb key))))
    (yason:encode snapshot stream)))

(defun load-blackboard (bb stream)
  "Restore blackboard sections from JSON STREAM."
  (let ((data (yason:parse stream :object-as :hash-table :object-key-fn #'identity)))
    (maphash (lambda (k v)
               (write-section bb (intern (string-upcase k) :keyword) v))
             data)
    bb))

(defun snapshot-to-file (bb path)
  "Save blackboard state to a JSON file."
  (with-open-file (s path :direction :output :if-exists :supersede)
    (save-blackboard bb s))
  path)

(defun restore-from-file (path &key bb)
  "Restore blackboard state from a JSON file."
  (let ((bb (or bb (make-blackboard))))
    (with-open-file (s path :direction :input)
      (load-blackboard bb s))
    bb))

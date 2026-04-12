(defpackage #:demiurge/src/knowledge-source/versioned
  (:use #:cl)
  (:import-from #:demiurge/src/knowledge-source/protocol
                #:knowledge-source #:ks-name #:ks-version
                #:ks-precondition #:ks-execute #:ks-postcondition)
  (:export #:versioned-ks #:make-versioned-ks
           #:current-version #:candidate-version #:version-metrics
           #:split-ratio #:promote-candidate #:demote-candidate))

(in-package #:demiurge/src/knowledge-source/versioned)

(defclass versioned-ks (knowledge-source)
  ((current :initarg :current :accessor current-version)
   (candidate :initarg :candidate :accessor candidate-version :initform nil)
   (metrics :initform (make-hash-table :test 'equal) :reader version-metrics)
   (split-ratio :initarg :split-ratio :initform 0.2 :accessor split-ratio)
   (call-count :initform 0 :accessor call-count)))

(defun make-versioned-ks (current &key candidate (split-ratio 0.2))
  (make-instance 'versioned-ks
                 :name (ks-name current)
                 :version (format nil "~A+versioned" (ks-version current))
                 :current current
                 :candidate candidate
                 :split-ratio split-ratio))

(defmethod ks-precondition ((ks versioned-ks) bb)
  (ks-precondition (current-version ks) bb))

(defmethod ks-execute ((ks versioned-ks) bb)
  (incf (call-count ks))
  (let* ((use-candidate (and (candidate-version ks)
                             (< (random 1.0) (split-ratio ks))))
         (impl (if use-candidate (candidate-version ks) (current-version ks)))
         (start (get-internal-real-time))
         (result (ks-execute impl bb))
         (elapsed (/ (- (get-internal-real-time) start)
                     internal-time-units-per-second)))
    ;; Record metrics
    (let ((key (if use-candidate :candidate :current)))
      (push (list :duration elapsed :success t :call (call-count ks))
            (gethash key (version-metrics ks))))
    result))

(defun promote-candidate (ks)
  "Replace current with candidate."
  (when (candidate-version ks)
    (setf (current-version ks) (candidate-version ks)
          (candidate-version ks) nil)))

(defun demote-candidate (ks)
  "Remove candidate."
  (setf (candidate-version ks) nil))

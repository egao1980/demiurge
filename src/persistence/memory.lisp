(defpackage #:demiurge/src/persistence/memory
  (:use #:cl)
  (:import-from #:alexandria #:when-let #:ensure-gethash)
  (:export #:persistent-memory #:make-persistent-memory
           #:mem-get #:mem-set #:mem-delete #:mem-keys #:mem-has-p
           #:mem-append #:mem-get-list #:mem-get-list-last
           #:mem-increment #:mem-get-number
           #:mem-save #:mem-load
           #:with-memory-transaction))

(in-package #:demiurge/src/persistence/memory)

(defclass persistent-memory ()
  ((store :initform (make-hash-table :test 'equal) :reader memory-store)
   (path :initarg :path :reader memory-path :initform nil)
   (lock :initform (bt2:make-lock :name "persistent-memory") :reader memory-lock)
   (dirty :initform nil :accessor memory-dirty-p)
   (auto-save :initarg :auto-save :reader memory-auto-save-p :initform t)))

(defun make-persistent-memory (&key path (auto-save t))
  "Create a persistent memory backed by a JSON file at PATH.
   Loads existing data if file exists. Auto-saves on mutation when AUTO-SAVE is T."
  (let ((mem (make-instance 'persistent-memory :path path :auto-save auto-save)))
    (when (and path (probe-file path))
      (mem-load mem))
    mem))

;;; --- Key-Value ---

(defun mem-get (mem key &optional default)
  "Get value by string key."
  (bt2:with-lock-held ((memory-lock mem))
    (gethash key (memory-store mem) default)))

(defun mem-set (mem key value)
  "Set value for key. Auto-saves if configured."
  (bt2:with-lock-held ((memory-lock mem))
    (setf (gethash key (memory-store mem)) value
          (memory-dirty-p mem) t))
  (when (memory-auto-save-p mem)
    (mem-save mem))
  value)

(defun mem-delete (mem key)
  (bt2:with-lock-held ((memory-lock mem))
    (remhash key (memory-store mem))
    (setf (memory-dirty-p mem) t))
  (when (memory-auto-save-p mem)
    (mem-save mem)))

(defun mem-has-p (mem key)
  (bt2:with-lock-held ((memory-lock mem))
    (nth-value 1 (gethash key (memory-store mem)))))

(defun mem-keys (mem &optional prefix)
  "List all keys, optionally filtered by PREFIX."
  (bt2:with-lock-held ((memory-lock mem))
    (let ((result nil))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (when (or (null prefix)
                           (and (>= (length k) (length prefix))
                                (string= prefix k :end2 (length prefix))))
                   (push k result)))
               (memory-store mem))
      (sort result #'string<))))

;;; --- List operations (append-only log per key) ---

(defun mem-append (mem key value &key (max-entries 1000))
  "Append VALUE to the list stored at KEY. Trims oldest entries beyond MAX-ENTRIES."
  (bt2:with-lock-held ((memory-lock mem))
    (let* ((existing (gethash key (memory-store mem)))
           (lst (if (listp existing) existing nil))
           (new-list (append lst (list value))))
      (when (> (length new-list) max-entries)
        (setf new-list (subseq new-list (- (length new-list) max-entries))))
      (setf (gethash key (memory-store mem)) new-list
            (memory-dirty-p mem) t)))
  (when (memory-auto-save-p mem)
    (mem-save mem))
  value)

(defun mem-get-list (mem key &key (limit nil) (offset 0))
  "Get list stored at KEY, with optional pagination."
  (bt2:with-lock-held ((memory-lock mem))
    (let ((lst (gethash key (memory-store mem))))
      (when (listp lst)
        (let ((sub (if (plusp offset) (nthcdr offset lst) lst)))
          (if limit (subseq sub 0 (min limit (length sub))) sub))))))

(defun mem-get-list-last (mem key &optional (n 1))
  "Get last N items from the list at KEY."
  (bt2:with-lock-held ((memory-lock mem))
    (let ((lst (gethash key (memory-store mem))))
      (when (listp lst)
        (last lst n)))))

;;; --- Numeric counters ---

(defun mem-increment (mem key &optional (delta 1))
  "Atomically increment numeric value at KEY."
  (bt2:with-lock-held ((memory-lock mem))
    (let ((cur (or (gethash key (memory-store mem)) 0)))
      (setf (gethash key (memory-store mem)) (+ cur delta)
            (memory-dirty-p mem) t)))
  (when (memory-auto-save-p mem)
    (mem-save mem)))

(defun mem-get-number (mem key &optional (default 0))
  (let ((v (mem-get mem key default)))
    (if (numberp v) v default)))

;;; --- Persistence ---

(defun serialize-for-json (value)
  "Recursively prepare value for yason encoding."
  (typecase value
    (hash-table value)
    (null :null)
    (keyword (symbol-name value))
    (symbol (format nil "~A" value))
    (list (mapcar #'serialize-for-json value))
    (t value)))

(defun mem-save (mem)
  "Write memory to disk as JSON."
  (when-let (path (memory-path mem))
    (bt2:with-lock-held ((memory-lock mem))
      (when (memory-dirty-p mem)
        (ensure-directories-exist path)
        (let ((out (make-hash-table :test 'equal)))
          (maphash (lambda (k v)
                     (setf (gethash k out) (serialize-for-json v)))
                   (memory-store mem))
          (with-open-file (s path :direction :output :if-exists :supersede)
            (yason:encode out s)))
        (setf (memory-dirty-p mem) nil))))
  mem)

(defun mem-load (mem)
  "Load memory from disk."
  (when-let (path (memory-path mem))
    (when (probe-file path)
      (bt2:with-lock-held ((memory-lock mem))
        (let ((data (with-open-file (s path :direction :input)
                      (yason:parse s :object-as :hash-table :object-key-fn #'identity))))
          (clrhash (memory-store mem))
          (maphash (lambda (k v)
                     (setf (gethash k (memory-store mem))
                           (if (eq v :null) nil v)))
                   data)
          (setf (memory-dirty-p mem) nil)))))
  mem)

;;; --- Transaction-like batch writes ---

(defmacro with-memory-transaction ((mem) &body body)
  "Execute BODY with auto-save temporarily disabled, then save once at the end."
  (let ((g-mem (gensym "MEM")))
    `(let ((,g-mem ,mem))
       (let ((old-auto (slot-value ,g-mem 'auto-save)))
         (unwind-protect
              (progn
                (setf (slot-value ,g-mem 'auto-save) nil)
                ,@body)
           (setf (slot-value ,g-mem 'auto-save) old-auto)
           (mem-save ,g-mem))))))

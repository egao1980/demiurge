(in-package #:demiurge/tests)

(defmacro with-clean-registry (&body body)
  `(let ((demiurge::*expert-registry* (make-hash-table :test 'equal))
         (demiurge::*board-domains* (make-hash-table :test 'eq)))
     ,@body))

(defun drain (bb &key (timeout 8))
  (bb:run-scheduler bb :until-empty t :timeout timeout)
  bb)

(defun mock-llm (&key (prefix "echo: ") handler)
  "llm-protocol mock backend for tests. PREFIX is prepended to the prompt."
  (if handler
      (llm:make-mock-llm-backend :handler handler)
      (llm:make-mock-llm-backend :prefix prefix)))

(defun %section-alist (board)
  (sort (mapcar (lambda (k) (cons k (bb:read-section board k)))
                (bb:list-sections board))
        #'string< :key (lambda (p) (string (car p)))))

(defun %write-tmp-toml (text)
  (uiop:with-temporary-file (:pathname path :prefix "demiurge-cfg-" :type "toml"
                             :keep t)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string text out))
    path))

(defun %sqlite-available-p ()
  (or (find-package '#:sql-backend-sqlite3)
      (ignore-errors
        (asdf:load-system "sql-backend-sqlite3" :verbose nil)
        (find-package '#:sql-backend-sqlite3))))

(defmacro with-tmp-dir ((var) &body body)
  `(let ((,var (ensure-directories-exist
                (uiop:ensure-directory-pathname
                 (merge-pathnames (format nil "demiurge-~a-~a/"
                                          (get-universal-time)
                                          (random 1000000))
                                  (uiop:temporary-directory))))))
     (unwind-protect (progn ,@body)
       (ignore-errors
         (uiop:delete-directory-tree ,var :validate t
                                     :if-does-not-exist :ignore)))))

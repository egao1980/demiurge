(in-package #:demiurge/workflows)

;;; Incremental workspace tree cache + optional BM25 hybrid wrap + Aider-shaped
;;; symbol-map seed. rag-backend-hybrid is a soft-dep (CI images may omit it).

(defparameter *workspace-symbol-focus*
  '("demiurge-plan-vectors/examples/corpus/cl-stack.md"
    "demiurge-plan-vectors/examples/cl-dev-expert.toml"
    "demiurge-plan-vectors/examples/cl-dev-expert.lisp"
    "demiurge-plan-vectors/src/improve/cycle.lisp"
    "demiurge-plan-vectors/src/improve/promotion.lisp"
    "demiurge-plan-vectors/src/improve/package.lisp"
    "demiurge-plan-vectors/src/workflows/deep-research.lisp"
    "demiurge-plan-vectors/src/workflows/workspace.lisp"
    "demiurge-plan-vectors/src/workflows/index.lisp")
  "Always-ingested product files when the tree root can see them.")

(defun workspace-local-query-p (query)
  "True when QUERY is a workspace:// lookup — skip websearch."
  (and (stringp query)
       (search "workspace://" query :test #'char-equal)
       t))

(defun ensure-research-tree-index (workspace &key root
                                              (max-files *default-tree-max-files*))
  "Walk ROOT once. Reuse entries whose mtime+size match."
  (check-type workspace research-workspace)
  (let ((root (or (and root (%existing-dir-pathname root))
                  (research-workspace-tree-root workspace))))
    (unless root
      (return-from ensure-research-tree-index nil))
    (unless (research-workspace-tree-root workspace)
      (setf (research-workspace-tree-root workspace) root))
    (let ((old (research-workspace-tree-index workspace))
          (new (make-hash-table :test #'equal))
          (rels (list-research-tree-files root :max-files max-files))
          (fresh 0))
      (dolist (rel rels)
        (let* ((path (ignore-errors (resolve-tree-path root rel)))
               (mtime (and path (ignore-errors (file-write-date path))))
               (size (or (and path
                              (ignore-errors
                                (pathlib:file-size (pathlib:path path))))
                         0))
               (cached (and old (gethash rel old))))
          (setf (gethash rel new)
                (if (and cached
                         (eql mtime (tfe-mtime cached))
                         (eql size (tfe-size cached)))
                    cached
                    (let* ((text (read-research-tree-file root rel))
                           (preview (if (<= (length text)
                                            *default-tree-preview-chars*)
                                        text
                                        (subseq text 0
                                                *default-tree-preview-chars*))))
                      (incf fresh)
                      (make-tree-file-entry :rel rel :path path
                                            :mtime mtime :size size
                                            :text text :preview preview))))))
      (setf (research-workspace-tree-index workspace) new)
      (research-trace "workspace index ~a files=~d fresh=~d"
                      (pathlib:as-posix root) (hash-table-count new) fresh)
      new)))

(defun %resolve-focus-rel (root rel)
  (flet ((ok (r)
           (when (and r (plusp (length r)) (not (%escapes-tree-p r)))
             (let ((p (ignore-errors (pathlib:path (resolve-tree-path root r)))))
               (and p (pathlib:exists-p p) (pathlib:file-p p) r)))))
    (or (ok rel)
        (when (eql (search "demiurge-plan-vectors/" rel) 0)
          (ok (subseq rel (length "demiurge-plan-vectors/")))))))

(defun %symbol-map-line-p (line)
  (let ((s (string-trim '(#\Space #\Tab) (or line ""))))
    (or (eql (search "(defun " s) 0)
        (eql (search "(defmacro " s) 0)
        (eql (search "(defparameter " s) 0)
        (eql (search "(defvar " s) 0)
        (eql (search "(defclass " s) 0)
        (eql (search "(defgeneric " s) 0)
        (and (plusp (length s)) (char= (char s 0) #\#))
        (search "no-critical-regression" s :test #'char-equal)
        (search "default-improve-gate" s :test #'char-equal)
        (search "make-default-promotion-gate" s :test #'char-equal)
        (search "KSAR" s)
        (search "gap-analysis" s :test #'char-equal))))

(defun workspace-symbol-map (&key root (focus *workspace-symbol-focus*)
                                  (max-lines 80) (max-chars 8000))
  "Compact defun/gate/heading lines from FOCUS files under ROOT."
  (let ((root (or (and root (%existing-dir-pathname root))
                  (resolve-research-tree-root)))
        (lines '())
        (n 0))
    (unless root
      (return-from workspace-symbol-map ""))
    (dolist (rel focus)
      (when (< n max-lines)
        (let ((resolved (%resolve-focus-rel root rel)))
          (when resolved
            (push (format nil "## ~a" resolved) lines)
            (incf n)
            (dolist (line (uiop:split-string (read-research-tree-file root resolved)
                                             :separator '(#\Newline)))
              (when (and (< n max-lines) (%symbol-map-line-p line))
                (push (format nil "~a|~a" resolved
                              (string-trim '(#\Space #\Tab) line))
                      lines)
                (incf n)))))))
    (let ((s (with-output-to-string (out)
               (dolist (line (nreverse lines))
                 (write-line line out)))))
      (if (> (length s) max-chars)
          (subseq s 0 max-chars)
          s))))

(defun %ingest-symbol-map (ws)
  (let ((map (workspace-symbol-map :root (research-tree-root ws))))
    (when (and map (plusp (length map)))
      (research-trace "workspace symbol-map chars=~d" (length map))
      (unless (find "workspace://.symbol-map" (research-workspace-sources ws)
                    :key #'%source-uri :test #'string=)
        (list (record-research-source
               ws
               :id "ws-symbol-map"
               :uri "workspace://.symbol-map"
               :title "workspace symbol map"
               :text map
               :subquestion "seed"
               :kind :workspace))))))

(defun %ingest-focus-files (ws)
  (let ((root (research-tree-root ws))
        (out '()))
    (unless root
      (return-from %ingest-focus-files nil))
    (dolist (rel *workspace-symbol-focus*)
      (let ((resolved (%resolve-focus-rel root rel)))
        (when resolved
          (let ((uri (workspace-resource-uri resolved)))
            (unless (find uri (research-workspace-sources ws)
                          :key #'%source-uri :test #'string=)
              (let ((text (read-research-tree-file root resolved)))
                (when (plusp (length text))
                  (push (record-research-source
                         ws
                         :id (format nil "ws-focus-~a"
                                     (substitute #\- #\/ resolved))
                         :uri uri
                         :title resolved
                         :text text
                         :subquestion "seed"
                         :kind :workspace)
                        out))))))))
    (nreverse out)))

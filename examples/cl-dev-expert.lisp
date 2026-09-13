(in-package #:demiurge)

;;; Reference expert: CL-development over this workspace's docs/.
;;; Catalogue ops lookup-symbol / search-corpus / run-tests (:compute-gated).

(defvar *cl-dev-corpus-chunks* nil
  "List of (:path :text) plists ingested from workspace docs/.")

(cap:defcapability :lisp-dev
    "Common Lisp workspace tools (lookup, corpus search, gated tests)."
  (:operation lookup-symbol ((name t)) :returns t
   :doc "Describe a Lisp symbol by name.")
  (:operation search-corpus ((query t)) :returns t
   :doc "Search the expert's document corpus.")
  (:operation run-tests ((system t)) :returns t
   :doc "Run ASDF test-op for SYSTEM. Requires a granted :compute capability."))

(cap:defcatalogue :cl-dev
    "CL-development expert vocabulary."
  :lisp-dev :compute :code-editing)

(defun %workspace-docs-dir ()
  (let ((root (asdf:system-source-directory "demiurge")))
    (or (probe-file (merge-pathnames "../docs/" root))
        (probe-file (asdf:system-relative-pathname "demiurge" "docs/")))))

(defun %bundled-corpus-dir ()
  (probe-file (asdf:system-relative-pathname "demiurge" "examples/corpus/")))

(defun %md-files (dir)
  (when (and dir (probe-file dir))
    (directory (merge-pathnames "*.md" (uiop:ensure-directory-pathname dir)))))

(defun %load-docs-corpus (&optional dir)
  (let* ((dirs (remove nil (list dir (%workspace-docs-dir) (%bundled-corpus-dir))))
         (files (mapcan #'%md-files dirs)))
    (setf *cl-dev-corpus-chunks*
          (mapcar (lambda (path)
                    (list :path (namestring path)
                          :text (uiop:read-file-string path)))
                  files))))

(defun %describe-symbol (name)
  (let* ((s (string-upcase (string name)))
         (packages (list (find-package :cl)
                         (find-package :demiurge)
                         (find-package :keyword)))
         (hits '()))
    (dolist (pkg packages)
      (when pkg
        (multiple-value-bind (sym status) (find-symbol s pkg)
          (when (and sym status)
            (push (format nil "~a:~a (~a~@[ function~]~@[ class~])"
                          (package-name pkg) (symbol-name sym)
                          (string-downcase (symbol-name status))
                          (and (fboundp sym) t)
                          (and (find-class sym nil) t))
                  hits)))))
    (if hits
        (format nil "~{~a~^; ~}" (nreverse hits))
        (format nil "unknown symbol ~a" name))))

(defmethod lookup-symbol ((cap lisp-dev-capability) name &key)
  (declare (ignore cap))
  (%describe-symbol name))

(defmethod search-corpus ((cap lisp-dev-capability) query &key)
  (declare (ignore cap))
  (let* ((q (string-downcase (string query)))
         (hits '()))
    (dolist (chunk *cl-dev-corpus-chunks*)
      (let ((text (getf chunk :text))
            (path (getf chunk :path)))
        (when (and text (search q (string-downcase text)))
          (let* ((pos (search q (string-downcase text)))
                 (start (max 0 (- pos 40)))
                 (end (min (length text) (+ pos (length q) 80))))
            (push (format nil "~a: …~a…"
                          (file-namestring path)
                          (substitute #\Space #\Newline
                                      (subseq text start end)))
                  hits)))))
    (if hits
        (format nil "~{~a~^~%~}" (nreverse hits))
        (format nil "no corpus hits for ~a" query))))

(defmethod run-tests ((cap lisp-dev-capability) system &key)
  (declare (ignore cap))
  (unless (and *operation-catalogue*
               (cap:capability-supported-p *operation-catalogue* :compute))
    (restart-case
        (error 'compute-denied
               :message "run-tests is gated behind :compute")
      (skip ()
        :report "Skip run-tests"
        (return-from run-tests "skipped: compute not granted"))))
  (let ((name (string-downcase (string system))))
    (if (asdf:find-system name nil)
        (format nil "would test-system ~a" name)
        (format nil "unknown system ~a" name))))

(defun %cl-dev-skills ()
  (list (steer:make-steer-skill
         "cite-docs"
         :description "Cite workspace docs when answering."
         :body "When you use a fact from the corpus, name the source file (e.g. DEMIURGE-PLAN.md). Prefer quotes over paraphrase for protocol contracts.")
        (steer:make-steer-skill
         "prefer-protocols"
         :description "Prefer shipped cl-stack protocol APIs."
         :body "Answer with the actual protocol GF/class names (blackboard-protocol, llm-protocol, task-protocol). Do not invent a parallel SDK.")
        (steer:make-steer-skill
         "no-guess"
         :description "Search before guessing."
         :body "If you are unsure, call search-corpus or lookup-symbol. Say when the corpus has no hit. Never fabricate CLHS section numbers.")))

(defun %cl-dev-eval-cases ()
  (flet ((c (input expected &optional critical)
           (eval:make-eval-case
            :input input :expected expected
            :metadata (if critical '(:tags (:critical)) nil))))
    (list
     (c "What is defexpert?" "expert-domain" t)
     (c "Where does KSAR control live?" "agenda" t)
     (c "What journals section writes?" "task-protocol" t)
     (c "How are logs correlated with traces?" "trace-id" t)
     (c "Personal profile persistence?" "SQLite" t)
     (c "What gates run-tests?" ":compute" t)
     (c "What wraps an ai-agent as a knowledge source?" "agent-ks")
     (c "Name the self-improvement gate that blocks critical regressions."
        "no-critical-regression")
     (c "Which protocol is the RAG store surface?" "rag-protocol")
     (c "Which protocol is the LLM generate surface?" "llm-protocol")
     (c "What subsystem persists the blackboard?" "blackboard-journal")
     (c "Config facade for TOML + env?" "cl-stack-config")
     (c "Span name for KS execution?" "demiurge.ksar.execute")
     (c "Span name for an agent run?" "demiurge.agent.run")
     (c "Default personal LLM backends?" "llama-cpp")
     (c "What macro is thin sugar over make-instance?" "defexpert")
     (c "Which protocol owns steering skills?" "steer-protocol")
     (c "Session SQL store system?" "conversation-backend-sql")
     (c "Durable step macro?" "with-durable-step")
     (c "Never emit spans through which logger?" "log-protocol"))))

(defun make-cl-dev-catalogue (&key grant-compute)
  (let ((cat (cap:make-catalogue :cl-dev))
        (lisp (make-instance 'lisp-dev-capability)))
    (cap:register-capability cat lisp)
    (when grant-compute
      (cap:register-capability cat (make-instance 'cap:compute-capability)))
    cat))

(defun make-cl-dev-expert (&key backend name profile (watch '(:prompt))
                             instructions grant-compute
                             (ingest t))
  "Reference CL-development expert. BACKEND is typically MAKE-MOCK-LLM-BACKEND."
  (when ingest
    (%load-docs-corpus))
  (let* ((cfg (current-demiurge-config))
         (catalogue (make-cl-dev-catalogue :grant-compute grant-compute))
         (dataset (eval:make-eval-dataset
                   :name "cl-dev"
                   :cases (%cl-dev-eval-cases)))
         (agent (agent:make-ai-agent
                 :name "cl-dev"
                 :backend backend
                 :instructions
                 (or instructions
                     "You are a Common Lisp / cl-stack expert. Use lookup-symbol and search-corpus. Cite docs. Do not guess.")
                 :memory (conv:make-window-memory
                          :store (and (deployment-profile-p profile)
                                      (profile-session-store profile))
                          :window-size (demiurge-config-session-window-turns cfg)
                          :session "cl-dev")))
         (ks (make-agent-ks :name 'cl-dev
                            :agent agent
                            :watch watch
                            :prompt-key :prompt
                            :result-key :result
                            :catalogue catalogue
                            :steering (%cl-dev-skills))))
    (make-expert-domain
     :name (or name "cl-dev")
     :catalogue catalogue
     :ks-set (list ks)
     :steering (%cl-dev-skills)
     :corpora (mapcar (lambda (c) (getf c :path)) *cl-dev-corpus-chunks*)
     :eval-suites (list dataset)
     :profile (or profile :personal))))

(defun cl-dev-expert (&rest args &key &allow-other-keys)
  (apply #'make-cl-dev-expert args))

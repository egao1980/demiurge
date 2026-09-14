(in-package #:demiurge/workflows)

(defparameter *default-tree-skip-dirs*
  '(".git" ".qlot" ".demo-oci" "node_modules" ".cache" "recordings"
    ".local" "__fasl" ".asdf" "fasl")
  "Directory names skipped while walking a research tree.")

(defparameter *default-tree-suffixes*
  '(".lisp" ".asd" ".md" ".toml" ".yml" ".yaml")
  "Text suffixes exposed over workspace://.")

(defparameter *default-tree-focus*
  '("demiurge-plan-vectors/" "demiurge/" "demiurge-parity-wrap/demos/"
    "MEMORY.md" "AGENTS.md" "docs/" "LESSONS_LEARNED.md")
  "When ROOT looks like cl-workspace, walk these first.")

(defparameter *default-tree-max-files* 400)
(defparameter *default-tree-max-bytes* 200000)
(defparameter *default-tree-preview-chars* 8000)

(defun %env-nonempty (name)
  (let ((v (uiop:getenv name)))
    (and v (plusp (length v)) v)))

(defun %as-dir (designator)
  (and designator (pathlib:ensure-directory designator)))

(defun %existing-dir-pathname (designator)
  "DIRECTORY pathname via pathlib:absolute (not resolve — /tmp ≠ /private/tmp)."
  (let ((p (%as-dir designator)))
    (and p (pathlib:directory-p p)
         (pathlib:path-pathname (pathlib:absolute p)))))

(defun find-lisp-workspace-root (&optional (start (pathlib:cwd)))
  "Walk up from START looking for .lisp-workspace/ or AGENTS.md."
  (loop for dir = (%as-dir start)
          then (pathlib:parent dir)
        for prev = nil then dir
        until (or (null dir)
                  (and prev (equal (pathlib:as-posix dir)
                                   (pathlib:as-posix prev))))
        when (or (pathlib:exists-p (pathlib:join dir ".lisp-workspace"))
                 (pathlib:exists-p (pathlib:join dir "AGENTS.md")))
          return (pathlib:path-pathname (pathlib:absolute dir))))

(defun resolve-research-tree-root (&optional explicit)
  "EXPLICIT path, then DEMIURGE_WORKSPACE / CL_WORKSPACE, then walk-up."
  (or (and explicit (%existing-dir-pathname explicit))
      (let ((env (or (%env-nonempty "DEMIURGE_WORKSPACE")
                     (%env-nonempty "CL_WORKSPACE"))))
        (and env (%existing-dir-pathname env)))
      (find-lisp-workspace-root)))

(defun %tree-root-from-domain (domain)
  (let* ((profile (and (expert-domain-p domain) (expert-profile domain)))
         (cfg (and (deployment-profile-p profile) (profile-config profile)))
         (raw (and cfg (demiurge-config-workspace-root cfg))))
    (and raw (plusp (length (string raw)))
         (%existing-dir-pathname raw))))

(defun research-tree-root (ws)
  (and (research-workspace-p ws) (research-workspace-tree-root ws)))

(defun %tree-root-path (root)
  (pathlib:absolute (%as-dir root)))

(defun workspace-resource-uri (rel)
  (let ((s (string-left-trim '(#\/)
                             (if rel (pathlib:as-posix rel) ""))))
    (if (plusp (length s))
        (format nil "workspace://~a" s)
        "workspace://")))

(defun workspace-resource-uri-p (uri)
  (and (stringp uri) (eql (search "workspace://" uri) 0)))

(defun workspace-uri-relpath (uri)
  (when (workspace-resource-uri-p uri)
    (string-left-trim '(#\/) (subseq uri (length "workspace://")))))

(defun %escapes-tree-p (rel)
  "Lexical jail: absolute, drive letter, or `..` components."
  (let* ((raw (substitute #\/ #\\ (string (or rel ""))))
         (p (pathlib:path raw)))
    (or (pathlib:absolute-p p)
        (member ".." (pathlib:parts p) :test #'string=)
        (and (>= (length raw) 2) (char= (char raw 1) #\:)))))

(defun resolve-tree-path (root rel)
  "Jail REL under ROOT with pathlib:under. Signals RESEARCH-ERROR on escape.
   Uses absolute + normpath, not resolve, so /tmp identity is preserved."
  (let* ((root-p (%tree-root-path root))
         (raw (substitute #\/ #\\ (string (or rel "")))))
    (when (zerop (length (string-left-trim '(#\/) raw)))
      (return-from resolve-tree-path (pathlib:path-pathname root-p)))
    (when (%escapes-tree-p raw)
      (error 'research-error
             :message (format nil "path escapes workspace: ~a" rel)))
    (let ((merged (pathlib:normpath
                   (pathlib:under root-p (string-left-trim '(#\/) raw)))))
      (unless (pathlib:relative-to-p merged root-p)
        (error 'research-error
               :message (format nil "path escapes workspace: ~a" rel)))
      (pathlib:path-pathname merged))))

(defun %skip-dir-p (path skip)
  (let ((name (pathlib:name (pathlib:ensure-directory path))))
    (and name (member name skip :test #'string-equal))))

(defun %suffix-ok-p (path suffixes)
  (let ((suf (pathlib:suffix path)))
    (and suf (plusp (length suf))
         (member suf suffixes :test #'string-equal))))

(defun %cl-workspace-root-p (root)
  (let ((dir (%as-dir root)))
    (and dir
         (pathlib:exists-p (pathlib:join dir ".lisp-workspace"))
         (pathlib:exists-p (pathlib:join dir "demiurge-plan-vectors")))))

(defun %rel-of (root path)
  "POSIX relpath. absolute first; resolve both only if listing used realpath."
  (let* ((root-p (%tree-root-path root))
         (path-p (pathlib:absolute path)))
    (flet ((rel (a b)
             (when (pathlib:relative-to-p a b)
               (string-left-trim
                '(#\/)
                (pathlib:as-posix (pathlib:relative-to a b))))))
      (or (rel path-p root-p)
          (let ((r-root (ignore-errors (pathlib:resolve root-p :strict nil)))
                (r-path (ignore-errors (pathlib:resolve path-p :strict nil))))
            (and r-root r-path (rel r-path r-root)))))))

(defun %walk-one (root rel &key max-files suffixes skip)
  (let ((start (handler-case (pathlib:path (resolve-tree-path root rel))
                 (research-error () nil)
                 (error () nil)))
        (out '()))
    (labels ((visit (p)
               (when (>= (length out) max-files)
                 (return-from visit))
               (cond
                 ((pathlib:directory-p p)
                  (unless (%skip-dir-p p skip)
                    (dolist (kid (ignore-errors (pathlib:iterdir p)))
                      (visit kid))))
                 ((and (pathlib:file-p p) (%suffix-ok-p p suffixes))
                  (let ((rel (%rel-of root p)))
                    (when rel (push rel out)))))))
      (when (and start (pathlib:exists-p start))
        (visit start)))
    (nreverse out)))

(defun list-research-tree-files (root &key (max-files *default-tree-max-files*)
                                      (suffixes *default-tree-suffixes*)
                                      (skip *default-tree-skip-dirs*))
  "Relative path strings under ROOT, bounded and suffix-filtered."
  (unless (and root (pathlib:directory-p (%as-dir root)))
    (return-from list-research-tree-files nil))
  (let ((acc '()))
    (if (%cl-workspace-root-p root)
        (dolist (focus *default-tree-focus*)
          (when (< (length acc) max-files)
            (setf acc (append acc
                              (%walk-one root focus
                                         :max-files (- max-files (length acc))
                                         :suffixes suffixes
                                         :skip skip)))))
        (setf acc (%walk-one root ""
                             :max-files max-files
                             :suffixes suffixes
                             :skip skip)))
    (remove-duplicates acc :test #'equal)))

(defun read-research-tree-file (root rel &key (max-bytes *default-tree-max-bytes*))
  "UTF-8 text of REL under ROOT. Empty string if unreadable / too large."
  (let ((p (pathlib:path (resolve-tree-path root rel))))
    (cond
      ((not (pathlib:exists-p p)) "")
      ((pathlib:directory-p p) "")
      (t
       (let ((len (or (ignore-errors (pathlib:file-size p)) 0)))
         (cond
           ((> len max-bytes)
            (format nil "[skipped ~a: ~d bytes > ~d]~%" rel len max-bytes))
           (t
            (handler-case (pathlib:read-text p)
              (error ()
                "")))))))))

(defun %tree-file-score (query rel text)
  (let* ((hay (string-downcase (format nil "~a~%~a" rel (or text ""))))
         (toks (remove-duplicates (%tokenize query) :test #'string=)))
    (if (null toks)
        0.0
        (/ (count-if (lambda (tok) (search tok hay)) toks)
           (float (length toks))))))

(defun search-research-tree (root query &key (top-k 4)
                                        (max-files *default-tree-max-files*))
  "Top-K relative paths under ROOT scored against QUERY (path + preview)."
  (let ((ranked
         (loop for rel in (list-research-tree-files root :max-files max-files)
               for preview = (let ((s (read-research-tree-file root rel)))
                               (if (<= (length s) *default-tree-preview-chars*)
                                   s
                                   (subseq s 0 *default-tree-preview-chars*)))
               for score = (%tree-file-score query rel preview)
               when (> score 0.0)
                 collect (list :rel rel :score score :preview preview))))
    (subseq (sort ranked #'> :key (lambda (e) (getf e :score)))
            0 (min top-k (length ranked)))))

(defun workspace-catalog-text (ws)
  (let ((root (research-tree-root ws)))
    (with-output-to-string (s)
      (format s "workspace root: ~a~%"
              (or (and root (pathlib:as-posix root)) ""))
      (dolist (rel (list-research-tree-files root))
        (format s "~a~%" (workspace-resource-uri rel))))))

(defun workspace-seed-from-domain (domain)
  (let* ((profile (and (expert-domain-p domain) (expert-profile domain)))
         (cfg (and (deployment-profile-p profile) (profile-config profile)))
         (raw (and cfg (demiurge-config-workspace-seed cfg))))
    (and raw (plusp (length (string raw))) (string raw))))

(defun seed-research-workspace (ws &key query seed (top-k 6))
  "Ingest checkout hits for SEED then QUERY so children see workspace:// sources."
  (unless (research-tree-root ws)
    (return-from seed-research-workspace nil))
  (let ((terms (remove-if (lambda (s) (or (null s) (zerop (length (string s)))))
                          (list seed query)))
        (out '()))
    (dolist (term terms)
      (research-trace "workspace seed ~s" term)
      (setf out (append out
                        (or (ignore-errors
                              (ingest-workspace-hits ws term
                                                     :top-k top-k
                                                     :subquestion "seed"))
                            '()))))
    (research-trace "workspace seed hits=~d" (length out))
    out))

(defun ingest-workspace-hits (ws query &key (top-k 4) subquestion)
  "Search the tree and record-research-source each hit as :workspace."
  (let ((root (research-tree-root ws)))
    (unless root
      (return-from ingest-workspace-hits nil))
    (research-trace "workspace walk ~a" (pathlib:as-posix root))
    (loop for hit in (search-research-tree root query :top-k top-k)
          for rel = (getf hit :rel)
          for text = (or (read-research-tree-file root rel) "")
          for n from 1
          collect (record-research-source
                   ws
                   :id (format nil "ws-~a-~d" (or subquestion "src") n)
                   :uri (workspace-resource-uri rel)
                   :title rel
                   :text text
                   :subquestion subquestion
                   :kind :workspace))))

(defun %register-workspace-resources (ws server)
  (mcp:register-resource
   server
   (mcp:make-mcp-resource
    "workspace://"
    :name "workspace"
    :title "Local workspace tree"
    :description "Jailed checkout. Read workspace://<relpath>."
    :mime-type "text/plain"
    :handler (lambda (res)
               (declare (ignore res))
               (workspace-catalog-text ws))))
  (mcp:register-resource-template
   server
   (mcp:make-mcp-resource-template
    "workspace://{path}"
    :name "workspace-file"
    :title "Workspace file"
    :description "Text file under the jailed research tree root"
    :mime-type "text/plain"
    :complete (lambda (name value)
                (declare (ignore name))
                (let ((prefix (or value "")))
                  (loop for rel in (list-research-tree-files
                                    (research-tree-root ws))
                        when (eql (search prefix rel) 0)
                          collect rel)))))
  (dolist (rel (list-research-tree-files (research-tree-root ws)))
    (let ((uri (workspace-resource-uri rel)))
      (mcp:register-resource
       server
       (mcp:make-mcp-resource
        uri
        :name rel
        :title rel
        :description "Local workspace file"
        :mime-type "text/plain"
        :handler (lambda (res)
                   (declare (ignore res))
                   (read-research-tree-file (research-tree-root ws) rel)))))))

(defmethod mcp:read-resource ((server research-mcp-server) uri &key)
  (if (workspace-resource-uri-p uri)
      (let* ((ws (research-mcp-server-workspace server))
             (rel (workspace-uri-relpath uri))
             (root (and ws (research-tree-root ws)))
             (text (cond
                     ((or (null rel) (zerop (length rel)))
                      (workspace-catalog-text ws))
                     (root (read-research-tree-file root rel))
                     (t ""))))
        (mcp:json-object "contents"
                         (vector (mcp:json-object "uri" uri
                                                  "mimeType" "text/plain"
                                                  "text" (or text "")))))
      (call-next-method)))

(defun make-research-workspace (&key name board store instructions domain
                                  clip-chars mcp tree-root)
  (let* ((board (or board (bb:make-blackboard)))
         (store (or store (rag:make-mock-vector-store :dimension *research-embed-dim*)))
         (merged (merge-research-instructions instructions))
         (expert (%domain-expert-instructions domain))
         (root (or (and tree-root (%existing-dir-pathname tree-root))
                   (%tree-root-from-domain domain)
                   (and (or (%env-nonempty "DEMIURGE_WORKSPACE")
                            (%env-nonempty "CL_WORKSPACE"))
                        (resolve-research-tree-root)))))
    (when expert
      (setf (getf merged :expert) expert))
    (%finish-research-workspace
     (make-instance 'research-workspace
                    :name (or name "research")
                    :board board
                    :store store
                    :instructions merged
                    :clip-chars (or clip-chars *default-research-clip-chars*)
                    :tree-root root
                    :mcp mcp)
     :mcp mcp)))

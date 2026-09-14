(in-package #:demiurge/cli)

;;; Parse argv, load expert.toml, call existing GFs. No product logic here.

(defvar *serve-start* t
  "Bound to NIL in tests so SERVE-EXPERT does not open sockets.")

(defun %require-option (opts key usage)
  (or (cli:get-option opts key)
      (error 'cli:cli-usage-error :message usage)))

(defun %require-arg (free usage)
  (or (first free)
      (error 'cli:cli-usage-error :message usage)))

(defun %join-free (free)
  (cond
    ((null free) nil)
    ((= 1 (length free)) (first free))
    (t (format nil "~{~a~^ ~}" free))))

(defun %load-domain (path)
  "CLI always registers the loaded domain."
  (load-expert-config path :register t))

(defun %resolve-expert (spec &key base-dir)
  "SPEC is a path (absolute, or relative to BASE-DIR) or a registry name."
  (let* ((spec (string spec))
         (merged (and base-dir
                      (merge-pathnames spec
                                       (uiop:ensure-directory-pathname base-dir)))))
    (cond
      ((and merged (probe-file merged))
       (load-expert-config merged :register t))
      ((probe-file spec)
       (load-expert-config spec :register t))
      (t (require-expert spec)))))

(defun %map-transports (value)
  "CLI --transport mcp|a2a|ag-ui|all → serve-expert :stdio / :http."
  (let ((token (string-downcase (string (or value "all")))))
    (cond
      ((member token '("mcp" "stdio") :test #'equal) '(:stdio))
      ((member token '("a2a" "ag-ui" "http") :test #'equal) '(:http))
      ((equal token "all") '(:stdio :http))
      (t (list (intern (string-upcase token) :keyword))))))

(defun %citation-id (item)
  (cond
    ((null item) nil)
    ((stringp item) item)
    ((and (consp item) (keywordp (first item)))
     (or (getf item :block-id)
         (getf item :target)
         (getf item :id)))
    (t (princ-to-string item))))

(defun %board-citations (board)
  (when (typep board 'bb:blackboard)
    (let ((raw (or (ignore-errors (bb:read-section board :citations :default nil))
                   (ignore-errors (bb:read-section board :citation :default nil)))))
      (remove nil
              (mapcar #'%citation-id
                      (cond
                        ((null raw) nil)
                        ((and (listp raw) (not (keywordp (first raw))))
                         raw)
                        (t (list raw))))))))

(defun %print-ask (text fid board)
  (format t "~a~%" (or text ""))
  (when fid
    (format t "feedback-id: ~a~%" fid))
  (dolist (id (%board-citations board))
    (format t "citation: ~a~%" id)))

(defun %out-format (path)
  (let ((type (and path (string-downcase (or (pathname-type (pathname path)) "")))))
    (if (equal type "pdf") :pdf :markdown)))

(defun %document-from-markdown (text)
  (let ((doc (doc:make-extracted-document
              :blocks (list (doc:make-text-block :kind :para
                                                 :text (or text ""))))))
    (ignore-errors (doc:ensure-ids doc))
    doc))

(defun %write-payload (payload path)
  (cond
    ((or (stringp payload) (null payload))
     (with-open-file (out path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
       (write-string (or payload "") out)))
    ((and (vectorp payload) (not (stringp payload)))
     (with-open-file (out path :direction :output :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
       (write-sequence payload out)))
    (t
     (with-open-file (out path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
       (princ payload out)))))

(defun %write-research (result out)
  (let* ((md (or (getf result :markdown) (getf result :document-text) ""))
         (fmt (%out-format out))
         (doc (or (getf result :document)
                  (getf result :extracted-document))))
    (cond
      ((null out)
       (write-string md)
       (terpri)
       md)
      ((eq fmt :markdown)
       (%write-payload md out)
       md)
      (t
       (let ((payload (wf:render-research-document
                       (or doc (%document-from-markdown md))
                       :format :pdf)))
         (%write-payload payload out)
         payload)))))

(defun %ref-name (ref)
  (let ((s (etypecase ref
             (pathname (namestring ref))
             (string ref)
             (t (princ-to-string ref)))))
    (string-downcase s)))

(defun %ingest-source-for (domain &key name)
  (let* ((refs (or (expert-corpora domain)
                   (error 'expert-config-error
                          :message "no corpus sources in expert config")))
         (ref (if name
                  (or (find-if (lambda (r)
                                 (search (string-downcase (string name))
                                         (%ref-name r)
                                         :test #'char-equal))
                               refs)
                      (error 'expert-config-error
                             :message (format nil "unknown corpus source ~s" name)))
                  (first refs))))
    (ingest:make-file-source :root ref)))

(defun %keywordize (value)
  (intern (string-upcase (string value)) :keyword))

(defun %run-improve (domain on-error)
  (flet ((invoke (c)
           (case (%keywordize (or on-error "abort"))
             (:promote (or (improve:invoke-promote c)
                           (improve:invoke-defer c)))
             (:demote (or (improve:invoke-demote c)
                          (improve:invoke-defer c)))
             (:defer (improve:invoke-defer c))
             (t nil))))
    (if on-error
        (handler-bind ((improve:improvement-decision #'invoke)
                       (error #'invoke))
          (improve:run-improvement-cycle domain))
        (improve:run-improvement-cycle domain))))

(defun %ensure-toml ()
  (or toml:*toml-backend*
      (let ((fn (find-symbol "USE-TOMLET-BACKEND" :toml-backend-tomlet)))
        (if (and fn (fboundp fn))
            (funcall fn)
            (error 'expert-config-error
                   :message "load toml-backend-tomlet to decode demo.toml")))))

(defun %table-get (table &rest keys)
  (when (hash-table-p table)
    (dolist (key keys)
      (let ((v (or (gethash key table)
                   (gethash (string-downcase (string key)) table))))
        (when v (return v))))))

(defun %query-lines (path)
  (when (and path (probe-file path))
    (loop for line in (uiop:read-file-lines path)
          for trimmed = (string-trim '(#\Space #\Tab #\Return #\Newline) line)
          unless (or (zerop (length trimmed))
                     (char= (char trimmed 0) #\#))
            collect trimmed)))

(defun %prefix-eq (line prefix)
  (let ((n (length prefix)))
    (and (>= (length line) n)
         (string-equal prefix line :end2 n))))

(defun %split-query (line default-cmd)
  (cond
    ((%prefix-eq line "research:")
     (values :research (string-trim '(#\Space #\Tab) (subseq line 9))))
    ((%prefix-eq line "improve:")
     (values :improve (string-trim '(#\Space #\Tab) (subseq line 8))))
    ((%prefix-eq line "ingest:")
     (values :ingest (string-trim '(#\Space #\Tab) (subseq line 7))))
    ((%prefix-eq line "ask:")
     (values :ask (string-trim '(#\Space #\Tab) (subseq line 4))))
    (t (values default-cmd line))))

(defun %demo-tier (value)
  "auto|live|mock (default auto)."
  (let ((token (string-downcase (string (or value "auto")))))
    (cond
      ((member token '("live" "local" "lmstudio" "llama" "searx" "searxng")
               :test #'equal)
       :live)
      ((member token '("mock" "scripted") :test #'equal) :mock)
      (t :auto))))

(defun %demo-narration (value)
  "quiet|normal|verbose (default normal)."
  (let ((token (string-downcase (string (or value "normal")))))
    (cond
      ((member token '("quiet" "off" "none") :test #'equal) :quiet)
      ((member token '("verbose" "full") :test #'equal) :verbose)
      (t :normal))))

(defun %demo-defaults (dir)
  (let ((demo (merge-pathnames "demo.toml" dir)))
    (if (probe-file demo)
        (progn
          (%ensure-toml)
          (let* ((table (toml:decode demo))
                 (expert (or (%table-get table "expert" "config")
                             "expert.toml"))
                 (queries (or (%table-get table "queries") "queries.md"))
                 (command (or (%table-get table "command") "ask"))
                 (fixtures (%table-get table "websearch-fixtures" "fixtures")))
            (list :dir dir
                  :demo demo
                  :expert expert
                  :queries (merge-pathnames queries dir)
                  :command (%keywordize command)
                  :llm (%demo-tier (%table-get table "llm"))
                  :websearch (%demo-tier (%table-get table "websearch"))
                  :narration (%demo-narration
                              (%table-get table "narration" "verbosity"))
                  :websearch-fixtures
                  (when fixtures (merge-pathnames fixtures dir)))))
        (list :dir dir
              :demo nil
              :expert "expert.toml"
              :queries (merge-pathnames "queries.md" dir)
              :command :ask
              :llm :auto
              :websearch :auto
              :narration :normal
              :websearch-fixtures nil))))

(defun %demo-narrate (spec fmt &rest args)
  (unless (eq (getf spec :narration) :quiet)
    (format t "~&~%── ~?~%" fmt args)
    (finish-output)))

(defun %demo-look-at (spec fmt &rest args)
  (when (eq (getf spec :narration) :verbose)
    (format t "~&   look at: ~?~%" fmt args)
    (finish-output)))

(defun %demo-kv (spec key value)
  (unless (eq (getf spec :narration) :quiet)
    (format t "~&   ~A: ~S~%" key value)
    (finish-output)))

(defun %print-improve (spec result)
  (%demo-kv spec "verdict" (or (getf result :verdict) :unknown))
  (%demo-kv spec "cycle-id" (getf result :cycle-id))
  (%demo-kv spec "baseline-score" (getf result :baseline-score))
  (%demo-kv spec "candidate-score" (getf result :candidate-score))
  (%demo-kv spec "eval-run-id" (getf result :eval-run-id))
  result)

(defun %print-ingest (spec result)
  (%demo-kv spec "hashes" (length (or (getf result :hashes) '())))
  result)

(defun %board-sections (board)
  (when (typep board 'bb:blackboard)
    (ignore-errors (bb:list-sections board))))

(defun %print-ask-verbose (spec board)
  (when (and (eq (getf spec :narration) :verbose) board)
    (let ((keys (%board-sections board)))
      (%demo-kv spec "section keys" keys)
      (dolist (key keys)
        (%demo-kv spec key (bb:read-section board key :default nil))))))

(defun cmd-serve (opts free)
  (declare (ignore free))
  (let* ((path (%require-option opts :config "--config is required"))
         (domain (%load-domain path))
         (transports (%map-transports (cli:get-option opts :transport)))
         (host (or (cli:get-option opts :host) "127.0.0.1"))
         (port (or (cli:get-option opts :port) 8080)))
    (format t "Serving ~a transports ~{~a~^,~} ~a:~a~%"
            (expert-name domain) transports host port)
    (serve:serve-expert domain
                        :transports transports
                        :host host
                        :port port
                        :start *serve-start*)))

(defun cmd-ask (opts free)
  (let* ((path (%require-option opts :config "--config is required"))
         (question (or (%join-free free)
                       (error 'cli:cli-usage-error
                              :message "ask requires a question")))
         (domain (%load-domain path)))
    (multiple-value-bind (text fid board)
        (serve:ask-expert domain question)
      (%print-ask text fid board)
      (values text fid board))))

(defun %call-research (domain topic &rest keys)
  "Call RUN-DEEP-RESEARCH. Plan/gap output errors retry then fail;
   they are not ignored."
  (apply #'wf:run-deep-research domain topic keys))

(defun cmd-research (opts free)
  (let* ((path (%require-option opts :config "--config is required"))
         (topic (or (%join-free free)
                    (error 'cli:cli-usage-error
                           :message "research requires a topic")))
         (rounds (cli:get-option opts :rounds))
         (out (cli:get-option opts :out))
         (domain (%load-domain path))
         (result (if rounds
                     (%call-research domain topic :max-rounds rounds)
                     (%call-research domain topic))))
    (format t "research ~a verdict ~a~%"
            (expert-name domain)
            (or (getf result :verdict) :unknown))
    (%write-research result out)
    result))

(defun cmd-ingest (opts free)
  (declare (ignore free))
  (let* ((path (%require-option opts :config "--config is required"))
         (name (cli:get-option opts :source))
         (domain (%load-domain path))
         (source (%ingest-source-for domain :name name))
         (result (ingest:run-ingest domain source)))
    (format t "ingest ~a: ~a hashes~%"
            (expert-name domain)
            (length (or (getf result :hashes) '())))
    result))

(defun cmd-improve (opts free)
  (declare (ignore free))
  (let* ((path (%require-option opts :config "--config is required"))
         (cycles (max 1 (or (cli:get-option opts :cycles) 1)))
         (on-error (cli:get-option opts :on-error))
         (domain (%load-domain path))
         (results '()))
    (dotimes (i cycles)
      (let ((result (%run-improve domain on-error)))
        (format t "improve cycle ~d/~d: ~a~%"
                (1+ i) cycles (or (getf result :verdict) :unknown))
        (push result results)))
    (nreverse results)))

(defun cmd-install (opts free)
  (declare (ignore opts))
  (let* ((ref (%require-arg free "install requires a bundle-ref"))
         (result (bundle:install-expert ref)))
    (format t "installed ~a~%" (or (getf result :name) ref))
    result))

(defun %fixture-hits (path)
  "Load [[hit]] rows from a TOML fixtures file → list of query/url/title/snippet."
  (when (and path (probe-file path))
    (%ensure-toml)
    (let* ((table (toml:decode path))
           (rows (or (%table-get table "hit" "hits") '()))
           (list (cond
                   ((listp rows) rows)
                   ((and (vectorp rows) (not (stringp rows)))
                    (coerce rows 'list))
                   (t (list rows)))))
      (mapcar (lambda (row)
                (list :query (or (%table-get row "query") "")
                      :url (or (%table-get row "url") "")
                      :title (or (%table-get row "title" "query") "")
                      :snippet (or (%table-get row "snippet") "")))
              list))))

(defun %bind-mock-websearch (spec)
  "Bind fixtures-backed mock websearch only when the tier is :mock.
   Live/auto use [websearch] from expert.toml (load-expert-config)."
  (when (eq (getf spec :websearch) :mock)
    (let ((hits (%fixture-hits (getf spec :websearch-fixtures))))
      (setf web:*websearch-backend*
            (web:make-mock-websearch-backend
             :handler
             (lambda (backend query &key &allow-other-keys)
               (declare (ignore backend))
               (let ((row (or (find query hits :key (lambda (h) (getf h :query))
                                    :test #'string-equal)
                              (first hits))))
                 (when row
                   (list (web:make-search-hit
                          :url (getf row :url)
                          :title (getf row :title)
                          :snippet (getf row :snippet)
                          :rank 1
                          :source "mock"))))))))))

(defun %run-demo-query (spec domain cmd question)
  (ecase cmd
    ((:boot :resume :corporate)
     (error 'cli:cli-usage-error
            :message (format nil "command ~a is parity-specific; use demos/runner.lisp"
                             cmd)))
    (:ask
     (%demo-look-at spec "board :result write and citation :block-id metadata")
     (multiple-value-bind (text fid board)
         (serve:ask-expert domain question)
       (%print-ask text fid board)
       (%print-ask-verbose spec board)
       (list :command :ask :text text :feedback-id fid
             :citations (%board-citations board))))
    (:research
     (%demo-look-at spec "verdict, child answers, citations, rendered report")
     (let ((got (if (eq (getf spec :llm) :mock)
                    (%call-research domain question
                                    :llm (llm:make-mock-llm-backend))
                    (%call-research domain question))))
       (%demo-kv spec "verdict" (or (getf got :verdict) :unknown))
       (%demo-kv spec "child count" (length (getf got :children)))
       (when (getf got :markdown)
         (format t "~a~%" (getf got :markdown)))
       (list* :command :research got)))
    (:improve
     (%demo-look-at spec "gate verdict, baseline vs candidate scores")
     (let ((got (%run-improve domain nil)))
       (%print-improve spec got)
       (list* :command :improve got)))
    (:ingest
     (%demo-look-at spec "ingest hashes from the named (or first) corpus source")
     (let* ((source (%ingest-source-for
                     domain
                     :name (and question (plusp (length question)) question)))
            (got (ingest:run-ingest domain source)))
       (%print-ingest spec got)
       (list* :command :ingest got)))))

(defun %apply-demo-env (spec)
  "DEMIURGE_PARITY_DEMO_LLM / _WEBSEARCH override demo.toml tiers when set."
  (let ((env-llm (uiop:getenv "DEMIURGE_PARITY_DEMO_LLM"))
        (env-ws (uiop:getenv "DEMIURGE_PARITY_DEMO_WEBSEARCH")))
    (when (and env-llm (plusp (length env-llm)))
      (setf (getf spec :llm) (%demo-tier env-llm)))
    (when (and env-ws (plusp (length env-ws)))
      (setf (getf spec :websearch) (%demo-tier env-ws))))
  spec)

(defun cmd-demo (opts free)
  (declare (ignore opts))
  (let* ((dir (uiop:ensure-directory-pathname
               (%require-arg free "demo requires a directory")))
         (spec (%apply-demo-env (%demo-defaults dir)))
         (domain (%resolve-expert (getf spec :expert) :base-dir dir))
         (default-cmd (getf spec :command))
         (queries (%query-lines (getf spec :queries)))
         (results '()))
    (%demo-narrate spec "demo ~a" dir)
    (%demo-kv spec "expert" (expert-name domain))
    (%demo-kv spec "command" default-cmd)
    (%demo-kv spec "llm" (getf spec :llm))
    (%demo-kv spec "websearch" (getf spec :websearch))
    (let ((sum (profile-backend-summary (expert-profile domain))))
      (when sum
        (%demo-kv spec "llm-model" (getf sum :model))
        (%demo-kv spec "llm-class" (getf sum :llm-class))
        (%demo-kv spec "llm-providers" (getf sum :providers))))
    (when web:*websearch-backend*
      (%demo-kv spec "websearch-class" (type-of web:*websearch-backend*))
      (when (web:searxng-backend-p web:*websearch-backend*)
        (%demo-kv spec "websearch-url" (web:searxng-base-url web:*websearch-backend*))))
    (%bind-mock-websearch spec)
    (when (eq (getf spec :narration) :quiet)
      (format t "demo ~a expert ~a command ~a~%"
              dir (expert-name domain) default-cmd))
    (unless queries
      (format t "no queries in ~a~%" (getf spec :queries)))
    (dolist (line queries)
      (multiple-value-bind (cmd question)
          (%split-query line default-cmd)
        (%demo-narrate spec "~a ~s" cmd question)
        (push (%run-demo-query spec domain cmd question) results)))
    (nreverse results)))

(defun make-app ()
  (cli-backend-clingon:use-clingon-backend)
  (cli:make-command
   :name "demiurge"
   :description "Drive a demiurge expert from the shell."
   :version "0.3.6"
   :subcommands
   (list
    (cli:make-command
     :name "serve"
     :description "Serve an expert over MCP / HTTP (A2A + AG-UI)."
     :options
     (list (cli:make-option :name "config" :short #\c :long "config"
                            :kind :string :key :config
                            :help "Path to expert.toml")
           (cli:make-option :name "transport" :short #\t :long "transport"
                            :kind :string :key :transport :default "all"
                            :help "mcp|a2a|ag-ui|all (mcp→stdio, others→http)")
           (cli:make-option :name "host" :long "host"
                            :kind :string :key :host :default "127.0.0.1"
                            :help "HTTP bind host")
           (cli:make-option :name "port" :short #\p :long "port"
                            :kind :integer :key :port :default 8080
                            :help "HTTP bind port"))
     :handler #'cmd-serve)
    (cli:make-command
     :name "ask"
     :description "One-shot ask-expert; print result and citations."
     :options
     (list (cli:make-option :name "config" :short #\c :long "config"
                            :kind :string :key :config
                            :help "Path to expert.toml"))
     :handler #'cmd-ask)
    (cli:make-command
     :name "research"
     :description "Run deep research and write a report."
     :options
     (list (cli:make-option :name "config" :short #\c :long "config"
                            :kind :string :key :config
                            :help "Path to expert.toml")
           (cli:make-option :name "rounds" :short #\r :long "rounds"
                            :kind :integer :key :rounds :default 2
                            :help "Max research rounds")
           (cli:make-option :name "out" :short #\o :long "out"
                            :kind :string :key :out
                            :help "report.md or report.pdf"))
     :handler #'cmd-research)
    (cli:make-command
     :name "ingest"
     :description "Ingest the first (or named) corpus source."
     :options
     (list (cli:make-option :name "config" :short #\c :long "config"
                            :kind :string :key :config
                            :help "Path to expert.toml")
           (cli:make-option :name "source" :short #\s :long "source"
                            :kind :string :key :source
                            :help "Corpus source name (path/spec substring)"))
     :handler #'cmd-ingest)
    (cli:make-command
     :name "improve"
     :description "Run N improvement cycles."
     :options
     (list (cli:make-option :name "config" :short #\c :long "config"
                            :kind :string :key :config
                            :help "Path to expert.toml")
           (cli:make-option :name "cycles" :long "cycles"
                            :kind :integer :key :cycles :default 1
                            :help "Number of run-improvement-cycle loops")
           (cli:make-option :name "on-error" :long "on-error"
                            :kind :string :key :on-error
                            :help "promote|demote|abort (existing restarts)"))
     :handler #'cmd-improve)
    (cli:make-command
     :name "install"
     :description "Install an expert bundle (path / layout / registry ref)."
     :handler #'cmd-install)
    (cli:make-command
     :name "demo"
     :description "Narrated ask/research/improve/ingest runner over a demo directory."
     :handler #'cmd-demo))))

(defun %command-for-argv (command argv)
  (or (and (first argv)
           (find (first argv) (cli:cli-command-subcommands command)
                 :key #'cli:cli-command-name :test #'string-equal))
      command))

(defun %invoke (command argv)
  "PARSE then call the matching handler. Avoid CLI:RUN — it rewrites
   product conditions as CLI-PARSE-ERROR."
  (multiple-value-bind (opts free)
      (cli:parse command argv)
    (let* ((sub (%command-for-argv command argv))
           (handler (cli:cli-command-handler sub)))
      (unless handler
        (error 'cli:cli-usage-error
               :message (format nil "no handler for command ~a"
                                (cli:cli-command-name sub))))
      (funcall handler opts free))))

(defun run-cli (argv &key (command (make-app)))
  "Parse + run ARGV. Returns an exit status (0/1/2) without UIOP:QUIT."
  (handler-case
      (progn
        (%invoke command (%cli-argv argv))
        0)
    (cli:cli-exit (e)
      (cli:cli-exit-code e))
    (cli:cli-parse-error (e)
      (format *error-output* "~&~a~%" e)
      2)
    (cli:cli-usage-error (e)
      (format *error-output* "~&~a~%" e)
      2)
    (unknown-expert (e)
      (format *error-output* "~&~a~%" e)
      1)
    (expert-config-error (e)
      (format *error-output* "~&~a~%" e)
      1)
    (error (e)
      (format *error-output* "~&~a~%" e)
      1)))

(defun %cli-argv (argv)
  "Drop a leading -- that SBCL/UIOP leaves in COMMAND-LINE-ARGUMENTS."
  (let ((argv (or argv (uiop:command-line-arguments))))
    (if (and argv (equal (first argv) "--"))
        (rest argv)
        argv)))

(defun main (&optional argv)
  (uiop:quit (run-cli (or argv (uiop:command-line-arguments)))))

(in-package #:demiurge/workflows)

(defparameter *research-embed-dim* 32)

(defparameter *default-research-clip-chars* 1000
  "Per-source clip for child / gap / synthesize prompts. Full text stays on the board.")

(defparameter *default-research-instructions*
  '(:plan
    "You are the planning KS of a Common Lisp expert-system researcher.
The local checkout is mounted at workspace:// — check that tree first.
Seed the plan with at least one subquestion that searches workspace:// for the
named symbols, files, and functions (do not expand acronyms). Prefer local
evidence over the web.
Decompose the user question into 2–5 short, independently searchable subquestions.
Interpret CL as Common Lisp unless the user says otherwise.
Return only a schema-typed research-plan JSON object:
{\"question\":string,\"subquestions\":[{\"id\":string,\"question\":string,\"rationale\":string}]}
Do not answer the question yourself. Do not invent sources or expand acronyms you were not given."

    :child
    "You are a research child KS answering ONE subquestion.
Check the local workspace first: prefer workspace:// file hits over web pages.
Use only the retrieved workspace sources in the user turn. Quote or paraphrase briefly.
Cite each claim as [src-id] and keep the URL next to the first cite.
Write 3–8 sentences. Never dump full page text, HTML, or PDF extracts.
If no workspace:// source is in the retrieved set, say so. If the sources are
insufficient, list what is missing."

    :gap
    "You are the gap-analysis KS.
Given the original question and the child answers, emit new subquestions only for missing evidence.
If children did not cite workspace:// sources for a local mechanism, emit a
subquestion that searches the checkout for it.
Return a research-plan JSON object. An empty subquestions array means no gaps.
Do not rewrite existing answers. Do not invent sources."

    :synthesize
    "You are the synthesis KS.
Write a cited briefing from the child answers and the source catalog.
Stay grounded: every factual sentence must be supportable by a [src-id].
Prefer workspace:// cites for product internals; say when a claim is web-only.
Do not invent organizations, products, or expansions of acronyms.
Lead with the answer, then per-subquestion findings, then open questions. Keep it under ~400 words."

    :expert
    "You are a cl-stack / Common Lisp expert attached to this research run.
Always check the local workspace (workspace://) before web or prior knowledge.
Prefer workspace sources (research://source/<id> and workspace://<relpath>).
Use lookup-symbol / search-corpus when those tools exist. Cite src-ids. Do not guess.")
  "Initial system prompts for each deep-research step and the attached expert.")

(defun merge-research-instructions (&optional override)
  "Defaults with OVERRIDE plist keys replacing matching steps."
  (let ((base (copy-list *default-research-instructions*)))
    (loop for (k v) on override by #'cddr
          do (setf (getf base k) v))
    base))

(defclass research-workspace ()
  ((name :initarg :name :accessor research-workspace-name :initform "research")
   (board :initarg :board :accessor research-workspace-board)
   (bb-workspace :initarg :bb-workspace :accessor research-workspace-bb
                 :initform nil)
   (store :initarg :store :accessor research-workspace-store :initform nil)
   (sources :initform nil :accessor research-workspace-sources)
   (mcp :initarg :mcp :accessor research-workspace-mcp :initform nil)
   (instructions :initarg :instructions :accessor research-workspace-instructions
                 :initform nil)
   (clip-chars :initarg :clip-chars :accessor research-workspace-clip-chars
               :initform *default-research-clip-chars*)
   (tree-root :initarg :tree-root :accessor research-workspace-tree-root
              :initform nil)
   (source-counter :initform 0 :accessor research-workspace-source-counter)))

(defun research-workspace-p (x)
  (typep x 'research-workspace))

(defun research-instruction (source step)
  "SOURCE is a research-workspace or an instructions plist."
  (let ((plist (cond
                 ((research-workspace-p source)
                  (research-workspace-instructions source))
                 ((and (consp source) (keywordp (first source))) source)
                 (t nil))))
    (or (getf plist step)
        (getf *default-research-instructions* step)
        "")))

(defun clip-research-text (text &optional (limit *default-research-clip-chars*))
  (let ((s (or text "")))
    (if (<= (length s) limit)
        s
        (format nil "~a~%… [clipped ~d chars; full text on research://source/<id>]"
                (subseq s 0 limit) (- (length s) limit)))))

(defun research-source-uri (id)
  (format nil "research://source/~a" id))

(defun research-instruction-uri (step)
  (format nil "research://instructions/~a"
          (string-downcase (string step))))

(defun %tokenize (text)
  (loop for start = 0 then (1+ pos)
        for pos = (position-if-not #'alphanumericp text :start start)
        for raw = (string-downcase
                   (if pos (subseq text start pos) (subseq text start)))
        when (plusp (length raw)) collect raw
        while pos))

(defun %bow-embed (text &optional (dim *research-embed-dim*))
  (let ((v (make-array dim :element-type 'single-float :initial-element 0.0f0)))
    (dolist (tok (%tokenize (or text "")))
      (incf (aref v (mod (sxhash tok) dim)) 1.0f0))
    (let ((norm 0.0f0))
      (loop for i from 0 below dim do (incf norm (expt (aref v i) 2)))
      (setf norm (sqrt norm))
      (when (> norm 0.0f0)
        (loop for i from 0 below dim do (setf (aref v i) (/ (aref v i) norm)))))
    v))

(defun %source-index-entry (rec)
  (list :id (getf rec :id)
        :uri (getf rec :uri)
        :title (getf rec :title)
        :chars (getf rec :chars)
        :kind (getf rec :kind)
        :subquestion (getf rec :subquestion)
        :resource-uri (getf rec :resource-uri)))

(defun research-source-catalog (ws)
  (mapcar #'%source-index-entry (research-workspace-sources ws)))

(defun %flush-sources-to-board (ws)
  (let ((board (research-workspace-board ws)))
    (when board
      (bb:write-section board :sources (copy-list (research-workspace-sources ws)))
      (bb:write-section board :source-index (research-source-catalog ws))
      (bb:write-section board :research-workspace
                        (list :name (research-workspace-name ws)
                              :source-count (length (research-workspace-sources ws))
                              :store (and (research-workspace-store ws) t)
                              :mcp (and (research-workspace-mcp ws) t)
                              :tree-root (let ((root (research-workspace-tree-root ws)))
                                           (and root (namestring root)))))
      (bb:write-section board :research-instructions
                        (copy-list (research-workspace-instructions ws)))))
  ws)

(defun %chunk-source-text (text &optional (size 1500))
  (let ((s (or text "")))
    (if (<= (length s) size)
        (list s)
        (loop for i from 0 below (length s) by size
              collect (subseq s i (min (length s) (+ i size)))))))

(defun %upsert-source-chunks (ws rec)
  (let ((store (research-workspace-store ws))
        (id (getf rec :id))
        (text (or (getf rec :text) "")))
    (when (and store id)
      (let ((chunks
             (loop for part in (%chunk-source-text text)
                   for n from 0
                   collect (rag:make-rag-chunk
                            :id (if (zerop n) id (format nil "~a#~d" id n))
                            :document-id id
                            :text part
                            :embedding (%bow-embed part)
                            :metadata (list :uri (getf rec :uri)
                                            :title (getf rec :title)
                                            :subquestion (getf rec :subquestion)
                                            :kind (getf rec :kind))))))
        (handler-bind ((error
                        (lambda (c)
                          (let ((r (find-restart 'continue c)))
                            (when r (invoke-restart r))))))
          (rag:upsert store chunks))))))

(defun %mcp-available-p ()
  (and (find-package '#:mcp-protocol)
       (find-class (find-symbol "MCP-SERVER" :mcp-protocol) nil)))

(defun %ensure-mcp-loaded ()
  (or (%mcp-available-p)
      (ignore-errors (asdf:load-system "mcp-protocol" :verbose nil)
                     (%mcp-available-p))))

(defun %mcp-resource-text (contents)
  "Unwrap READ-RESOURCE json-object → text."
  (cond
    ((stringp contents) contents)
    ((hash-table-p contents)
     (let ((vec (or (gethash "contents" contents) (gethash :contents contents))))
       (if (and vec (plusp (length vec)))
           (let ((item (elt vec 0)))
             (if (hash-table-p item)
                 (or (gethash "text" item) (gethash :text item) "")
                 (princ-to-string item)))
           (or (gethash "text" contents) ""))))
    (t (princ-to-string contents))))

(defun %register-instruction-resources (ws server)
  (dolist (step '(:plan :child :gap :synthesize :expert))
    (let ((uri (research-instruction-uri step)))
      (mcp:register-resource
       server
       (mcp:make-mcp-resource
        uri
        :name (format nil "instructions/~a" (string-downcase (string step)))
        :title (format nil "Research ~a KS instructions" step)
        :description "Initial system prompt for this research step"
        :mime-type "text/plain"
        :handler (lambda (res)
                   (declare (ignore res))
                   (research-instruction ws step)))))))

(defun %register-catalog-resource (ws server)
  (mcp:register-resource
   server
   (mcp:make-mcp-resource
    "research://catalog"
    :name "catalog"
    :title "Research source catalog"
    :description "id / uri / title / chars for every ingested page"
    :mime-type "text/plain"
    :handler (lambda (res)
               (declare (ignore res))
               (with-output-to-string (s)
                 (dolist (e (research-source-catalog ws))
                   (format s "[~a] ~a~%  ~a (~d chars)~%"
                           (getf e :id)
                           (or (getf e :title) "")
                           (or (getf e :uri) "")
                           (or (getf e :chars) 0))))))))

(defun %register-source-resource (ws server rec)
  (declare (ignore ws))
  (let ((uri (getf rec :resource-uri))
        (id (getf rec :id)))
    (mcp:register-resource
     server
     (mcp:make-mcp-resource
      uri
      :name id
      :title (or (getf rec :title) id)
      :description (or (getf rec :uri) "")
      :mime-type "text/plain"
      :handler (lambda (res)
                 (declare (ignore res))
                 (or (getf rec :text) ""))))))

(defclass research-mcp-server (mcp:mcp-server)
  ((workspace :initarg :workspace :accessor research-mcp-server-workspace
              :initform nil)))

(defun research-mcp-server-p (x)
  (typep x 'research-mcp-server))

(defun ensure-research-mcp-server (ws &key (force nil))
  "In-process MCP server exposing instructions + catalog + sources + workspace://."
  (when (or force (null (research-workspace-mcp ws)))
    (when (%ensure-mcp-loaded)
      (let ((server (make-instance 'research-mcp-server
                                   :name (or (research-workspace-name ws) "research")
                                   :version "0.3.6"
                                   :workspace ws
                                   :instructions (mcp-server-instructions-for ws))))
        (%register-instruction-resources ws server)
        (%register-catalog-resource ws server)
        (%register-workspace-resources ws server)
        (dolist (rec (research-workspace-sources ws))
          (%register-source-resource ws server rec))
        (setf (research-workspace-mcp ws) server))))
  (research-workspace-mcp ws))

(defun mcp-server-instructions-for (ws)
  (format nil "Demiurge research workspace ~a.
Step system prompts: research://instructions/{plan,child,gap,synthesize,expert}.
Fetched pages: research://source/<id>. Catalog: research://catalog.
Local checkout: workspace:// and workspace://<relpath>.
Retrieve with retrieve-research-sources (RAG) or MCP read-resource."
          (research-workspace-name ws)))

(defun record-research-source (ws &key id uri title text subquestion kind)
  "Append a fetched/downloaded page to the board, RAG store, and MCP catalog."
  (check-type ws research-workspace)
  (let* ((id (or id (format nil "src-~d"
                            (incf (research-workspace-source-counter ws)))))
         (text (or text ""))
         (rec (list :id id
                    :uri uri
                    :title title
                    :text text
                    :subquestion subquestion
                    :kind (or kind :web)
                    :chars (length text)
                    :resource-uri (research-source-uri id))))
    (setf (research-workspace-sources ws)
          (append (research-workspace-sources ws) (list rec)))
    (%upsert-source-chunks ws rec)
    (let ((mcp (research-workspace-mcp ws)))
      (when mcp
        (%register-source-resource ws mcp rec)))
    (%flush-sources-to-board ws)
    rec))

(defun %lexical-score (query text)
  (let* ((q (remove-duplicates (%tokenize query) :test #'string=))
         (hay (string-downcase (or text "")))
         (n (length q)))
    (if (zerop n)
        0.0
        (/ (count-if (lambda (tok) (search tok hay)) q) (float n)))))

(defun retrieve-research-sources (ws query &key (top-k 4))
  "RAG-style retrieve over workspace sources. Cosine on bag-of-words, lexical fallback."
  (check-type ws research-workspace)
  (let* ((k (or top-k 4))
         (store (research-workspace-store ws))
         (hits (when store
                 (ignore-errors
                   (rag:query-store store (%bow-embed query) :top-k k))))
         (from-store
          (loop for hit in (or hits nil)
                for chunk = (and (rag:rag-hit-p hit) (rag:rag-hit-chunk hit))
                for id = (and chunk (or (rag:rag-chunk-document-id chunk)
                                        (rag:rag-chunk-id chunk)))
                for rec = (find id (research-workspace-sources ws)
                                :key (lambda (s) (getf s :id))
                                :test #'equal)
                when rec
                  collect (append rec (list :score (rag:rag-hit-score hit)
                                            :chunk-text (rag:rag-chunk-text chunk))))))
    (if from-store
        from-store
        (let ((ranked (sort (copy-list (research-workspace-sources ws)) #'>
                            :key (lambda (s)
                                   (%lexical-score query (getf s :text))))))
          (subseq ranked 0 (min k (length ranked)))))))

(defun list-research-resources (ws)
  "MCP list-resources when a server is bound; otherwise a plist catalog."
  (let ((mcp (research-workspace-mcp ws)))
    (if mcp
        (mcp:list-resources mcp)
        (append
         (list (list :uri "research://catalog" :name "catalog"))
         (loop for step in '(:plan :child :gap :synthesize :expert)
               collect (list :uri (research-instruction-uri step)
                             :name (format nil "instructions/~a"
                                           (string-downcase (string step)))))
         (loop for rec in (research-workspace-sources ws)
               collect (list :uri (getf rec :resource-uri)
                             :name (getf rec :id)))
         (when (research-tree-root ws)
           (list* (list :uri "workspace://" :name "workspace")
                  (loop for rel in (list-research-tree-files
                                    (research-tree-root ws))
                        collect (list :uri (workspace-resource-uri rel)
                                      :name rel))))))))

(defun read-research-resource (ws uri)
  "MCP read-resource when a server is bound; otherwise board/source text."
  (let ((mcp (research-workspace-mcp ws)))
    (if mcp
        (mcp:read-resource mcp uri)
        (cond
          ((equal uri "research://catalog")
           (with-output-to-string (s)
             (dolist (e (research-source-catalog ws))
               (format s "[~a] ~a~%" (getf e :id) (or (getf e :uri) "")))))
          ((eql (search "research://instructions/" uri) 0)
           (research-instruction
            ws (intern (string-upcase (subseq uri (length "research://instructions/")))
                       :keyword)))
          ((eql (search "research://source/" uri) 0)
           (let* ((id (subseq uri (length "research://source/")))
                  (rec (find id (research-workspace-sources ws)
                             :key (lambda (s) (getf s :id)) :test #'equal)))
             (or (getf rec :text) "")))
          ((workspace-resource-uri-p uri)
           (let ((rel (workspace-uri-relpath uri))
                 (root (research-tree-root ws)))
             (cond
               ((or (null rel) (zerop (length rel)))
                (workspace-catalog-text ws))
               (root (read-research-tree-file root rel))
               (t ""))))
          (t "")))))

(defparameter *research-output-attempts* 3
  "GENERATE attempts for a schema-typed research step before RESEARCH-ERROR.")

(defvar *research-trace-stream* nil
  "When bound, research LLM / web / workspace steps print live progress here.
   Callers must force-output; run-demo.sh pipes SBCL through tee.")

(defun research-trace (fmt &rest args)
  "Write one progress line and flush. No-op when *RESEARCH-TRACE-STREAM* is NIL."
  (let ((s *research-trace-stream*))
    (when s
      (apply #'format s "~&── ~@?~%" fmt args)
      (force-output s)
      (finish-output s))))

(defun %llm-response-text (response)
  (or (and response (llm:llm-response-text response)) ""))

(defun %research-llm-model (llm)
  (or (ignore-errors (llm:backend-model (bare-llm-backend llm)))
      (ignore-errors (llm:backend-model llm))))

(defun %research-elapsed (t0)
  (/ (- (get-internal-real-time) t0) internal-time-units-per-second))

(defun generate-research-step (llm step user-text &key output workspace instructions)
  "system-turn + user-turn. → llm-response.
   :OUTPUT parse/empty misses retry GENERATE. After *RESEARCH-OUTPUT-ATTEMPTS*
   the run dies with RESEARCH-ERROR carrying the raw completion. No IGNORE-OUTPUT."
  (let* ((sys (research-instruction (or workspace instructions) step))
         (turns (list (llm:system-turn sys)
                      (llm:user-turn (or user-text ""))))
         (attempts 0)
         (last-text nil)
         (model (%research-llm-model llm)))
    (loop
      (incf attempts)
      (research-trace "LLM generate ~s attempt ~d/~d~@[ model ~s~]~@[ output ~a~] (~d chars)"
                      step attempts *research-output-attempts*
                      model output (length (or user-text "")))
      (let ((t0 (get-internal-real-time)))
        (handler-case
            (let ((r (if output
                         (llm:generate llm turns :output output)
                         (llm:generate llm turns))))
              (research-trace "LLM generate ~s done ~,1fs chars=~d~:[~; structured~]"
                              step (%research-elapsed t0)
                              (length (%llm-response-text r))
                              (and output (llm:llm-response-output r)))
              (when (and output
                         (null (llm:llm-response-output r))
                         (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                     (%llm-response-text r)))))
                (error 'research-error
                       :message (format nil "~a returned an empty completion" step)))
              (return r))
          (llm:llm-output-error (c)
            (setf last-text (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         (%llm-response-text
                                          (llm:llm-output-error-response c))))
            (research-trace "LLM generate ~s output-error ~,1fs attempt ~d chars=~d"
                            step (%research-elapsed t0) attempts
                            (length (or last-text "")))
            (when (>= attempts *research-output-attempts*)
              (error 'research-error
                     :message (format nil
                                      "~a structured output failed after ~d attempt~:p~@[; completion: ~s~]"
                                      step attempts
                                      (and (plusp (length last-text)) last-text))))))))))

(defun %domain-expert-instructions (domain)
  (when (expert-domain-p domain)
    (loop for ks in (expert-ks-set domain)
          when (and (agent-ks-p ks) (agent-ks-agent ks))
            do (let ((text (agent:ai-agent-instructions (agent-ks-agent ks))))
                 (when (and text (plusp (length text)))
                   (return text))))))

(defun %finish-research-workspace (ws &key mcp)
  (unless mcp
    (ensure-research-mcp-server ws))
  (%flush-sources-to-board ws)
  ws)

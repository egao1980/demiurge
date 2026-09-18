(in-package #:demiurge/improve)

(schema:defschema ks-revision ()
  "LLM-proposed revision of a knowledge source (skill / prompt / chunker)."
  (skill-text string :optional t :default "" :accessor ks-revision-skill-text)
  (prompt string :optional t :default "" :accessor ks-revision-prompt)
  (chunk-config string :optional t :default "" :accessor ks-revision-chunk-config)
  (:key-style :kebab)
  (:extra :allow))

(defun ks-revision-p (x)
  (typep x 'ks-revision))

(defun %stringify-chunk-config (value)
  (cond
    ((null value) "")
    ((stringp value) value)
    (t (with-standard-io-syntax
         (let ((*print-pretty* nil)
               (*print-circle* nil))
           (prin1-to-string value))))))

(defun make-ks-revision (&key (skill-text "") (prompt "") chunk-config)
  (make-instance 'ks-revision
                 :skill-text (or skill-text "")
                 :prompt (or prompt "")
                 :chunk-config (%stringify-chunk-config chunk-config)))

(defun %ht-get (table key)
  (or (gethash key table)
      (gethash (string-downcase (string key)) table)
      (gethash (intern (string-upcase (string key)) :keyword) table)))

(defun coerce-ks-revision (value)
  "Accept a KS-REVISION, plist, hash-table, or skill-text string."
  (cond
    ((ks-revision-p value) value)
    ((null value) (make-ks-revision))
    ((stringp value) (make-ks-revision :skill-text value))
    ((hash-table-p value)
     (make-ks-revision :skill-text (or (%ht-get value :skill-text) "")
                       :prompt (or (%ht-get value :prompt) "")
                       :chunk-config (%ht-get value :chunk-config)))
    ((and (consp value) (keywordp (first value)))
     (make-ks-revision :skill-text (or (getf value :skill-text) "")
                       :prompt (or (getf value :prompt) "")
                       :chunk-config (getf value :chunk-config)))
    (t
     (restart-case
         (error 'improve-error
                :message (format nil "cannot coerce ~s to ks-revision" value))
       (use-value (rev)
         :report "Use a supplied ks-revision"
         (coerce-ks-revision rev))))))

(defun ks-revision-plist (revision)
  (list :skill-text (or (ks-revision-skill-text revision) "")
        :prompt (or (ks-revision-prompt revision) "")
        :chunk-config (or (ks-revision-chunk-config revision) "")))

(defun parse-chunk-config (value)
  "Parse a revision chunk-config string or plist → plist or NIL."
  (cond
    ((null value) nil)
    ((and (consp value) (keywordp (first value))) (copy-list value))
    ((stringp value)
     (when (plusp (length value))
       (or (ignore-errors
             (let ((read (with-standard-io-syntax
                           (let ((*read-eval* nil))
                             (read-from-string value)))))
               (cond
                 ((and (consp read) (keywordp (first read))) read)
                 ((integerp read) (list :size read))
                 (t (list :raw value)))))
           (list :raw value))))
    ((integerp value) (list :size value))
    (t (list :raw value))))

(defun %chunker-from-config (config)
  (let* ((parsed (parse-chunk-config config))
         (size (and parsed (or (getf parsed :size) (getf parsed :chunk-size))))
         (pkg (find-package '#:rag-backend-text))
         (make (and pkg (find-symbol "MAKE-RECURSIVE-CHARACTER-CHUNKER" pkg))))
    (when (and parsed make (fboundp make) size)
      (funcall make
               :size size
               :overlap (or (getf parsed :overlap) 0)))))

(defun %copy-ai-agent (agent &key instructions)
  (agent:make-ai-agent
   :name (agent:ai-agent-name agent)
   :backend (agent:ai-agent-backend agent)
   :instructions (or instructions (agent:ai-agent-instructions agent))
   :tools (copy-list (agent:ai-agent-tools agent))
   :handoffs (copy-list (agent:ai-agent-handoffs agent))
   :settings (agent:ai-agent-settings agent)
   :memory (agent:ai-agent-memory agent)
   :session (agent:ai-agent-session agent)
   :steering (agent:ai-agent-steering agent)))

(defun %materialize-agent-ks (ks revision catalogue)
  (let* ((prompt (and revision (ks-revision-prompt revision)))
         (agent (agent-ks-agent ks))
         (installed (and prompt (plusp (length prompt)) prompt)))
    (make-agent-ks
     :name (bb:ks-name ks)
     :agent (%copy-ai-agent agent :instructions installed)
     :watch (copy-list (agent-ks-watch ks))
     :prompt-key (agent-ks-prompt-key ks)
     :result-key (agent-ks-result-key ks)
     :memory (agent-ks-memory ks)
     :steering (agent-ks-steering ks)
     :catalogue (or catalogue *trial-restricted-catalogue* (agent-ks-catalogue ks))
     :mcp-peer (agent-ks-mcp-peer ks)
     :durability (agent-ks-durability ks)
     :priority (bb:ks-priority ks)
     :version (bb:ks-version ks))))

(defclass revised-ks (bb:knowledge-source)
  ((base :initarg :base :accessor revised-ks-base :initform nil)
   (revision :initarg :revision :accessor revised-ks-revision :initform nil)
   (catalogue :initarg :catalogue :accessor revised-ks-catalogue :initform nil)
   (chunker :initarg :chunker :accessor revised-ks-chunker :initform nil)))

(defun revised-ks-p (x)
  (typep x 'revised-ks))

(defun apply-ks-revision (ks revision &key catalogue)
  "Install REVISION as a candidate wrapper around KS.
   Prompt and chunk-config are materialized onto the candidate (the
   original KS / agent is not mutated). CATALOGUE, when supplied, is
   installed on the candidate and used for tool construction."
  (let* ((rev (coerce-ks-revision revision))
         (cat (or catalogue *trial-restricted-catalogue*))
         (base (if (agent-ks-p ks)
                   (%materialize-agent-ks ks rev cat)
                   ks)))
    (make-instance 'revised-ks
                   :name (if (typep ks 'bb:knowledge-source)
                             (bb:ks-name ks)
                             'revised)
                   :base base
                   :revision rev
                   :catalogue cat
                   :chunker (%chunker-from-config (ks-revision-chunk-config rev))
                   :priority (if (typep ks 'bb:knowledge-source)
                                 (bb:ks-priority ks)
                                 0))))

(defun %revision-prefix (rev)
  (let* ((skill (and rev (ks-revision-skill-text rev)))
         (cfg (parse-chunk-config (and rev (ks-revision-chunk-config rev))))
         (prefix (and cfg (getf cfg :prefix))))
    (concatenate 'string (or skill "") (or prefix ""))))

(defmethod ks-watch-keys ((ks revised-ks))
  (let ((base (revised-ks-base ks)))
    (if (typep base 'bb:knowledge-source)
        (ks-watch-keys base)
        '(:prompt))))

(defmethod bb:ks-precondition ((ks revised-ks) blackboard)
  (let ((base (revised-ks-base ks)))
    (if (typep base 'bb:knowledge-source)
        (bb:ks-precondition base blackboard)
        (bb:section-bound-p blackboard :prompt))))

(defmethod bb:ks-execute ((ks revised-ks) blackboard)
  (let* ((rev (revised-ks-revision ks))
         (prefix (%revision-prefix rev))
         (prompt (and rev (ks-revision-prompt rev)))
         (chunk-cfg (and rev (ks-revision-chunk-config rev)))
         (chunker (revised-ks-chunker ks))
         (input (cond
                  ((bb:section-bound-p blackboard :prompt)
                   (bb:read-section blackboard :prompt))
                  (t nil)))
         (star (find-symbol "*RAG-CHUNKER*" :rag-protocol)))
    (when (and chunk-cfg (plusp (length (string chunk-cfg))))
      (bb:write-section blackboard :chunk-config chunk-cfg))
    (let ((*trial-restricted-catalogue*
           (or (revised-ks-catalogue ks) *trial-restricted-catalogue*)))
      (flet ((run ()
               (cond
                 ((plusp (length prefix))
                  (let ((out (if (stringp input)
                                 (concatenate 'string prefix input)
                                 prefix)))
                    (bb:write-section blackboard :result out)
                    out))
                 ((and prompt (plusp (length prompt))
                       (not (agent-ks-p (revised-ks-base ks))))
                  (let ((out (if (stringp input)
                                 (concatenate 'string prompt input)
                                 prompt)))
                    (bb:write-section blackboard :result out)
                    out))
                 ((typep (revised-ks-base ks) 'bb:knowledge-source)
                  (bb:ks-execute (revised-ks-base ks) blackboard))
                 (t nil))))
        (if (and chunker star)
            (let ((old (symbol-value star)))
              (unwind-protect
                   (progn
                     (setf (symbol-value star) chunker)
                     (run))
                (setf (symbol-value star) old)))
            (run))))))

(defvar *current-ksar* nil
  "KSAR bound while a versioned-ks handler runs. Used by SELECT-VARIANT.")

(defvar *trial-force-variant* nil
  "When bound to :CURRENT or :CANDIDATE, SELECT-VARIANT returns that side.")

(defclass versioned-ks (bb:knowledge-source)
  ((current :initarg :current :accessor versioned-ks-current :initform nil)
   (candidate :initarg :candidate :accessor versioned-ks-candidate :initform nil)
   (split-ratio :initarg :split-ratio :accessor versioned-ks-split-ratio
                :initform 2)
   (cycle-id :initarg :cycle-id :accessor versioned-ks-cycle-id :initform nil)
   (force-variant :initarg :force-variant :accessor versioned-ks-force-variant
                  :initform nil)
   (observations :initarg :observations :accessor versioned-ks-observations
                 :initform nil)))

(defun versioned-ks-p (x)
  (typep x 'versioned-ks))

(defun make-versioned-ks (&key name current candidate (split-ratio 2) cycle-id
                            force-variant (priority 0) (version "0.1.0"))
  (check-type split-ratio (integer 1 *))
  (when force-variant
    (check-type force-variant keyword))
  (make-instance 'versioned-ks
                 :name (or name
                           (and current (bb:ks-name current))
                           'versioned)
                 :current current
                 :candidate candidate
                 :split-ratio split-ratio
                 :cycle-id cycle-id
                 :force-variant force-variant
                 :priority priority
                 :version version))

(defun %id-hash (id)
  "Portable FNV-1a-ish hash. Integer ids are used as-is (absolute)."
  (cond
    ((integerp id) (abs id))
    ((characterp id) (char-code id))
    (t
     (let ((s (if (stringp id)
                  id
                  (write-to-string id :escape t)))
           (h 2166136261))
       (declare (type (unsigned-byte 32) h))
       (loop for c across s
             do (setf h (ldb (byte 32 0)
                             (* (logxor h (char-code c)) 16777619))))
       h))))

(defun select-variant (vks ksar)
  "Deterministic current/candidate pick: hash(KSAR id) mod split-ratio.
   Zero remainder → :candidate, else :current. No RNG.
   *TRIAL-FORCE-VARIANT* or VERSIONED-KS-FORCE-VARIANT short-circuits."
  (check-type vks versioned-ks)
  (or *trial-force-variant*
      (versioned-ks-force-variant vks)
      (let* ((ratio (max 1 (versioned-ks-split-ratio vks)))
             (id (cond
                   ((null ksar) 0)
                   ((numberp ksar) ksar)
                   ((and (typep ksar 'bb:ksar)) (bb:ksar-id ksar))
                   (t ksar)))
             (h (%id-hash id)))
        (if (zerop (mod h ratio))
            :candidate
            :current))))

(defun %chosen-variant (vks which)
  (if (eq which :candidate)
      (or (versioned-ks-candidate vks) (versioned-ks-current vks))
      (versioned-ks-current vks)))

(defvar *observation-lock* (bt2:make-lock "demiurge-observations")
  "Serializes appends to VERSIONED-KS-OBSERVATIONS.")

(defun record-variant-observation (vks variant request actual)
  "Record an eval-protocol observation: case = request, actual = output,
   tag = variant + KS id. Append-only under *OBSERVATION-LOCK*."
  (let ((obs (eval:make-eval-case-result
              :case (eval:make-eval-case
                     :input request
                     :metadata (list :tags (list variant (bb:ks-name vks))
                                     :variant variant
                                     :ks (bb:ks-name vks)
                                     :cycle-id (versioned-ks-cycle-id vks)))
              :actual actual)))
    (bt2:with-lock-held (*observation-lock*)
      (push obs (versioned-ks-observations vks)))
    (demiurge::%observe-record "RECORD-EVAL-SCORE"
                               (bb:ks-name vks)
                               (if actual 1 0))
    obs))

(defmethod ks-watch-keys ((ks versioned-ks))
  (let ((cur (versioned-ks-current ks)))
    (if cur (ks-watch-keys cur) nil)))

(defmethod bb:ks-precondition ((ks versioned-ks) blackboard)
  (let ((cur (versioned-ks-current ks)))
    (if cur
        (bb:ks-precondition cur blackboard)
        t)))

(defun %triggering-request (blackboard ks)
  (let ((cur (versioned-ks-current ks)))
    (cond
      ((and (agent-ks-p cur)
            (bb:section-bound-p blackboard (agent-ks-prompt-key cur)))
       (bb:read-section blackboard (agent-ks-prompt-key cur)))
      ((bb:section-bound-p blackboard :prompt)
       (bb:read-section blackboard :prompt))
      (t nil))))

(defun %execute-variant (variant blackboard)
  (cond
    ((null variant) nil)
    ((functionp variant) (funcall variant blackboard))
    (t (bb:ks-execute variant blackboard))))

(defmethod bb:ks-execute ((ks versioned-ks) blackboard)
  (let* ((ksar (or *current-ksar*
                   (bb:make-ksar :id (%id-hash (%triggering-request blackboard ks)))))
         (which (select-variant ks ksar))
         (chosen (%chosen-variant ks which))
         (request (%triggering-request blackboard ks))
         (actual (%execute-variant chosen blackboard)))
    (record-variant-observation ks which request actual)
    actual))

(defmethod bb:ks-postcondition ((ks versioned-ks) blackboard result)
  (let ((cur (versioned-ks-current ks)))
    (if cur
        (bb:ks-postcondition cur blackboard result)
        t)))

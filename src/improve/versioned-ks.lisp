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

(defvar *current-ksar* nil
  "KSAR bound while a versioned-ks handler runs. Used by SELECT-VARIANT.")

(defclass versioned-ks (bb:knowledge-source)
  ((current :initarg :current :accessor versioned-ks-current :initform nil)
   (candidate :initarg :candidate :accessor versioned-ks-candidate :initform nil)
   (split-ratio :initarg :split-ratio :accessor versioned-ks-split-ratio
                :initform 2)
   (cycle-id :initarg :cycle-id :accessor versioned-ks-cycle-id :initform nil)
   (observations :initarg :observations :accessor versioned-ks-observations
                 :initform nil)))

(defun versioned-ks-p (x)
  (typep x 'versioned-ks))

(defun make-versioned-ks (&key name current candidate (split-ratio 2) cycle-id
                            (priority 0) (version "0.1.0"))
  (check-type split-ratio (integer 1 *))
  (make-instance 'versioned-ks
                 :name (or name
                           (and current (bb:ks-name current))
                           'versioned)
                 :current current
                 :candidate candidate
                 :split-ratio split-ratio
                 :cycle-id cycle-id
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
   Zero remainder → :candidate, else :current. No RNG."
  (check-type vks versioned-ks)
  (let* ((ratio (max 1 (versioned-ks-split-ratio vks)))
         (id (cond
               ((null ksar) 0)
               ((numberp ksar) ksar)
               ((and (typep ksar 'bb:ksar)) (bb:ksar-id ksar))
               (t ksar)))
         (h (%id-hash id)))
    (if (zerop (mod h ratio))
        :candidate
        :current)))

(defun %chosen-variant (vks which)
  (if (eq which :candidate)
      (or (versioned-ks-candidate vks) (versioned-ks-current vks))
      (versioned-ks-current vks)))

(defun record-variant-observation (vks variant request actual)
  "Record an eval-protocol observation: case = request, actual = output,
   tag = variant + KS id."
  (let ((obs (eval:make-eval-case-result
              :case (eval:make-eval-case
                     :input request
                     :metadata (list :tags (list variant (bb:ks-name vks))
                                     :variant variant
                                     :ks (bb:ks-name vks)
                                     :cycle-id (versioned-ks-cycle-id vks)))
              :actual actual)))
    (push obs (versioned-ks-observations vks))
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

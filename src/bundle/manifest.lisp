(in-package #:demiurge/bundle)

(schema:defschema bundle-skill-ref ()
  "Skill pinned to an A4 / steer-protocol store version plus content digest."
  (name string :accessor bundle-skill-ref-name)
  (version string :optional t :default "" :accessor bundle-skill-ref-version)
  (digest string :optional t :default "" :accessor bundle-skill-ref-digest)
  (media-type string :optional t :default "text/markdown"
              :accessor bundle-skill-ref-media-type)
  (:key-style :kebab)
  (:extra :allow))

(schema:defschema bundle-corpus-item ()
  "One corpus file: source URI plus portable content hash."
  (uri string :accessor bundle-corpus-item-uri)
  (digest string :accessor bundle-corpus-item-digest)
  (format string :optional t :default "txt" :accessor bundle-corpus-item-format)
  (:key-style :kebab)
  (:extra :allow))

(schema:defschema bundle-corpus-source ()
  "Source spec (file / imap / s3 / snapshot) plus per-item content hashes."
  (kind string :optional t :default "file" :accessor bundle-corpus-source-kind)
  (spec string :optional t :default "" :accessor bundle-corpus-source-spec)
  (items (list bundle-corpus-item) :optional t :default nil
         :accessor bundle-corpus-source-items)
  (:key-style :kebab)
  (:extra :allow))

(schema:defschema bundle-eval-dataset-ref ()
  "Eval dataset identity: A1 content-hashed version plus JSON blob digest."
  (name string :accessor bundle-eval-dataset-ref-name)
  (version string :optional t :default "" :accessor bundle-eval-dataset-ref-version)
  (digest string :optional t :default "" :accessor bundle-eval-dataset-ref-digest)
  (payload string :optional t :default "" :accessor bundle-eval-dataset-ref-payload)
  (:key-style :kebab)
  (:extra :allow))

(schema:defschema bundle-ks-definition ()
  "Serializable KS description used to rebuild a runnable domain."
  (name string :accessor bundle-ks-definition-name)
  (kind string :optional t :default "agent-ks" :accessor bundle-ks-definition-kind)
  (watch string :optional t :default "(:prompt)" :accessor bundle-ks-definition-watch)
  (prompt-key string :optional t :default "prompt"
              :accessor bundle-ks-definition-prompt-key)
  (result-key string :optional t :default "result"
              :accessor bundle-ks-definition-result-key)
  (instructions string :optional t :default ""
                :accessor bundle-ks-definition-instructions)
  (:key-style :kebab)
  (:extra :allow))

(schema:defschema bundle-provenance ()
  "Built-from improvement cycle ids and eval evidence run ids."
  (cycle-ids (list string) :optional t :default nil
             :accessor bundle-provenance-cycle-ids)
  (eval-run-ids (list string) :optional t :default nil
                :accessor bundle-provenance-eval-run-ids)
  (built-at string :optional t :default "" :accessor bundle-provenance-built-at)
  (:key-style :kebab)
  (:extra :allow))

(schema:defschema expert-bundle-manifest ()
  "Versioned expert-bundle: catalogue, KS, pinned skills, corpus, eval, provenance."
  (name string :accessor expert-bundle-manifest-name)
  (version string :optional t :default "0.1.0"
           :accessor expert-bundle-manifest-version)
  (catalogue-vocab string :optional t :default "(:world)"
                   :accessor expert-bundle-manifest-catalogue-vocab)
  (ks-definitions (list bundle-ks-definition) :optional t :default nil
                  :accessor expert-bundle-manifest-ks-definitions)
  (skill-refs (list bundle-skill-ref) :optional t :default nil
              :accessor expert-bundle-manifest-skill-refs)
  (corpus-sources (list bundle-corpus-source) :optional t :default nil
                  :accessor expert-bundle-manifest-corpus-sources)
  (eval-datasets (list bundle-eval-dataset-ref) :optional t :default nil
                 :accessor expert-bundle-manifest-eval-datasets)
  (profile-defaults string :optional t :default "(:kind :personal)"
                    :accessor expert-bundle-manifest-profile-defaults)
  (provenance bundle-provenance :optional t
              :accessor expert-bundle-manifest-provenance)
  (annotations string :optional t :default ""
               :accessor expert-bundle-manifest-annotations)
  (:key-style :kebab)
  (:extra :allow))

(defun bundle-skill-ref-p (x)
  (typep x 'bundle-skill-ref))

(defun bundle-corpus-item-p (x)
  (typep x 'bundle-corpus-item))

(defun bundle-corpus-source-p (x)
  (typep x 'bundle-corpus-source))

(defun bundle-eval-dataset-ref-p (x)
  (typep x 'bundle-eval-dataset-ref))

(defun bundle-ks-definition-p (x)
  (typep x 'bundle-ks-definition))

(defun bundle-provenance-p (x)
  (typep x 'bundle-provenance))

(defun expert-bundle-manifest-p (x)
  (typep x 'expert-bundle-manifest))

(defun %prin1-string (value)
  (with-standard-io-syntax
    (let ((*print-pretty* nil)
          (*print-circle* nil)
          (*print-readably* nil))
      (prin1-to-string value))))

(defun %read-sexp (string &optional default)
  (if (and string (plusp (length string)))
      (with-standard-io-syntax
        (let ((*read-eval* nil))
          (read-from-string string)))
      default))

(defun %as-keyword (key)
  (cond
    ((keywordp key) key)
    ((symbolp key) (intern (symbol-name key) :keyword))
    ((stringp key) (intern (string-upcase key) :keyword))
    (t key)))

(defun %plist-shaped-p (value)
  (and (consp value)
       (evenp (length value))
       (loop for (k v) on value by #'cddr
             always (or (keywordp k) (symbolp k) (stringp k)))))

(defun %jsonish-to-lisp (value)
  "Coerce dump / journal / JSON-ish trees to keyword plists and lists.
   Vectors (JSON arrays) and hash-tables (nested schema:dump) become Lisp.
   Do not NREVERSE a list* plist — that swaps keys and values."
  (cond
    ((hash-table-p value)
     (let ((acc '()))
       (maphash (lambda (k v)
                  (setf acc (list* (%as-keyword k) (%jsonish-to-lisp v) acc)))
                value)
       acc))
    ((and (vectorp value) (not (stringp value)))
     (map 'list #'%jsonish-to-lisp value))
    ((%plist-shaped-p value)
     (loop for (k v) on value by #'cddr
           append (list (%as-keyword k) (%jsonish-to-lisp v))))
    ((consp value)
     (mapcar #'%jsonish-to-lisp value))
    (t value)))

(defun %readable-value (value)
  "schema:dump :as :plist still embeds hash-tables for nested objects.
   Those print as #<HASH-TABLE> and cannot be READ back from an OCI blob."
  (cond
    ((and (typep value 'standard-object)
          (schema:schema-class-p (class-of value)))
     (%readable-value (schema:dump value :as :plist)))
    (t (%jsonish-to-lisp value))))

(defun %manifest-text (manifest)
  (%prin1-string (%readable-value manifest)))

(defun make-bundle-skill-ref (&key name (version "") (digest "")
                                (media-type "text/markdown"))
  (make-instance 'bundle-skill-ref
                 :name name
                 :version (or version "")
                 :digest (or digest "")
                 :media-type (or media-type "text/markdown")))

(defun make-bundle-corpus-item (&key uri digest (format "txt"))
  (make-instance 'bundle-corpus-item
                 :uri uri
                 :digest digest
                 :format (or format "txt")))

(defun make-bundle-corpus-source (&key (kind "file") (spec "") items)
  (make-instance 'bundle-corpus-source
                 :kind (or kind "file")
                 :spec (or spec "")
                 :items (copy-list items)))

(defun make-bundle-eval-dataset-ref (&key name (version "") (digest "")
                                       (payload ""))
  (make-instance 'bundle-eval-dataset-ref
                 :name name
                 :version (or version "")
                 :digest (or digest "")
                 :payload (or payload "")))

(defun make-bundle-ks-definition (&key name (kind "agent-ks")
                                    (watch "(:prompt)")
                                    (prompt-key "prompt")
                                    (result-key "result")
                                    (instructions ""))
  (make-instance 'bundle-ks-definition
                 :name name
                 :kind (or kind "agent-ks")
                 :watch (or watch "(:prompt)")
                 :prompt-key (or prompt-key "prompt")
                 :result-key (or result-key "result")
                 :instructions (or instructions "")))

(defun make-bundle-provenance (&key cycle-ids eval-run-ids (built-at ""))
  (make-instance 'bundle-provenance
                 :cycle-ids (copy-list cycle-ids)
                 :eval-run-ids (copy-list eval-run-ids)
                 :built-at (or built-at "")))

(defun make-expert-bundle-manifest
    (&key name (version "0.1.0") (catalogue-vocab "(:world)")
       ks-definitions skill-refs corpus-sources eval-datasets
       (profile-defaults "(:kind :personal)") provenance
       (annotations "") cycle-ids eval-run-ids)
  (make-instance 'expert-bundle-manifest
                 :name name
                 :version (or version "0.1.0")
                 :catalogue-vocab (or catalogue-vocab "(:world)")
                 :ks-definitions (copy-list ks-definitions)
                 :skill-refs (copy-list skill-refs)
                 :corpus-sources (copy-list corpus-sources)
                 :eval-datasets (copy-list eval-datasets)
                 :profile-defaults (or profile-defaults "(:kind :personal)")
                 :provenance (or provenance
                                 (make-bundle-provenance
                                  :cycle-ids cycle-ids
                                  :eval-run-ids eval-run-ids))
                 :annotations (or annotations "")))

(defun catalogue-vocab-sexp (catalogue)
  "Sexp snapshot of CATALOGUE name + capability/operation names."
  (cond
    ((null catalogue) '(:world))
    ((keywordp catalogue) (list catalogue))
    ((typep catalogue 'cap:capability-catalogue)
     (list (cap:catalogue-name catalogue)
           (loop for row in (cap:list-capabilities catalogue)
                 for cap = (cap:get-capability catalogue (getf row :name))
                 collect (list (getf row :name)
                               (and cap
                                    (mapcar #'cap:capability-operation-name
                                            (cap:capability-operations cap)))))))
    (t (list catalogue))))

(defun %ks-definition (ks)
  (let ((agent (and (agent-ks-p ks) (agent-ks-agent ks))))
    (make-bundle-ks-definition
     :name (string-downcase (string (bb:ks-name ks)))
     :kind (if (agent-ks-p ks) "agent-ks" "ks")
     :watch (%prin1-string (if (agent-ks-p ks)
                               (agent-ks-watch ks)
                               '(:prompt)))
     :prompt-key (string-downcase
                  (string (if (agent-ks-p ks)
                              (agent-ks-prompt-key ks)
                              :prompt)))
     :result-key (string-downcase
                  (string (if (agent-ks-p ks)
                              (agent-ks-result-key ks)
                              :result)))
     :instructions (or (and agent (agent:ai-agent-instructions agent)) ""))))

(defun %profile-defaults-sexp (profile)
  (cond
    ((deployment-profile-p profile)
     (list :kind (or (profile-kind profile) :personal)))
    ((keywordp profile) (list :kind profile))
    (t '(:kind :personal))))

(defun %latest-skill-version (store name)
  (let ((vers (ignore-errors (steer:skill-versions store name))))
    (when vers
      (steer:skill-version-id (first vers)))))

(defun assemble-manifest (domain &key version skill-refs corpus-sources
                                   eval-datasets cycle-ids eval-run-ids
                                   annotations)
  "Build an EXPERT-BUNDLE-MANIFEST from DOMAIN plus collected refs."
  (check-type domain expert-domain)
  (make-expert-bundle-manifest
   :name (expert-name domain)
   :version (or version "0.1.0")
   :catalogue-vocab (%prin1-string (catalogue-vocab-sexp
                                    (expert-catalogue domain)))
   :ks-definitions (mapcar #'%ks-definition (expert-ks-set domain))
   :skill-refs (copy-list skill-refs)
   :corpus-sources (copy-list corpus-sources)
   :eval-datasets (copy-list eval-datasets)
   :profile-defaults (%prin1-string (%profile-defaults-sexp
                                     (expert-profile domain)))
   :cycle-ids cycle-ids
   :eval-run-ids eval-run-ids
   :annotations (or annotations "")))

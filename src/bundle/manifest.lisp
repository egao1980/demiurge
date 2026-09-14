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

(defun %looks-swapped-plist-p (value)
  "schema:dump :as :plist nreverses list* → (value key value key …)."
  (and (consp value)
       (evenp (length value))
       (>= (length value) 2)
       (not (keywordp (first value)))
       (or (keywordp (second value)) (symbolp (second value)))))

(defun %jsonish-to-lisp (value)
  "Coerce dump / journal / JSON-ish trees to keyword plists and lists.
   Vectors (JSON arrays) and hash-tables become Lisp. Repair swapped plists."
  (cond
    ((hash-table-p value)
     (let ((acc '()))
       (maphash (lambda (k v)
                  (push (%as-keyword k) acc)
                  (push (%jsonish-to-lisp v) acc))
                value)
       (nreverse acc)))
    ((and (vectorp value) (not (stringp value)))
     (map 'list #'%jsonish-to-lisp value))
    ((%looks-swapped-plist-p value)
     (loop for (v k) on value by #'cddr
           append (list (%as-keyword k) (%jsonish-to-lisp v))))
    ((%plist-shaped-p value)
     (loop for (k v) on value by #'cddr
           append (list (%as-keyword k) (%jsonish-to-lisp v))))
    ((consp value)
     (mapcar #'%jsonish-to-lisp value))
    (t value)))

(defun %g (plist key &optional default)
  (let ((tail (member key plist)))
    (if tail (second tail) default)))

(defun %readable-skill-ref (r)
  (list :name (bundle-skill-ref-name r)
        :version (bundle-skill-ref-version r)
        :digest (bundle-skill-ref-digest r)
        :media-type (bundle-skill-ref-media-type r)))

(defun %readable-corpus-item (i)
  (list :uri (bundle-corpus-item-uri i)
        :digest (bundle-corpus-item-digest i)
        :format (bundle-corpus-item-format i)))

(defun %readable-corpus-source (s)
  (list :kind (bundle-corpus-source-kind s)
        :spec (bundle-corpus-source-spec s)
        :items (mapcar #'%readable-corpus-item
                       (or (bundle-corpus-source-items s) '()))))

(defun %readable-eval-dataset-ref (r)
  (list :name (bundle-eval-dataset-ref-name r)
        :version (bundle-eval-dataset-ref-version r)
        :digest (bundle-eval-dataset-ref-digest r)
        :payload (bundle-eval-dataset-ref-payload r)))

(defun %readable-ks-definition (d)
  (list :name (bundle-ks-definition-name d)
        :kind (bundle-ks-definition-kind d)
        :watch (bundle-ks-definition-watch d)
        :prompt-key (bundle-ks-definition-prompt-key d)
        :result-key (bundle-ks-definition-result-key d)
        :instructions (bundle-ks-definition-instructions d)))

(defun %readable-provenance (p)
  (if (null p)
      nil
      (list :cycle-ids (copy-list (bundle-provenance-cycle-ids p))
            :eval-run-ids (copy-list (bundle-provenance-eval-run-ids p))
            :built-at (or (bundle-provenance-built-at p) ""))))

(defun %readable-manifest (m)
  (list :name (expert-bundle-manifest-name m)
        :version (expert-bundle-manifest-version m)
        :catalogue-vocab (expert-bundle-manifest-catalogue-vocab m)
        :ks-definitions (mapcar #'%readable-ks-definition
                                (or (expert-bundle-manifest-ks-definitions m) '()))
        :skill-refs (mapcar #'%readable-skill-ref
                            (or (expert-bundle-manifest-skill-refs m) '()))
        :corpus-sources (mapcar #'%readable-corpus-source
                                (or (expert-bundle-manifest-corpus-sources m) '()))
        :eval-datasets (mapcar #'%readable-eval-dataset-ref
                               (or (expert-bundle-manifest-eval-datasets m) '()))
        :profile-defaults (expert-bundle-manifest-profile-defaults m)
        :provenance (%readable-provenance
                     (expert-bundle-manifest-provenance m))
        :annotations (or (expert-bundle-manifest-annotations m) "")))

(defun %readable-value (value)
  "PRINT/READ-able keyword plist. Do not use schema:dump — :as :plist
   swaps keys via nreverse, and :as :hash-table still mis-parses on replay."
  (cond
    ((expert-bundle-manifest-p value) (%readable-manifest value))
    ((bundle-skill-ref-p value) (%readable-skill-ref value))
    ((bundle-corpus-item-p value) (%readable-corpus-item value))
    ((bundle-corpus-source-p value) (%readable-corpus-source value))
    ((bundle-eval-dataset-ref-p value) (%readable-eval-dataset-ref value))
    ((bundle-ks-definition-p value) (%readable-ks-definition value))
    ((bundle-provenance-p value) (%readable-provenance value))
    (t (%jsonish-to-lisp value))))

(defun %manifest-text (manifest)
  (%prin1-string (%readable-value manifest)))

(defun %parse-skill-ref (x)
  (let ((p (%jsonish-to-lisp x)))
    (make-bundle-skill-ref
     :name (or (%g p :name) "")
     :version (or (%g p :version) "")
     :digest (or (%g p :digest) "")
     :media-type (or (%g p :media-type) "text/markdown"))))

(defun %parse-corpus-item (x)
  (let ((p (%jsonish-to-lisp x)))
    (make-bundle-corpus-item
     :uri (or (%g p :uri) "")
     :digest (or (%g p :digest) "")
     :format (or (%g p :format) "txt"))))

(defun %parse-corpus-source (x)
  (let ((p (%jsonish-to-lisp x)))
    (make-bundle-corpus-source
     :kind (or (%g p :kind) "file")
     :spec (or (%g p :spec) "")
     :items (mapcar #'%parse-corpus-item (or (%g p :items) '())))))

(defun %parse-eval-dataset-ref (x)
  (let ((p (%jsonish-to-lisp x)))
    (make-bundle-eval-dataset-ref
     :name (or (%g p :name) "")
     :version (or (%g p :version) "")
     :digest (or (%g p :digest) "")
     :payload (or (%g p :payload) ""))))

(defun %parse-ks-definition (x)
  (let ((p (%jsonish-to-lisp x)))
    (make-bundle-ks-definition
     :name (or (%g p :name) "")
     :kind (or (%g p :kind) "agent-ks")
     :watch (or (%g p :watch) "(:prompt)")
     :prompt-key (or (%g p :prompt-key) "prompt")
     :result-key (or (%g p :result-key) "result")
     :instructions (or (%g p :instructions) ""))))

(defun %parse-provenance (x)
  (if (null x)
      (make-bundle-provenance)
      (let ((p (%jsonish-to-lisp x)))
        (make-bundle-provenance
         :cycle-ids (copy-list (or (%g p :cycle-ids) '()))
         :eval-run-ids (copy-list (or (%g p :eval-run-ids) '()))
         :built-at (or (%g p :built-at) "")))))

(defun %parse-manifest-plist (plist)
  "Build a manifest from a keyword plist. Does not use schema:parse."
  (let ((p (%jsonish-to-lisp plist)))
    (make-expert-bundle-manifest
     :name (%g p :name)
     :version (or (%g p :version) "0.1.0")
     :catalogue-vocab (or (%g p :catalogue-vocab) "(:world)")
     :ks-definitions (mapcar #'%parse-ks-definition
                             (or (%g p :ks-definitions) '()))
     :skill-refs (mapcar #'%parse-skill-ref (or (%g p :skill-refs) '()))
     :corpus-sources (mapcar #'%parse-corpus-source
                             (or (%g p :corpus-sources) '()))
     :eval-datasets (mapcar #'%parse-eval-dataset-ref
                            (or (%g p :eval-datasets) '()))
     :profile-defaults (or (%g p :profile-defaults) "(:kind :personal)")
     :provenance (%parse-provenance (%g p :provenance))
     :annotations (or (%g p :annotations) ""))))

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

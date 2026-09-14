(defpackage #:demiurge/bundle
  (:use #:cl #:demiurge)
  (:nicknames #:stack-demiurge.bundle)
  (:local-nicknames (#:bb #:blackboard-protocol)
                    (#:cap #:capability-protocol)
                    (#:agent #:ai-agent-protocol)
                    (#:steer #:steer-protocol)
                    (#:eval #:eval-protocol)
                    (#:llm #:llm-protocol)
                    (#:web #:websearch-protocol)
                    (#:task #:task-protocol)
                    (#:rag #:rag-protocol)
                    (#:schema #:schema-protocol)
                    (#:toml #:toml-protocol)
                    (#:doc #:doc-extract-protocol)
                    (#:ingest #:demiurge/ingest))
  (:export
   #:bundle-error
   #:bundle-verification-error
   #:bundle-verification-error-expected
   #:bundle-verification-error-actual
   #:bundle-verification-error-path

   #:bundle-skill-ref
   #:bundle-skill-ref-p
   #:make-bundle-skill-ref
   #:bundle-skill-ref-name
   #:bundle-skill-ref-version
   #:bundle-skill-ref-digest
   #:bundle-skill-ref-media-type

   #:bundle-corpus-item
   #:bundle-corpus-item-p
   #:make-bundle-corpus-item
   #:bundle-corpus-item-uri
   #:bundle-corpus-item-digest
   #:bundle-corpus-item-format

   #:bundle-corpus-source
   #:bundle-corpus-source-p
   #:make-bundle-corpus-source
   #:bundle-corpus-source-kind
   #:bundle-corpus-source-spec
   #:bundle-corpus-source-items

   #:bundle-eval-dataset-ref
   #:bundle-eval-dataset-ref-p
   #:make-bundle-eval-dataset-ref
   #:bundle-eval-dataset-ref-name
   #:bundle-eval-dataset-ref-version
   #:bundle-eval-dataset-ref-digest
   #:bundle-eval-dataset-ref-payload

   #:bundle-ks-definition
   #:bundle-ks-definition-p
   #:make-bundle-ks-definition
   #:bundle-ks-definition-name
   #:bundle-ks-definition-kind
   #:bundle-ks-definition-watch
   #:bundle-ks-definition-prompt-key
   #:bundle-ks-definition-result-key
   #:bundle-ks-definition-instructions
   #:bundle-ks-definition-skill-ref
   #:bundle-ks-definition-tool-grants
   #:bundle-ks-definition-mcp-url
   #:bundle-ks-definition-split-ratio

   #:bundle-provenance
   #:bundle-provenance-p
   #:make-bundle-provenance
   #:bundle-provenance-cycle-ids
   #:bundle-provenance-eval-run-ids
   #:bundle-provenance-built-at

   #:expert-bundle-manifest
   #:expert-bundle-manifest-p
   #:make-expert-bundle-manifest
   #:expert-bundle-manifest-name
   #:expert-bundle-manifest-version
   #:expert-bundle-manifest-catalogue-vocab
   #:expert-bundle-manifest-ks-definitions
   #:expert-bundle-manifest-skill-refs
   #:expert-bundle-manifest-corpus-sources
   #:expert-bundle-manifest-eval-datasets
   #:expert-bundle-manifest-profile-defaults
   #:expert-bundle-manifest-provenance
   #:expert-bundle-manifest-annotations

   #:assemble-manifest
   #:+cosign-annotation-key+
   #:+checksum-annotation-key+

   #:pack-expert
   #:install-expert
   #:rollback-expert
   #:verify-bundle-layout
   #:load-expert-config
   #:parse-expert-config

   #:*bundle-installs*
   #:clear-bundle-installs
   #:find-bundle-install
   #:record-bundle-install)
  (:documentation
   "Expert-bundle packaging: OCI layout pack / hash-verified install / rollback."))

(in-package #:demiurge/bundle)

(define-condition bundle-error (demiurge-error)
  ()
  (:report (lambda (c s)
             (format s "demiurge bundle error~@[: ~A~]"
                     (demiurge-error-message c)))))

(define-condition bundle-verification-error (bundle-error)
  ((expected :initarg :expected :reader bundle-verification-error-expected
             :initform nil)
   (actual :initarg :actual :reader bundle-verification-error-actual
           :initform nil)
   (path :initarg :path :reader bundle-verification-error-path :initform nil))
  (:report (lambda (c s)
             (format s "bundle verification failed~@[ (~A)~]: expected ~S got ~S"
                     (or (bundle-verification-error-path c)
                         (demiurge-error-message c))
                     (bundle-verification-error-expected c)
                     (bundle-verification-error-actual c)))))

(defparameter +cosign-annotation-key+ "dev.sigstore.cosign/signature"
  "Reserved annotation slot for a later cosign signature. Empty for now.")

(defparameter +checksum-annotation-key+ "io.demiurge.bundle.checksums"
  "Checksum annotation listing blob digest → title.")

(defvar *bundle-installs* (make-hash-table :test 'equal)
  "Installed bundle records keyed by downcased name, newest first.")

(defun clear-bundle-installs ()
  (clrhash *bundle-installs*)
  nil)

(in-package #:demiurge/ingest)

(define-condition ingest-error (demiurge-error)
  ()
  (:report (lambda (c s)
             (format s "demiurge ingest error~@[: ~A~]"
                     (demiurge-error-message c)))))

(define-condition ingest-source-error (ingest-error)
  ((source :initarg :source :reader ingest-source-error-source :initform nil))
  (:report (lambda (c s)
             (format s "ingest source error~@[: ~A~]"
                     (demiurge-error-message c)))))

(define-condition ingest-stage-error (ingest-error)
  ((stage :initarg :stage :reader ingest-stage-error-stage :initform nil)
   (item :initarg :item :reader ingest-stage-error-item :initform nil)
   (retryable :initarg :retryable :reader ingest-stage-error-retryable-p
              :initform t))
  (:report (lambda (c s)
             (format s "ingest ~a failed~@[: ~A~]"
                     (or (ingest-stage-error-stage c) "stage")
                     (demiurge-error-message c)))))

(define-condition ingest-extractor-error (ingest-stage-error)
  ((format :initarg :format :reader ingest-extractor-error-format :initform nil))
  (:default-initargs :stage :extract)
  (:report (lambda (c s)
             (format s "ingest extractor failed~@[ for ~S~]~@[: ~A~]"
                     (ingest-extractor-error-format c)
                     (demiurge-error-message c)))))

(define-condition ingest-embedder-error (ingest-stage-error)
  ()
  (:default-initargs :stage :embed)
  (:report (lambda (c s)
             (format s "ingest embedder failed~@[: ~A~]"
                     (demiurge-error-message c)))))

(define-condition ingest-store-error (ingest-stage-error)
  ()
  (:default-initargs :stage :store)
  (:report (lambda (c s)
             (format s "ingest store failed~@[: ~A~]"
                     (demiurge-error-message c)))))

(define-condition ingest-store-required (ingest-error)
  ((domain :initarg :domain :reader ingest-store-required-domain :initform nil))
  (:report (lambda (c s)
             (format s "ingest requires a configured rag store~@[: ~A~]"
                     (demiurge-error-message c)))))

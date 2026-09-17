(defpackage #:demiurge/ingest
  (:use #:cl #:demiurge)
  (:nicknames #:stack-demiurge.ingest)
  (:local-nicknames (#:doc #:doc-extract-protocol)
                    (#:obj #:object-store-protocol)
                    (#:mail #:mail-protocol)
                    (#:mime #:mime-protocol)
                    (#:pathlib #:cl-stack-pathlib)
                    (#:rag #:rag-protocol)
                    (#:rag.text #:rag-backend-text)
                    (#:llm #:llm-protocol)
                    (#:task #:task-protocol))
  (:export
   #:ingest-error
   #:ingest-source-error
   #:ingest-source-error-source
   #:ingest-stage-error
   #:ingest-stage-error-stage
   #:ingest-stage-error-item
   #:ingest-stage-error-retryable-p
   #:ingest-extractor-error
   #:ingest-extractor-error-format
   #:ingest-embedder-error
   #:ingest-store-error
   #:ingest-store-required
   #:ingest-store-required-domain

   #:ingest-item
   #:ingest-item-p
   #:make-ingest-item
   #:ingest-item-id
   #:ingest-item-hash
   #:ingest-item-uri
   #:ingest-item-content
   #:ingest-item-format
   #:ingest-item-metadata
   #:item-plist
   #:item-from-plist
   #:content-hash

   #:ingest-source
   #:ingest-source-p
   #:ingest-source-id
   #:file-source
   #:file-source-p
   #:make-file-source
   #:imap-source
   #:imap-source-p
   #:make-imap-source
   #:s3-source
   #:s3-source-p
   #:make-s3-source
   #:enumerate-items

   #:*ingest-item-hook*
   #:*ingest-profile*
   #:list-stored-chunks
   #:stored-content-hashes
   #:sweep-deleted-items
   #:ingest-one-item
   #:run-ingest)
  (:documentation
   "Durable ingest: enumerate sources, extract, chunk, embed, upsert."))

(in-package #:demiurge/ingest)

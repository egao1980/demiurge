(defpackage #:demiurge/src/capabilities/llm
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/protocol #:capability #:capability-operations
                #:make-capability-operation)
  (:import-from #:demiurge/src/capabilities/macros #:defcapability)
  (:export #:llm-generation-capability #:generate-text #:generate-embedding #:list-models))

(in-package #:demiurge/src/capabilities/llm)

(defcapability :llm-generation
  "Text and embedding generation"
  (:operation generate-text ((messages list))
   :returns string
   :doc "Generate text from message history")
  (:operation generate-embedding ((text string))
   :returns vector
   :doc "Generate embedding vector for text")
  (:operation list-models ()
   :returns list
   :doc "List available models"))

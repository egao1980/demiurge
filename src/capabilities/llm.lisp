(defpackage #:demiurge/src/capabilities/llm
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/protocol #:capability #:capability-operations
                #:make-capability-operation)
  (:import-from #:demiurge/src/capabilities/macros #:defcapability)
  (:export #:llm-generation-capability #:generate-text #:generate-with-tools
           #:generate-embedding #:list-models))

(in-package #:demiurge/src/capabilities/llm)

(defcapability :llm-generation
  "Text and embedding generation"
  (:operation generate-text ((messages list))
   :returns string
   :doc "Generate text from message history")
  (:operation generate-embedding ((text string))
   :returns vector
   :doc "Generate embedding vector for text")
  (:operation generate-with-tools ((messages list) (tools list))
   :returns list
   :doc "Generate text with tool calling. Returns (values content tool-calls finish-reason).")
  (:operation list-models ()
   :returns list
   :doc "List available models"))

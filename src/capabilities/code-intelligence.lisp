(defpackage #:demiurge/src/capabilities/code-intelligence
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/protocol #:capability #:capability-operations
                #:make-capability-operation)
  (:import-from #:demiurge/src/capabilities/macros #:defcapability)
  (:export #:code-intelligence-capability
           #:query-definitions #:query-references #:query-callers
           #:search-similar #:search-text #:reindex))

(in-package #:demiurge/src/capabilities/code-intelligence)

(defcapability :code-intelligence
  "Code understanding and search"
  (:operation query-definitions ((name string))
   :returns list
   :doc "Find definitions matching name")
  (:operation query-references ((symbol string))
   :returns list
   :doc "Find references to a symbol")
  (:operation query-callers ((function-name string))
   :returns list
   :doc "Find callers of a function")
  (:operation search-similar ((query string))
   :returns list
   :doc "Semantic similarity search")
  (:operation search-text ((pattern string))
   :returns list
   :doc "Text/regex search across codebase")
  (:operation reindex ((path string))
   :returns t
   :doc "Reindex a file or directory"))

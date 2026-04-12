(defpackage #:demiurge/src/capabilities/code-editing
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/protocol #:capability #:capability-operations
                #:make-capability-operation)
  (:import-from #:demiurge/src/capabilities/macros #:defcapability)
  (:export #:code-editing-capability
           #:read-file #:write-file #:patch-file #:list-files))

(in-package #:demiurge/src/capabilities/code-editing)

(defcapability :code-editing
  "File system access for code editing"
  (:operation read-file ((path string))
   :returns string
   :doc "Read file contents")
  (:operation write-file ((path string) (content string))
   :returns boolean
   :doc "Write content to file")
  (:operation patch-file ((path string) (patch string))
   :returns boolean
   :doc "Apply a patch to a file")
  (:operation list-files ((directory string))
   :returns list
   :doc "List files in a directory"))

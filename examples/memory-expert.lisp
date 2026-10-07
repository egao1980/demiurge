;;;; Offline chronicle demo. Load at compile time for tests if needed.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:demiurge)
    (asdf:load-system "demiurge")))

(defpackage #:demiurge/memory-demo
  (:use #:cl)
  (:export #:run-memory-demo))

(in-package #:demiurge/memory-demo)

(defun run-memory-demo ()
  (let* ((store (memory-protocol:make-in-memory-store))
         (datetime-protocol:*clock*
           (datetime-protocol:make-fixed-clock
            (datetime-protocol:make-instant 1700000000))))
    (demiurge:chronicle-turns store "we froze the budget"
                              :identity "demo" :tenant "default" :session "s1")
    (let ((injected (demiurge:inject-memory-state
                     store "what did we decide?"
                     :identity "demo" :tenant "default")))
      (list :system (llm-protocol:turn-text (first injected))
            :layer (memory-protocol:memory-result-layer
                    (memory-protocol:query-memory
                     store
                     (memory-protocol:make-memory-query
                      :text "budget" :identity "demo" :tenant "default")))))))

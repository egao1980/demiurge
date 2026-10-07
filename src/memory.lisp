(in-package #:demiurge)

;;; Lossless chronicle adapter. conversation-protocol stays the prompt
;;; working set; this appends verbatim turns and injects current-state.

(defvar *chronicle-store* nil
  "Optional MEMORY-PROTOCOL store used when AGENT-KS has no :chronicle.")

(defun %chronicle-tenant ()
  (or (current-tenant nil) "default"))

(defun turn-to-memory-record (turn &key identity tenant session actor ts)
  (mem:make-memory-record
   :ts (or ts (dt:now))
   :actor (or actor identity "default")
   :role (llm:llm-turn-role turn)
   :kind :text
   :text (or (llm:turn-text turn) "")
   :session (or session "default")
   :identity (or identity "default")
   :tenant (or tenant "default")))

(defun chronicle-turns (store turns &key identity tenant session actor)
  "Append TURNS to STORE. Recorder path — no generate."
  (when store
    (dolist (turn (llm:coerce-turns turns))
      (mem:append-record store
                         (turn-to-memory-record
                          turn
                          :identity identity
                          :tenant tenant
                          :session session
                          :actor actor))))
  store)

(defun inject-memory-state (store prompt &key identity tenant bound)
  "Prepend render-state as a :system turn. Empty index → PROMPT unchanged."
  (if (null store)
      prompt
      (let* ((state (mem:current-state store :identity identity :tenant tenant))
             (text (mem:render-state state :bound bound)))
        (if (or (null text) (zerop (length text)))
            prompt
            (append (list (llm:system-turn text))
                    (llm:coerce-turns prompt))))))

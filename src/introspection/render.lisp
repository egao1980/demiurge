(defpackage #:demiurge/src/introspection/render
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:list-sections #:read-section
                #:blackboard-capabilities)
  (:import-from #:demiurge/src/blackboard/workspace
                #:list-workspaces #:workspace-name #:workspace-status)
  (:import-from #:demiurge/src/capabilities/registry
                #:list-capabilities)
  (:import-from #:demiurge/src/knowledge-source/registry
                #:list-ks)
  (:import-from #:demiurge/src/knowledge-source/protocol
                #:ks-name #:ks-version)
  (:import-from #:demiurge/src/introspection/object-registry
                #:object-registry #:register-object #:inspectable-p)
  (:export #:render-bb-summary #:render-workspace #:render-capabilities
           #:render-ks-list #:render-event-log))

(in-package #:demiurge/src/introspection/render)

(defun render-bb-summary (bb &key registry)
  "Produce markdown summary of blackboard state."
  (with-output-to-string (s)
    (format s "## Blackboard State~%~%")
    ;; Workspaces
    (let ((ws-list (list-workspaces bb)))
      (format s "### Active Workspaces (~A)~%" (length ws-list))
      (dolist (ws ws-list)
        (format s "- ~A [~A]~%" (workspace-name ws) (workspace-status ws))))
    (format s "~%")
    ;; Capabilities
    (let ((caps (list-capabilities bb)))
      (format s "### Capabilities (~A)~%" (length caps))
      (dolist (cap caps)
        (format s "- ~A v~A~%" (getf cap :name) (getf cap :version))))
    (format s "~%")
    ;; Sections
    (let ((sections (list-sections bb)))
      (format s "### Sections (~A)~%" (length sections))
      (dolist (key sections)
        (let ((val (read-section bb key)))
          (if (and registry (inspectable-p val))
              (let ((id (register-object registry val)))
                (format s "- ~A -> [object #~A: ~A]~%" key id (type-of val)))
              (format s "- ~A -> ~A~%" key
                      (let ((repr (format nil "~A" val)))
                        (if (> (length repr) 80)
                            (concatenate 'string (subseq repr 0 77) "...")
                            repr)))))))))

(defun render-workspace (ws &key registry)
  "Render workspace detail as markdown."
  (declare (ignore registry))
  (with-output-to-string (s)
    (format s "## Workspace: ~A~%~%" (workspace-name ws))
    (format s "- **Status**: ~A~%" (workspace-status ws))
    (let ((ws-bb (demiurge/src/blackboard/workspace:workspace-blackboard ws)))
      (let ((sections (list-sections ws-bb)))
        (format s "- **Sections**: ~{~A~^, ~}~%" sections)))))

(defun render-capabilities (bb)
  "Render capabilities as markdown."
  (with-output-to-string (s)
    (format s "## Registered Capabilities~%~%")
    (dolist (cap (list-capabilities bb))
      (format s "### ~A (v~A)~%" (getf cap :name) (getf cap :version))
      (format s "Operations: ~{~A~^, ~}~%~%" (getf cap :operations)))))

(defun render-ks-list (bb)
  "Render knowledge sources as markdown."
  (with-output-to-string (s)
    (format s "## Knowledge Sources~%~%")
    (dolist (ks (list-ks bb))
      (format s "- **~A** v~A~%" (ks-name ks) (ks-version ks)))))

(defun render-event-log (bb &key (limit 50))
  "Render recent events as a list."
  (declare (ignore bb limit))
  "[]")

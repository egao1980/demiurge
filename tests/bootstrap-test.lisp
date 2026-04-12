(defpackage #:demiurge-bootstrap/tests/bootstrap-test
  (:use #:cl #:rove)
  (:import-from #:demiurge/src/blackboard/core
                #:make-blackboard #:read-section #:write-section)
  (:import-from #:demiurge/src/capabilities/registry
                #:get-capability #:list-capabilities)
  (:import-from #:demiurge-bootstrap/bootstrap/llm-ks
                #:openai-llm-capability #:make-openai-llm-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/podman-compute-ks
                #:podman-compute-capability #:make-podman-compute-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/loader
                #:load-bootstrap-capabilities #:seed-bb-state)
  (:import-from #:demiurge-bootstrap/bootstrap/supervisor
                #:parse-action #:dispatch-action))

(in-package #:demiurge-bootstrap/tests/bootstrap-test)

;;; --- LLM capability tests ---

(deftest llm-capability-creation
  (let ((cap (make-openai-llm-capability nil :version "test")))
    (ok (typep cap 'openai-llm-capability))
    (ok (eq :llm-generation (demiurge/src/capabilities/protocol:capability-name cap)))))

(deftest llm-capability-with-bb
  (let* ((bb (make-blackboard))
         (cap (make-openai-llm-capability nil :bb bb)))
    (write-section bb :llm-config '(:base-url "http://localhost:1234/v1"))
    (write-section bb :model-roles '((:supervisor . "test-model")
                                     (:coder . "coder-model")))
    (ok (eq bb (slot-value cap 'demiurge-bootstrap/bootstrap/llm-ks::bb)))))

;;; --- Podman compute tests ---

(deftest podman-capability-creation
  (let ((cap (make-podman-compute-capability :image "alpine:3.19")))
    (ok (typep cap 'podman-compute-capability))
    (ok (string= "alpine:3.19"
                 (demiurge-bootstrap/bootstrap/podman-compute-ks::default-image cap)))))

;;; --- Loader tests ---

(deftest loader-seeds-bb-state
  (let ((bb (make-blackboard)))
    (seed-bb-state bb nil nil)
    (ok (read-section bb :llm-config) "LLM config should be seeded")
    (ok (read-section bb :model-roles) "Model roles should be seeded")
    (ok (read-section bb :container-config) "Container config should be seeded")
    (ok (read-section bb :project-root) "Project root should be seeded")))

(deftest loader-seeds-from-config
  (let ((bb (make-blackboard))
        (config (make-hash-table :test 'equal)))
    (setf (gethash "lm_studio_url" config) "http://test:1234/v1"
          (gethash "container_runtime" config) "docker"
          (gethash "project_root" config) "/tmp/test/")
    (seed-bb-state bb nil config)
    (let ((llm-cfg (read-section bb :llm-config)))
      (ok (string= "http://test:1234/v1" (getf llm-cfg :base-url))))
    (let ((container-cfg (read-section bb :container-config)))
      (ok (eq :docker (getf container-cfg :runtime))))
    (ok (string= "/tmp/test/" (read-section bb :project-root)))))

(deftest loader-registers-capabilities
  (let ((bb (make-blackboard)))
    (load-bootstrap-capabilities bb)
    (ok (get-capability bb :llm-generation) "LLM capability registered")
    (ok (get-capability bb :code-editing) "Code editing registered")
    (ok (get-capability bb :version-control) "Git registered")
    (ok (get-capability bb :compute) "Compute registered")
    (ok (>= (length (list-capabilities bb)) 5) "At least 5 capabilities")))

;;; --- Supervisor action parsing tests ---

(deftest parse-valid-json-action
  (let ((action (parse-action "{\"action\": \"execute\", \"reasoning\": \"test\"}")))
    (ok (string-equal "execute" (getf-string action "action")))
    (ok (string-equal "test" (getf-string action "reasoning")))))

(deftest parse-json-with-code-fences
  (let ((action (parse-action "```json
{\"action\": \"idle\", \"reasoning\": \"nothing to do\"}
```")))
    (ok (string-equal "idle" (getf-string action "action")))))

(deftest parse-invalid-json
  (let ((action (parse-action "not valid json at all")))
    (ok (string-equal "idle" (getf-string action "action"))
        "Invalid JSON should fall back to idle")))

(deftest dispatch-configure-action
  (let ((bb (make-blackboard)))
    (dispatch-action (list "action" "configure"
                           "section" "model-roles"
                           "value" "new-value")
                     bb nil)
    (ok (string= "new-value" (read-section bb :model-roles)))))

(deftest dispatch-learn-action
  (let* ((bb (make-blackboard))
         (mem (demiurge/src/persistence/memory:make-persistent-memory)))
    (dispatch-action (list "action" "learn"
                           "topic" "testing"
                           "content" "dispatch works")
                     bb mem)
    (ok (demiurge/src/persistence/memory:mem-get mem "learn:testing")
        "Learn action should write to memory")))

(deftest dispatch-idle-action
  (let ((bb (make-blackboard)))
    (ok (not (handler-case
                 (progn (dispatch-action (list "action" "idle") bb nil) nil)
               (error () t)))
        "Idle should not error")))

;;; --- helper ---

(defun getf-string (plist key)
  (loop for (k v) on plist by #'cddr
        when (and (stringp k) (string-equal k key))
          return v))

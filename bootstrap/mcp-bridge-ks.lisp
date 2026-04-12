(defpackage #:demiurge-bootstrap/bootstrap/mcp-bridge-ks
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/communication
                #:communication-capability
                #:expose-tools #:call-remote-tool #:discover-agents #:delegate-task)
  (:import-from #:demiurge/src/introspection/render
                #:render-bb-summary #:render-capabilities #:render-ks-list)
  (:export #:mcp-bridge-capability #:make-mcp-bridge-capability))

(in-package #:demiurge-bootstrap/bootstrap/mcp-bridge-ks)

(defclass mcp-bridge-capability (communication-capability)
  ((bb :initarg :bb :reader bridge-bb :initform nil)))

(defun make-mcp-bridge-capability (bb &key (version "0.1.0"))
  (make-instance 'mcp-bridge-capability
                 :name :communication :version version :bb bb))

(defmethod expose-tools ((cap mcp-bridge-capability) tools &key)
  (declare (ignore tools))
  t)

(defmethod call-remote-tool ((cap mcp-bridge-capability) server tool args &key)
  (declare (ignore server tool args))
  nil)

(defmethod discover-agents ((cap mcp-bridge-capability) &key)
  nil)

(defmethod delegate-task ((cap mcp-bridge-capability) agent task &key)
  (declare (ignore agent task))
  nil)

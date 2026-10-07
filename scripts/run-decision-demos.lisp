;;;; Offline decision-plane demos (mock / fixture — no Kev sidecar).
;;;;   sbcl --load scripts/run-decision-demos.lisp
;;;; Live HTTP: DECISION_LIVE=1 plus a sidecar on 127.0.0.1:8009.
;;;; Missing sibling examples are skipped (stacked branch may not have them yet).

(require :asdf)
(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&DEMO FAIL: ~A~%" c)
        (uiop:quit 1)))

(defun %demo-path (system relative)
  (let ((asd (ignore-errors (asdf:find-system system nil))))
    (when asd
      (probe-file (merge-pathnames relative (asdf:system-source-directory asd))))))

(defun %run-demo (system relative thunk)
  (let ((path (%demo-path system relative)))
    (cond
      (path
       (format t "~&;; ~a ~a~%" system relative)
       (asdf:load-system system)
       (load path)
       (funcall thunk))
      (t
       (format t "~&;; skip ~a (~a not on registry)~%" system relative)))))

(%run-demo "decision-protocol" "examples/batch.lisp"
           (lambda () (decision-protocol/demo:run)))

(%run-demo "eval-protocol" "examples/calibration.lisp"
           (lambda () (eval-protocol/demo:run)))

(%run-demo "agent-runtime-protocol" "examples/lifecycle.lisp"
           (lambda () (agent-runtime-protocol/demo:run)))

(%run-demo "decision-backend-http" "examples/systemone.lisp"
           (lambda () (decision-backend-http/demo:run)))

(%run-demo "compute-protocol" "examples/egress.lisp"
           (lambda () (compute-protocol/demo:run)))

(%run-demo "compute-backend-podman" "examples/egress.lisp"
           (lambda () (compute-backend-podman/demo:run)))

(%run-demo "secrets-protocol" "examples/store.lisp"
           (lambda () (secrets-protocol/demo:run)))

(%run-demo "capability-protocol" "examples/decision-interceptor.lisp"
           (lambda () (capability-protocol/demo:run)))

(format t "~&;; demiurge decision-ks~%")
(asdf:load-system "demiurge")
(load (merge-pathnames "examples/decision-expert.lisp"
                       (asdf:system-source-directory "demiurge")))
(demiurge:run-decision-demo)

(format t "~&DEMO OK~%")
(uiop:quit 0)

(defpackage #:demiurge-bootstrap/bootstrap/podman-compute-ks
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/compute
                #:compute-capability
                #:run-command #:create-environment #:exec-in-environment #:destroy-environment)
  (:export #:podman-compute-capability #:make-podman-compute-capability #:shared-workspace))

(in-package #:demiurge-bootstrap/bootstrap/podman-compute-ks)

;;; Podman binary detection

(defun detect-podman-path ()
  "Find podman binary via PATH or fall back to /opt/podman/bin/podman."
  (handler-case
      (let ((path (uiop:run-program '("which" "podman")
                                    :output '(:string :stripped t)
                                    :error-output :interactive
                                    :ignore-error-status nil)))
        (if (plusp (length path)) path "/opt/podman/bin/podman"))
    (uiop:subprocess-error ()
      "/opt/podman/bin/podman")))

;;; Capability class

(defclass podman-compute-capability (compute-capability)
  ((podman-path :initarg :podman-path :reader podman-path)
   (default-image :initarg :image :reader default-image :initform "docker.io/library/ubuntu:24.04")
   (authfile :initarg :authfile :reader podman-authfile :initform nil)
   (shared-workspace :initarg :shared-workspace :reader shared-workspace
                     :documentation "Host dir mounted as /workspace in every ephemeral run-command.")))

(defun ensure-authfile (path)
  "Ensure a minimal auth file exists to bypass broken credential helpers."
  (unless (and path (probe-file path))
    (setf path (merge-pathnames ".config/demiurge/podman-auth.json" (user-homedir-pathname)))
    (ensure-directories-exist path)
    (unless (probe-file path)
      (with-open-file (s path :direction :output)
        (write-string "{}" s))))
  path)

(defun ensure-shared-workspace ()
  "Create and return the shared workspace directory for ephemeral containers."
  (let ((dir (merge-pathnames ".demiurge/workspace/" (user-homedir-pathname))))
    (ensure-directories-exist dir)
    dir))

(defun make-podman-compute-capability (&key image podman-path authfile (version "0.1.0"))
  (make-instance 'podman-compute-capability
                 :name :compute :version version
                 :podman-path (or podman-path (detect-podman-path))
                 :image (or image "docker.io/library/ubuntu:24.04")
                 :authfile (ensure-authfile authfile)
                 :shared-workspace (ensure-shared-workspace)))

;;; Internal helpers

(defvar *authfile-subcommands* '("run" "create" "pull" "push" "build" "login")
  "Podman subcommands that accept --authfile.")

(defun run-podman (cap args &key ignore-errors)
  "Invoke podman CLI, returning (values stdout stderr exit-code).
--authfile is injected only for subcommands that support it."
  (let* ((subcmd (first args))
         (rest   (rest args))
         (use-authfile (and (podman-authfile cap)
                            (member subcmd *authfile-subcommands* :test #'string-equal)))
         (full-args (if use-authfile
                        (list* (podman-path cap) subcmd
                               "--authfile" (namestring (podman-authfile cap))
                               rest)
                        (cons (podman-path cap) args))))
    (uiop:run-program full-args
                      :output '(:string :stripped t)
                      :error-output '(:string :stripped t)
                      :ignore-error-status (or ignore-errors t))))

(defun generate-env-name ()
  (format nil "demiurge-env-~A" (get-universal-time)))

;;; Protocol methods

(defmethod run-command ((cap podman-compute-capability) command &key)
  (let ((ws (namestring (shared-workspace cap))))
    (multiple-value-bind (stdout stderr exit-code)
        (run-podman cap (list "run" "--rm"
                              "-v" (format nil "~A:/workspace" ws)
                              "-w" "/workspace"
                              (default-image cap) "sh" "-c" command))
      (list exit-code stdout stderr))))

(defmethod create-environment ((cap podman-compute-capability) spec &key)
  "Create a persistent named container with /workspace mounted from the shared workspace.
The container's own filesystem persists across exec calls (packages, files outside /workspace)."
  (let* ((image (or (getf spec :image) (default-image cap)))
         (name (or (getf spec :name) (generate-env-name)))
         (ws (namestring (shared-workspace cap))))
    ;; Remove any stale container with the same name
    (run-podman cap (list "rm" "-f" name) :ignore-errors t)
    (multiple-value-bind (stdout stderr exit-code)
        (run-podman cap (list "create" "--name" name
                              "-v" (format nil "~A:/workspace" ws)
                              "-w" "/workspace"
                              image "sleep" "infinity"))
      (declare (ignore stdout))
      (unless (zerop exit-code)
        (error "podman create failed (exit ~D): ~A" exit-code stderr)))
    (multiple-value-bind (stdout stderr exit-code)
        (run-podman cap (list "start" name))
      (declare (ignore stdout))
      (unless (zerop exit-code)
        (run-podman cap (list "rm" name) :ignore-errors t)
        (error "podman start failed (exit ~D): ~A" exit-code stderr)))
    (list :name name :image image)))

(defmethod exec-in-environment ((cap podman-compute-capability) env command &key)
  (let ((name (getf env :name)))
    (multiple-value-bind (stdout stderr exit-code)
        (run-podman cap (list "exec" name "sh" "-c" command))
      (list exit-code stdout stderr))))

(defmethod destroy-environment ((cap podman-compute-capability) env &key)
  (let ((name (getf env :name)))
    (run-podman cap (list "stop" name) :ignore-errors t)
    (run-podman cap (list "rm" name) :ignore-errors t)
    t))

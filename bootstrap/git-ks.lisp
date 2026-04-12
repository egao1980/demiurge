(defpackage #:demiurge-bootstrap/bootstrap/git-ks
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/vcs
                #:version-control-capability
                #:vcs-status #:vcs-diff #:vcs-commit #:vcs-branch #:vcs-log)
  (:export #:git-vcs-capability #:make-git-vcs-capability))

(in-package #:demiurge-bootstrap/bootstrap/git-ks)

(defclass git-vcs-capability (version-control-capability) ())

(defun make-git-vcs-capability (&key (version "0.1.0"))
  (make-instance 'git-vcs-capability :name :version-control :version version))

(defun run-git (args &key cwd)
  "Run a git command. Returns (values stdout exit-code)."
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (format nil "git ~A" args)
                        :directory cwd
                        :output '(:string :stripped t)
                        :error-output '(:string :stripped t)
                        :ignore-error-status t)
    (declare (ignore stderr))
    (values stdout exit-code)))

(defmethod vcs-status ((cap git-vcs-capability) path &key)
  (run-git "status --porcelain" :cwd path))

(defmethod vcs-diff ((cap git-vcs-capability) path &key)
  (run-git "diff" :cwd path))

(defmethod vcs-commit ((cap git-vcs-capability) path message &key)
  (run-git "add -A" :cwd path)
  (run-git (format nil "commit -m ~S" message) :cwd path))

(defmethod vcs-branch ((cap git-vcs-capability) name &key)
  (run-git (format nil "checkout -b ~A" name)))

(defmethod vcs-log ((cap git-vcs-capability) path &key)
  (run-git "log --oneline -20" :cwd path))

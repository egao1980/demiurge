(defpackage #:demiurge-bootstrap/bootstrap/swank-server
  (:use #:cl)
  (:export #:start-swank #:stop-swank))

(in-package #:demiurge-bootstrap/bootstrap/swank-server)

(defvar *swank-server* nil)

(defun start-swank (&key (port 4005))
  "Start a Swank server for SLIME/SLY connections.
Loads swank via ASDF if not already present."
  (when *swank-server*
    (format t "~&Swank already running on port ~D~%" port)
    (return-from start-swank *swank-server*))
  (handler-case
      (progn
        (unless (find-package :swank)
          (asdf:load-system :swank))
        (let ((create-server (find-symbol "CREATE-SERVER" :swank)))
          (when create-server
            (setf *swank-server*
                  (funcall create-server :port port :dont-close t))
            (format t "~&Swank server started on port ~D~%" port)
            *swank-server*)))
    (error (e)
      (format *error-output* "Failed to start Swank: ~A~%" e)
      nil)))

(defun stop-swank ()
  "Stop the Swank server."
  (when *swank-server*
    (handler-case
        (let ((stop-server (find-symbol "STOP-SERVER" :swank)))
          (when stop-server
            (funcall stop-server *swank-server*)))
      (error (e)
        (format *error-output* "Error stopping Swank: ~A~%" e)))
    (setf *swank-server* nil)
    (format t "~&Swank server stopped.~%")))

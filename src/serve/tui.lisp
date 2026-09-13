(in-package #:demiurge/serve)

(defun render-expert-transcript (domain board &key stream)
  "Print DOMAIN's board sections. Soft-uses cl-stack-llm-tui when loaded."
  (let ((out (or stream *standard-output*)))
    (format out "~&[demiurge] ~a~%" (expert-name domain))
    (dolist (key (bb:list-sections board))
      (format out "  ~a: ~a~%" key (bb:read-section board key :default nil)))
    (let* ((pkg (find-package :cl-stack-llm-tui))
           (render (and pkg (find-symbol "RENDER-SCREEN" pkg))))
      (when (and render (fboundp render))
        (ignore-errors (funcall render :stream out))))
    board))

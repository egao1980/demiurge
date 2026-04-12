(defpackage #:demiurge/src/utils/logging
  (:use #:cl)
  (:export #:setup-logging))

(in-package #:demiurge/src/utils/logging)

(defun setup-logging (&key (level :info))
  "Configure log4cl for demiurge."
  (log:config level)
  (log:config :daily "/tmp/demiurge.log"))

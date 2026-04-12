;;; events.lisp — DEPRECATED: event bus removed in favour of BB watcher/KSAR system.
;;; This file is kept as a stub for compilation compatibility during migration.
;;; All reactive behaviour now lives in demiurge/src/blackboard/core (watchers, agenda, scheduler).

(defpackage #:demiurge/src/blackboard/events
  (:use #:cl)
  (:documentation "Legacy stub — event bus replaced by BB watcher/KSAR/agenda in core.lisp."))

(in-package #:demiurge/src/blackboard/events)

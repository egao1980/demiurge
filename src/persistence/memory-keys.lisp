(defpackage #:demiurge/src/persistence/memory-keys
  (:use #:cl)
  (:import-from #:demiurge/src/persistence/memory
                #:persistent-memory #:mem-get #:mem-set #:mem-append
                #:mem-increment #:mem-get-number #:mem-get-list-last
                #:with-memory-transaction)
  (:export ;; KS performance
           #:record-ks-execution #:ks-success-rate #:ks-avg-duration
           ;; Task history
           #:record-task-result #:recent-tasks
           ;; Learned patterns
           #:remember #:recall #:forget
           ;; Preferences
           #:set-preference #:get-preference
           ;; Conversation / interaction log
           #:log-interaction #:recent-interactions))

(in-package #:demiurge/src/persistence/memory-keys)

;;; Well-known key prefixes:
;;;   "ks:<name>:*"       - KS performance data
;;;   "task:*"            - Task history
;;;   "learn:<topic>"     - Learned patterns / heuristics
;;;   "pref:<key>"        - User/system preferences
;;;   "interaction:*"     - Conversation/interaction log

;;; --- KS Performance Tracking ---

(defun record-ks-execution (mem ks-name &key (success t) (duration-ms 0) (context nil))
  "Record a KS execution for performance tracking."
  (with-memory-transaction (mem)
    (let ((prefix (format nil "ks:~(~A~)" ks-name)))
      (mem-increment mem (format nil "~A:total" prefix))
      (when success
        (mem-increment mem (format nil "~A:success" prefix)))
      (unless success
        (mem-increment mem (format nil "~A:failure" prefix)))
      (mem-append mem (format nil "~A:history" prefix)
                  (list :timestamp (get-universal-time)
                        :success success
                        :duration-ms duration-ms
                        :context context)
                  :max-entries 200))))

(defun ks-success-rate (mem ks-name)
  "Return success rate for a KS as a float [0.0, 1.0], or NIL if no data."
  (let* ((prefix (format nil "ks:~(~A~)" ks-name))
         (total (mem-get-number mem (format nil "~A:total" prefix) 0)))
    (when (plusp total)
      (/ (mem-get-number mem (format nil "~A:success" prefix) 0)
         (float total)))))

(defun ks-avg-duration (mem ks-name &optional (last-n 20))
  "Average duration of last N executions in ms."
  (let* ((prefix (format nil "ks:~(~A~)" ks-name))
         (recent (mem-get-list-last mem (format nil "~A:history" prefix) last-n)))
    (when recent
      (let ((durations (remove nil (mapcar (lambda (entry)
                                             (getf entry :duration-ms))
                                           recent))))
        (when durations
          (/ (reduce #'+ durations) (float (length durations))))))))

;;; --- Task History ---

(defun record-task-result (mem &key task-id description status steps error workspace)
  "Record completed task for history."
  (mem-append mem "task:history"
              (list :task-id (or task-id (format nil "~A" (get-universal-time)))
                    :description description
                    :status status
                    :steps steps
                    :error error
                    :workspace workspace
                    :timestamp (get-universal-time))
              :max-entries 500))

(defun recent-tasks (mem &optional (n 10))
  "Return last N task results."
  (mem-get-list-last mem "task:history" n))

;;; --- Learned Patterns / Heuristics ---

(defun remember (mem topic content &key (confidence 1.0) source)
  "Store a learned pattern or heuristic under TOPIC."
  (mem-set mem (format nil "learn:~(~A~)" topic)
           (list :content content
                 :confidence confidence
                 :source source
                 :learned-at (get-universal-time)
                 :accessed 0)))

(defun recall (mem topic)
  "Recall a learned pattern. Returns the content or NIL."
  (let ((entry (mem-get mem (format nil "learn:~(~A~)" topic))))
    (when entry
      ;; bump access count
      (let ((new-entry (copy-list entry)))
        (setf (getf new-entry :accessed) (1+ (or (getf new-entry :accessed) 0)))
        (mem-set mem (format nil "learn:~(~A~)" topic) new-entry))
      (getf entry :content))))

(defun forget (mem topic)
  "Remove a learned pattern."
  (mem-set mem (format nil "learn:~(~A~)" topic) nil))

;;; --- Preferences ---

(defun set-preference (mem key value)
  (mem-set mem (format nil "pref:~(~A~)" key) value))

(defun get-preference (mem key &optional default)
  (or (mem-get mem (format nil "pref:~(~A~)" key)) default))

;;; --- Interaction Log ---

(defun log-interaction (mem &key role content model tokens task-id)
  "Log an LLM interaction for audit trail and learning."
  (mem-append mem "interaction:log"
              (list :role role :content content :model model
                    :tokens tokens :task-id task-id
                    :timestamp (get-universal-time))
              :max-entries 2000))

(defun recent-interactions (mem &optional (n 20))
  "Return last N logged interactions."
  (mem-get-list-last mem "interaction:log" n))

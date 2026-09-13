(in-package #:demiurge)

(defparameter +default-config-toml+
  "[agenda]
max-concurrency = 4

[ksar]
timeout-seconds = 10

[session]
window-turns = 8

[llm]
default-model = \"mock\"

[paths]
data-dir = \"\"

[improve]
enabled = false
")

(defclass demiurge-config ()
  ((agenda-max-concurrency
    :initarg :agenda-max-concurrency
    :accessor demiurge-config-agenda-max-concurrency
    :initform 4)
   (ksar-timeout-seconds
    :initarg :ksar-timeout-seconds
    :accessor demiurge-config-ksar-timeout-seconds
    :initform 10)
   (session-window-turns
    :initarg :session-window-turns
    :accessor demiurge-config-session-window-turns
    :initform 8)
   (llm-default-model
    :initarg :llm-default-model
    :accessor demiurge-config-llm-default-model
    :initform "mock")
   (llm-catalog
    :initarg :llm-catalog
    :accessor demiurge-config-llm-catalog
    :initform nil)
   (paths-data-dir
    :initarg :paths-data-dir
    :accessor demiurge-config-paths-data-dir
    :initform nil)
   (improve-enabled
    :initarg :improve-enabled
    :accessor demiurge-config-improve-enabled
    :initform nil)
   (raw
    :initarg :raw
    :accessor demiurge-config-raw
    :initform nil)))

(defun demiurge-config-p (x)
  (typep x 'demiurge-config))

(defvar *demiurge-config* nil
  "Last config returned by LOAD-DEMIURGE-CONFIG.")

(defun %cfg-get (stack path &optional default)
  (let ((alt (substitute #\_ #\- (string path))))
    (cond
      ((null stack) default)
      ((not (eq alt path))
       (or (ignore-errors (cfg:get stack path))
           (ignore-errors (cfg:get stack alt))
           default))
      (t
       (or (ignore-errors (cfg:get stack path))
           default)))))

(defun %cfg-int (stack path default)
  (let ((v (%cfg-get stack path default)))
    (cond
      ((integerp v) v)
      ((stringp v) (parse-integer v :junk-allowed nil))
      (t default))))

(defun %cfg-bool (stack path default)
  (handler-case (cfg:get-boolean stack path)
    (error ()
      (let ((alt (substitute #\_ #\- (string path))))
        (handler-case (cfg:get-boolean stack alt)
          (error () default))))))

(defun %cfg-string (stack path default)
  (let ((v (%cfg-get stack path default)))
    (cond
      ((null v) default)
      ((stringp v) (if (zerop (length v)) default v))
      (t (princ-to-string v)))))

(defun %table-to-plist (table)
  (cond
    ((null table) nil)
    ((hash-table-p table)
     (let ((out nil))
       (maphash (lambda (k v)
                  (setf out (list* (intern (string-upcase (string k)) :keyword)
                                   (if (hash-table-p v) (%table-to-plist v) v)
                                   out)))
                table)
       out))
    ((listp table) table)
    (t table)))

(defun %cfg-catalog (stack)
  (let ((raw (or (%cfg-get stack "llm.catalog")
                 (%cfg-get stack "llm.catalogue"))))
    (cond
      ((null raw) nil)
      ((hash-table-p raw)
       (let ((out nil))
         (maphash (lambda (name spec)
                    (push (list* :name (string name)
                                 (%table-to-plist spec))
                          out))
                  raw)
         (nreverse out)))
      ((or (vectorp raw) (listp raw))
       (map 'list (lambda (spec)
                    (if (hash-table-p spec)
                        (%table-to-plist spec)
                        spec))
            raw))
      (t nil))))

(defun %config-from-stack (stack)
  (make-instance 'demiurge-config
                 :agenda-max-concurrency (%cfg-int stack "agenda.max-concurrency" 4)
                 :ksar-timeout-seconds (%cfg-int stack "ksar.timeout-seconds" 10)
                 :session-window-turns (%cfg-int stack "session.window-turns" 8)
                 :llm-default-model (%cfg-string stack "llm.default-model" "mock")
                 :llm-catalog (%cfg-catalog stack)
                 :paths-data-dir (%cfg-string stack "paths.data-dir" nil)
                 :improve-enabled (%cfg-bool stack "improve.enabled" nil)
                 :raw stack))

(defun %write-default-toml (path)
  (with-open-file (out path :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
    (write-string +default-config-toml+ out))
  path)

(defun load-demiurge-config (&key path (prefix "DEMIURGE") (env t) environ)
  "Load TOML at PATH (or built-in defaults) via cl-stack-config.
   Precedence: defaults < file < env (PREFIX_TABLE__KEY). → DEMIURGE-CONFIG."
  (let* ((source (or (and path (probe-file path) path)
                     (and (null path)
                          (let ((from-env (uiop:getenv "DEMIURGE_CONFIG")))
                            (and from-env (probe-file from-env))))))
         (stack
          (if source
              (cfg:load-config source :prefix prefix :env env :environ environ)
              (uiop:with-temporary-file (:pathname tmp :prefix "demiurge-cfg-"
                                        :type "toml")
                (%write-default-toml tmp)
                (cfg:load-config tmp :prefix prefix :env env :environ environ)))))
    (let ((obj (%config-from-stack stack)))
      (setf *demiurge-config* obj)
      obj)))

(defun current-demiurge-config ()
  (or *demiurge-config* (load-demiurge-config :env nil)))

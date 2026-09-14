(in-package #:demiurge)

(defvar *expert-registry* (make-hash-table :test 'equal)
  "Registered EXPERT-DOMAIN objects, keyed by downcased name string.")

(defun expert-key (name)
  (string-downcase (string name)))

(defun %coerce-name (name)
  (unless name
    (restart-case
        (error 'invalid-expert :message "expert-domain requires :name")
      (use-value (value)
        :report "Use a supplied expert name"
        (return-from %coerce-name (expert-key value)))))
  (expert-key name))

(defun %coerce-catalogue (catalogue)
  (cond
    ((null catalogue) (cap:make-catalogue :world))
    ((keywordp catalogue) (cap:make-catalogue catalogue))
    ((typep catalogue 'cap:capability-catalogue) catalogue)
    (t (restart-case
           (error 'invalid-expert
                  :message "catalogue must be a capability-catalogue or keyword")
         (use-value (value)
           :report "Use a supplied catalogue"
           (%coerce-catalogue value))))))

(defun %coerce-profile (profile)
  (cond
    ((deployment-profile-p profile) profile)
    ((member profile '(:personal :corporate) :test #'eq) profile)
    (t
     (restart-case
         (error 'invalid-expert
                :message "profile must be :personal, :corporate, or a deployment-profile")
       (use-value (value)
         :report "Use a supplied profile"
         (%coerce-profile value))))))

(defun %coerce-eval-suites (suites)
  (mapcar (lambda (ds)
            (if (eval:eval-dataset-p ds)
                ds
                (restart-case
                    (error 'invalid-expert
                           :message "eval-suites entries must be eval-dataset")
                  (use-value (value)
                    :report "Use a supplied eval-dataset"
                    value))))
          (copy-list suites)))

(defclass expert-domain ()
  ((name :initarg :name :accessor expert-name)
   (catalogue :initarg :catalogue :accessor expert-catalogue :initform nil)
   (ks-set :initarg :ks-set :accessor expert-ks-set :initform nil)
   (steering :initarg :steering :accessor expert-steering :initform nil)
   (corpora :initarg :corpora :accessor expert-corpora :initform nil
            :documentation "rag-protocol store designators (objects, keywords, or paths).")
   (eval-suites :initarg :eval-suites :accessor expert-eval-suites :initform nil)
   (profile :initarg :profile :accessor expert-profile :initform :personal)))

(defun expert-domain-p (x)
  (typep x 'expert-domain))

(defmethod initialize-instance :after ((domain expert-domain)
                                       &key knowledge-sources &allow-other-keys)
  (unless (slot-boundp domain 'name)
    (setf (expert-name domain) nil))
  (setf (expert-name domain) (%coerce-name (expert-name domain)))

  (setf (expert-catalogue domain) (%coerce-catalogue (expert-catalogue domain)))
  (when knowledge-sources
    (setf (expert-ks-set domain) knowledge-sources))
  (setf (expert-ks-set domain) (copy-list (expert-ks-set domain)))
  (when (expert-steering domain)
    (setf (expert-steering domain)
          (steer:coerce-steering (expert-steering domain))))
  (setf (expert-corpora domain) (copy-list (expert-corpora domain)))
  (setf (expert-eval-suites domain)
        (%coerce-eval-suites (expert-eval-suites domain)))
  (setf (expert-profile domain) (%coerce-profile (expert-profile domain))))

(defun instantiate-expert-domain (&key name catalogue ks-set knowledge-sources
                                    steering corpora eval-suites
                                    (profile :personal)
                                    &allow-other-keys)
  "Single construction path for MAKE-EXPERT-DOMAIN, DEFEXPERT,
   LOAD-EXPERT-CONFIG, and INSTALL-EXPERT. Live objects only —
   serializable specs are resolved by the caller (bundle loader)."
  (make-instance 'expert-domain
                 :name name
                 :catalogue catalogue
                 :ks-set (or ks-set knowledge-sources)
                 :steering steering
                 :corpora corpora
                 :eval-suites eval-suites
                 :profile profile))

(defun make-expert-domain (&key name catalogue ks-set knowledge-sources
                             steering corpora eval-suites (profile :personal))
  (instantiate-expert-domain
   :name name
   :catalogue catalogue
   :ks-set ks-set
   :knowledge-sources knowledge-sources
   :steering steering
   :corpora corpora
   :eval-suites eval-suites
   :profile profile))

(defun load-expert-config (path &rest args)
  "Defined by demiurge/bundle. Core stub so the #:demiurge export is fbound."
  (declare (ignore path args))
  (error 'expert-config-error
         :message "load the demiurge/bundle system to use load-expert-config"))

(defun register-expert (domain)
  (check-type domain expert-domain)
  (setf (gethash (expert-name domain) *expert-registry*) domain)
  domain)

(defun unregister-expert (name)
  (remhash (expert-key name) *expert-registry*)
  name)

(defun find-expert (name)
  (gethash (expert-key name) *expert-registry*))

(defun require-expert (name)
  "FIND-EXPERT or UNKNOWN-EXPERT with USE-VALUE / SKIP."
  (or (find-expert name)
      (restart-case
          (error 'unknown-expert :name name)
        (use-value (domain)
          :report "Use a supplied expert domain"
          domain)
        (skip ()
          :report "Treat the missing expert as NIL"
          nil))))

(defun list-experts ()
  (let ((out nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v out))
             *expert-registry*)
    out))

(defun clear-expert-registry ()
  (clrhash *expert-registry*)
  nil)

(defun %parse-defexpert-body (body)
  (let ((doc nil) (options '()))
    (when (stringp (first body))
      (setf doc (pop body)))
    (dolist (form body)
      (unless (and (consp form) (keywordp (first form)))
        (error 'invalid-expert
               :message (format nil "bad defexpert form: ~S" form)))
      (push form options))
    (values doc (nreverse options))))

(defmacro defexpert (name &body body)
  "Thin sugar over INSTANTIATE-EXPERT-DOMAIN + REGISTER-EXPERT.
   Options: (:catalogue form) (:ks-set form) (:knowledge-sources form)
            (:steering form) (:corpora form) (:eval-suites form)
            (:profile form). Same construction path as LOAD-EXPERT-CONFIG
            and INSTALL-EXPERT. Everything is reachable via MAKE-INSTANCE."
  (multiple-value-bind (doc options)
      (%parse-defexpert-body body)
    (declare (ignore doc))
    (let ((initargs '()))
      (dolist (opt options)
        (ecase (first opt)
          (:catalogue (setf initargs (list* :catalogue (second opt) initargs)))
          (:ks-set (setf initargs (list* :ks-set (second opt) initargs)))
          (:knowledge-sources
           (setf initargs (list* :knowledge-sources (second opt) initargs)))
          (:steering (setf initargs (list* :steering (second opt) initargs)))
          (:corpora (setf initargs (list* :corpora (second opt) initargs)))
          (:eval-suites
           (setf initargs (list* :eval-suites (second opt) initargs)))
          (:profile (setf initargs (list* :profile (second opt) initargs)))))
      `(register-expert
        (instantiate-expert-domain
         :name ,(if (stringp name)
                    name
                    (string-downcase (symbol-name name)))
         ,@initargs)))))

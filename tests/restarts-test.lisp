(in-package #:demiurge/tests)

(deftest require-expert-signals
  (with-clean-registry
    (ok (signals (require-expert "missing") 'unknown-expert))))

(deftest require-expert-use-value
  (with-clean-registry
    (let* ((supplied (make-expert-domain :name "fallback"))
           (got (handler-bind ((unknown-expert
                                (lambda (c)
                                  (use-value supplied c))))
                  (require-expert "missing"))))
      (ok (eq supplied got)))))

(deftest require-expert-skip
  (with-clean-registry
    (let ((got :unset))
      (handler-bind ((unknown-expert
                      (lambda (c)
                        (invoke-skip c))))
        (setf got (require-expert "missing")))
      (ok (null got)))))

(deftest invalid-profile-signals
  (ok (signals (make-expert-domain :name "x" :profile :lab)
               'invalid-expert)))

(deftest invalid-profile-use-value
  (let ((domain (handler-bind ((invalid-expert
                                (lambda (c)
                                  (use-value :personal c))))
                  (make-expert-domain :name "x" :profile :lab))))
    (ok (eq :personal (expert-profile domain)))))

(deftest invalid-eval-suite-use-value
  (let* ((ds (eval:make-eval-dataset
              :name "ok"
              :cases (list (eval:make-eval-case :input 1 :expected 1))))
         (domain (handler-bind ((invalid-expert
                                 (lambda (c)
                                   (use-value ds c))))
                   (make-expert-domain :name "x"
                                       :eval-suites (list :not-a-dataset)))))
    (ok (eval:eval-dataset-p (first (expert-eval-suites domain))))))

(deftest missing-event-backend-use-value
  (let* ((maker (symbol-function
                 (find-symbol "MAKE-LIBUV-BACKEND" :event-backend-libuv)))
         (got nil))
    (let ((demiurge::*event-backend-maker* nil))
      (handler-bind ((missing-event-backend
                      (lambda (c)
                        (use-value maker c))))
        (setf got (demiurge::call-with-event-loop (lambda () :ok)))))
    (ok (eq :ok got))))

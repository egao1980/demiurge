(defpackage #:demiurge-bootstrap/bootstrap/forge-github-ks
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/forge
                #:forge-capability #:list-issues #:get-issue
                #:create-issue #:create-pr #:comment-on)
  (:export #:github-forge-capability #:make-github-forge-capability))

(in-package #:demiurge-bootstrap/bootstrap/forge-github-ks)

(defclass github-forge-capability (forge-capability)
  ((token :initarg :token :reader gh-token :initform nil)))

(defun make-github-forge-capability (&key token (version "0.1.0"))
  (make-instance 'github-forge-capability
                 :name :forge :version version
                 :token (or token (uiop:getenv "GITHUB_TOKEN"))))

(defun gh-api (cap method endpoint &key body)
  "Call GitHub REST API."
  (let ((url (format nil "https://api.github.com~A" endpoint))
        (headers (list (cons "Accept" "application/vnd.github+json")
                       (cons "X-GitHub-Api-Version" "2022-11-28"))))
    (when (gh-token cap)
      (push (cons "Authorization" (format nil "Bearer ~A" (gh-token cap))) headers))
    (multiple-value-bind (response-body status)
        (dex:request url :method method :headers headers
                         :content (when body (with-output-to-string (s) (yason:encode body s)))
                         :want-stream nil)
      (when (<= 200 status 299)
        (yason:parse (if (stringp response-body) response-body
                         (babel:octets-to-string response-body))
                     :object-as :hash-table :object-key-fn #'identity)))))

(defmethod list-issues ((cap github-forge-capability) repo &key)
  (gh-api cap :get (format nil "/repos/~A/issues?state=open" repo)))

(defmethod get-issue ((cap github-forge-capability) repo id &key)
  (gh-api cap :get (format nil "/repos/~A/issues/~A" repo id)))

(defmethod create-issue ((cap github-forge-capability) repo title body &key)
  (let ((payload (make-hash-table :test 'equal)))
    (setf (gethash "title" payload) title
          (gethash "body" payload) body)
    (gh-api cap :post (format nil "/repos/~A/issues" repo) :body payload)))

(defmethod create-pr ((cap github-forge-capability) repo title body branch &key)
  (let ((payload (make-hash-table :test 'equal)))
    (setf (gethash "title" payload) title
          (gethash "body" payload) body
          (gethash "head" payload) branch
          (gethash "base" payload) "main")
    (gh-api cap :post (format nil "/repos/~A/pulls" repo) :body payload)))

(defmethod comment-on ((cap github-forge-capability) repo id body &key)
  (let ((payload (make-hash-table :test 'equal)))
    (setf (gethash "body" payload) body)
    (gh-api cap :post (format nil "/repos/~A/issues/~A/comments" repo id) :body payload)))

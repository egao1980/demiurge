(defpackage #:demiurge-bootstrap/bootstrap/forge-forgejo-ks
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/forge
                #:forge-capability #:list-issues #:get-issue
                #:create-issue #:create-pr #:comment-on)
  (:export #:forgejo-forge-capability #:make-forgejo-forge-capability))

(in-package #:demiurge-bootstrap/bootstrap/forge-forgejo-ks)

(defclass forgejo-forge-capability (forge-capability)
  ((base-url :initarg :base-url :reader forgejo-base-url)
   (token :initarg :token :reader forgejo-token :initform nil)))

(defun make-forgejo-forge-capability (base-url &key token (version "0.1.0"))
  (make-instance 'forgejo-forge-capability
                 :name :forge :version version
                 :base-url (string-right-trim "/" base-url)
                 :token token))

(defun forgejo-api (cap method endpoint &key body)
  (let ((url (format nil "~A/api/v1~A" (forgejo-base-url cap) endpoint))
        (headers (list (cons "Accept" "application/json")
                       (cons "Content-Type" "application/json"))))
    (when (forgejo-token cap)
      (push (cons "Authorization" (format nil "token ~A" (forgejo-token cap))) headers))
    (multiple-value-bind (response-body status)
        (dex:request url :method method :headers headers
                         :content (when body (with-output-to-string (s) (yason:encode body s)))
                         :want-stream nil)
      (when (<= 200 status 299)
        (yason:parse (if (stringp response-body) response-body
                         (babel:octets-to-string response-body))
                     :object-as :hash-table :object-key-fn #'identity)))))

(defmethod list-issues ((cap forgejo-forge-capability) repo &key)
  (forgejo-api cap :get (format nil "/repos/~A/issues?state=open" repo)))

(defmethod get-issue ((cap forgejo-forge-capability) repo id &key)
  (forgejo-api cap :get (format nil "/repos/~A/issues/~A" repo id)))

(defmethod create-issue ((cap forgejo-forge-capability) repo title body &key)
  (let ((payload (make-hash-table :test 'equal)))
    (setf (gethash "title" payload) title
          (gethash "body" payload) body)
    (forgejo-api cap :post (format nil "/repos/~A/issues" repo) :body payload)))

(defmethod create-pr ((cap forgejo-forge-capability) repo title body branch &key)
  (let ((payload (make-hash-table :test 'equal)))
    (setf (gethash "title" payload) title
          (gethash "body" payload) body
          (gethash "head" payload) branch
          (gethash "base" payload) "main")
    (forgejo-api cap :post (format nil "/repos/~A/pulls" repo) :body payload)))

(defmethod comment-on ((cap forgejo-forge-capability) repo id body &key)
  (let ((payload (make-hash-table :test 'equal)))
    (setf (gethash "body" payload) body)
    (forgejo-api cap :post (format nil "/repos/~A/issues/~A/comments" repo id) :body payload)))

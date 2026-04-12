(defpackage #:demiurge-bootstrap/bootstrap/searxng-ks
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/web-search
                #:web-search-capability #:web-search #:fetch-page)
  (:export #:searxng-web-search-capability #:make-searxng-web-search-capability))

(in-package #:demiurge-bootstrap/bootstrap/searxng-ks)

(defclass searxng-web-search-capability (web-search-capability)
  ((base-url :initarg :base-url :reader searxng-base-url
             :initform "http://localhost:8888")
   (max-results :initarg :max-results :reader searxng-max-results :initform 10)))

(defun make-searxng-web-search-capability (&key (base-url "http://localhost:8888")
                                                (max-results 10)
                                                (version "0.1.0"))
  (make-instance 'searxng-web-search-capability
                 :name :web-search :version version
                 :base-url base-url :max-results max-results))

(defun url-encode (string)
  "Minimal URL encoding for query parameters."
  (with-output-to-string (s)
    (loop for c across string
          do (cond ((alphanumericp c) (write-char c s))
                   ((char= c #\Space) (write-char #\+ s))
                   (t (format s "%~2,'0X" (char-code c)))))))

(defmethod web-search ((cap searxng-web-search-capability) query &key)
  "Search via SearXNG JSON API. Returns list of (:title :url :snippet) plists."
  (let* ((url (format nil "~A/search?q=~A&format=json"
                      (searxng-base-url cap) (url-encode query)))
         (response (handler-case
                       (dex:get url :want-stream nil)
                     (error (e)
                       (return-from web-search
                         (list (list :title "Search error"
                                     :url "" :snippet (format nil "~A" e)))))))
         (parsed (yason:parse response :object-as :hash-table :object-key-fn #'identity))
         (results (gethash "results" parsed)))
    (loop for r in (if (> (length results) (searxng-max-results cap))
                       (subseq results 0 (searxng-max-results cap))
                       results)
          collect (list :title (or (gethash "title" r) "")
                        :url (or (gethash "url" r) "")
                        :snippet (or (gethash "content" r) "")))))

(defmethod fetch-page ((cap searxng-web-search-capability) url &key)
  "Fetch a URL and return text content. Uses dexador with a timeout."
  (handler-case
      (let ((body (dex:get url :want-stream nil
                               :connect-timeout 10
                               :read-timeout 15)))
        (if (> (length body) 50000)
            (subseq body 0 50000)
            body))
    (error (e)
      (format nil "Fetch error: ~A" e))))

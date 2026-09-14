(in-package #:demiurge/tests)

(defun %echo-toml ()
  (asdf:system-relative-pathname "demiurge" "tests/fixtures/echo.toml"))

(defun %parse-app (argv)
  (cli:parse (make-app) argv))

(defmacro with-cli-io (&body body)
  `(let ((*standard-output* (make-string-output-stream))
         (*error-output* (make-string-output-stream)))
     ,@body))

(deftest cli-parse-table
  "Each subcommand + key options parse with no side effects."
  (let ((app (make-app)))
    (dolist (row
             '(("serve" ("serve" "--config" "e.toml" "--transport" "mcp"
                         "--host" "0.0.0.0" "--port" "9090")
                (:config "e.toml" :transport "mcp" :host "0.0.0.0" :port 9090)
                ())
               ("ask" ("ask" "--config" "e.toml" "What is KSAR?")
                (:config "e.toml")
                ("What is KSAR?"))
               ("research" ("research" "--config" "e.toml" "--rounds" "3"
                            "--out" "report.md" "topic")
                (:config "e.toml" :rounds 3 :out "report.md")
                ("topic"))
               ("ingest" ("ingest" "--config" "e.toml" "--source" "corpus")
                (:config "e.toml" :source "corpus")
                ())
               ("improve" ("improve" "--config" "e.toml" "--cycles" "2"
                           "--on-error" "demote")
                (:config "e.toml" :cycles 2 :on-error "demote")
                ())
               ("install" ("install" "/tmp/bundle")
                ()
                ("/tmp/bundle"))
               ("demo" ("demo" "/tmp/demo")
                ()
                ("/tmp/demo"))))
      (destructuring-bind (name argv expect-opts expect-free) row
        (multiple-value-bind (opts free)
            (cli:parse app argv)
          (ok (equal expect-free free)
              (format nil "~a free args" name))
          (loop for (key expected) on expect-opts by #'cddr
                do (ok (equal expected (cli:get-option opts key))
                       (format nil "~a ~s" name key))))))))

(deftest cli-ask-smoke
  (with-clean-registry
    (with-cli-io
      (let ((status (run-cli (list "ask" "--config" (namestring (%echo-toml))
                                   "hi"))))
        (ok (= 0 status))
        (ok (find-expert "echo"))))))

(deftest cli-serve-smoke
  (with-clean-registry
    (let ((*serve-start* nil))
      (with-cli-io
        (let ((status (run-cli (list "serve" "--config" (namestring (%echo-toml))
                                     "--transport" "mcp"))))
          (ok (= 0 status))
          (ok (find-expert "echo")))))))

(deftest cli-research-smoke
  (with-clean-registry
    (with-tmp-dir (tmp)
      (let ((out (merge-pathnames "report.md" tmp)))
        (with-cli-io
          (let ((status (run-cli (list "research"
                                       "--config" (namestring (%echo-toml))
                                       "--rounds" "1"
                                       "--out" (namestring out)
                                       "What is KSAR?"))))
            (ok (= 0 status))
            (ok (probe-file out))
            (ok (plusp (length (uiop:read-file-string out))))))))))

(deftest cli-ingest-smoke
  (with-clean-registry
    (with-cli-io
      (let ((status (run-cli (list "ingest"
                                   "--config" (namestring (%echo-toml))
                                   "--source" "corpus"))))
        (ok (= 0 status))))))

(deftest cli-improve-smoke
  (with-clean-registry
    (with-cli-io
      (let ((status (run-cli (list "improve"
                                   "--config" (namestring (%echo-toml))
                                   "--cycles" "2"
                                   "--on-error" "demote"))))
        (ok (= 0 status))))))

(deftest cli-install-smoke
  (with-clean-registry
    (clear-bundle-installs)
    (with-tmp-dir (tmp)
      (let* ((llm (mock-llm))
             (journal (task:make-in-memory-journal))
             (domain (make-echo-expert :backend llm :name "cli-bundle"))
             (packed (pack-expert domain
                                  :registry (merge-pathnames "oci/" tmp)
                                  :version "1.0.0"))
             (layout (namestring
                      (uiop:ensure-directory-pathname (getf packed :layout)))))
        (clear-expert-registry)
        (with-cli-io
          (let ((status (run-cli (list "install" layout))))
            (ok (= 0 status))
            (ok (expert-domain-p (find-expert "cli-bundle")))))))))

(deftest cli-demo-smoke
  (with-clean-registry
    (with-tmp-dir (tmp)
      (let ((toml (merge-pathnames "expert.toml" tmp))
            (queries (merge-pathnames "queries.md" tmp)))
        (uiop:copy-file (%echo-toml) toml)
        (with-open-file (out queries :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
          (format out "# comment~%hi~%ask: hello~%research: What is KSAR?~%"))
        (with-cli-io
          (let ((status (run-cli (list "demo" (namestring tmp)))))
            (ok (= 0 status))
            (ok (find-expert "echo"))))))))

(deftest cli-demo-toml-smoke
  (with-clean-registry
    (with-tmp-dir (tmp)
      (let ((expert (merge-pathnames "echo.toml" tmp))
            (demo (merge-pathnames "demo.toml" tmp))
            (queries (merge-pathnames "qs.md" tmp)))
        (uiop:copy-file (%echo-toml) expert)
        (with-open-file (out demo :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
          (write-string "expert = \"echo.toml\"
command = \"ask\"
queries = \"qs.md\"
" out))
        (with-open-file (out queries :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
          (write-string "hi~%" out))
        (with-cli-io
          (let ((status (run-cli (list "demo" (namestring tmp)))))
            (ok (= 0 status))))))))

(deftest cli-exit-code-contract
  "unknown-expert / expert-config-error / cli-parse-error map without uiop:quit."
  (with-clean-registry
    (let ((missing (namestring
                    (merge-pathnames "no-such-expert.toml"
                                     (uiop:temporary-directory)))))
      (with-cli-io
        (ok (= 1 (run-cli (list "ask" "--config" missing "hi")))
            "missing expert.toml → expert-config-error → 1")))
    (with-cli-io
      (ok (= 2 (run-cli '("ask" "--not-a-real-flag" "hi")))
          "unknown flag → cli-parse-error → 2"))
    (with-cli-io
      (ok (= 2 (run-cli '("ask" "question-without-config")))
          "missing --config → cli-usage-error → 2"))
    (with-tmp-dir (tmp)
      (let ((demo (merge-pathnames "demo.toml" tmp)))
        (with-open-file (out demo :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
          (write-string "expert = \"missing-name\"
queries = \"queries.md\"
" out))
        (with-open-file (out (merge-pathnames "queries.md" tmp)
                             :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
          (write-string "hi~%" out))
        (with-cli-io
          (ok (= 1 (run-cli (list "demo" (namestring tmp))))
              "unknown registry expert → unknown-expert → 1"))))))

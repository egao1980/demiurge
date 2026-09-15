(in-package #:demiurge/tests)

(defun %cl-dev-toml ()
  (asdf:system-relative-pathname "demiurge" "examples/cl-dev-expert.toml"))

(defun %skill-names (domain)
  (let* ((steering (expert-steering domain))
         (dirs (cond
                 ((null steering) nil)
                 ((steer:steering-source-p steering)
                  (steer:list-directives steering))
                 ((consp steering) steering)
                 (t (ignore-errors
                      (steer:list-directives (steer:coerce-steering steering)))))))
    (sort (mapcar #'steer:steer-directive-name
                  (remove-if-not (lambda (d)
                                   (eq (steer:steer-directive-kind d) :skill))
                                 dirs))
          #'string<)))

(defun %ks-name (ks)
  (string-downcase (string (bb:ks-name ks))))

(defun %corpus-basenames (domain)
  (sort (mapcar (lambda (c)
                  (file-namestring
                   (pathname (etypecase c
                               (pathname c)
                               (string c)
                               (t (princ-to-string c))))))
                (expert-corpora domain))
        #'string<))

(defun %dataset-name (domain)
  (let ((ds (first (expert-eval-suites domain))))
    (and ds (eval:eval-dataset-name ds))))

(deftest golden-cl-dev-toml-matches-lisp
  "examples/cl-dev-expert.toml ≡ make-cl-dev-expert / defexpert on identity fields."
  (with-clean-registry
    (let* ((llm (mock-llm))
           (from-lisp (make-cl-dev-expert :backend llm :name "cl-dev"))
           (from-toml (load-expert-config (%cl-dev-toml) :llm llm))
           (from-def (defexpert cl-dev
                       (:catalogue :cl-dev)
                       (:profile :personal))))
      (ok (expert-domain-p from-toml))
      (ok (equal (expert-name from-toml) (expert-name from-lisp)))
      (ok (equal (expert-name from-toml) (expert-name from-def)))
      (ok (eq (cap:catalogue-name (expert-catalogue from-toml))
              (cap:catalogue-name (expert-catalogue from-lisp))))
      (ok (eq (cap:catalogue-name (expert-catalogue from-toml))
              (cap:catalogue-name (expert-catalogue from-def))))
      (ok (eq :cl-dev (cap:catalogue-name (expert-catalogue from-toml))))
      (let ((ks-t (first (expert-ks-set from-toml)))
            (ks-l (first (expert-ks-set from-lisp))))
        (ok (agent-ks-p ks-t))
        (ok (equal (%ks-name ks-t) (%ks-name ks-l)))
        (ok (equal (agent-ks-watch ks-t) (agent-ks-watch ks-l)))
        (ok (equal (agent:ai-agent-instructions (agent-ks-agent ks-t))
                   (agent:ai-agent-instructions (agent-ks-agent ks-l)))))
      (ok (equal (%skill-names from-toml) (%skill-names from-lisp)))
      (ok (member "cl-stack.md" (%corpus-basenames from-lisp) :test #'equal))
      (ok (find-if (lambda (c)
                     (search "corpus" (if (stringp c) c (namestring c))
                             :test #'char-equal))
                   (expert-corpora from-toml))
          "toml corpora refs include the bundled corpus")
      (ok (equal (%dataset-name from-toml) (%dataset-name from-lisp)))
      (ok (equal "cl-dev" (%dataset-name from-toml))))))

(deftest expert-config-missing-name
  (ok (signals (load-expert-config
                (%write-tmp-toml "[expert]
description = \"no name\"
"))
               'expert-config-error)))

(deftest expert-config-bad-name-type
  (ok (signals (load-expert-config
                (%write-tmp-toml "[expert]
name = [\"not-a-string\"]
"))
               'expert-config-error)))

(deftest expert-config-missing-expert-section
  (ok (signals (load-expert-config
                (%write-tmp-toml "[profile]
kind = \"personal\"
"))
               'expert-config-error)))

(deftest unknown-key-continue-lists-valid-keys
  "Unknown key signals UNKNOWN-EXPERT-CONFIG-KEY; CONTINUE proceeds and lists valid keys."
  (let* ((path (%write-tmp-toml "
[expert]
name = \"unknown-key-demo\"
catalogue = \"world\"
mystery = true
"))
         (report nil)
         (domain
          (handler-bind
              ((unknown-expert-config-key
                (lambda (c)
                  (let ((r (find-restart 'continue c)))
                    (ok (not (null r)))
                    (setf report (and r (princ-to-string r)))
                    (ok (member "mystery"
                                (list (unknown-expert-config-key-name c))
                                :test #'equal))
                    (ok (member "name"
                                (unknown-expert-config-valid-keys c)
                                :test #'equal))
                    (invoke-restart r)))))
            (load-expert-config path :llm (mock-llm)))))
    (ok (expert-domain-p domain))
    (ok (equal "unknown-key-demo" (expert-name domain)))
    (ok (and report (search "name" report))
        "continue restart report lists valid keys")))

(deftest undeclared-op-is-rejected
  "Config can only reference declared ops — invented names are invalid-expert."
  (let ((path (%write-tmp-toml "
[expert]
name = \"cap-bound\"
catalogue = \"world\"

[[ks]]
name = \"echo\"
watch = \"(:prompt)\"
tool-grants = [\"invented-op\"]
")))
    (ok (signals (load-expert-config path :llm (mock-llm))
                 'invalid-expert))))

(deftest expert-config-pack-install-round-trip
  "config → pack-expert → install-expert is still runnable."
  (with-clean-registry
    (clear-bundle-installs)
    (with-tmp-dir (tmp)
      (let* ((llm (mock-llm))
             (journal (task:make-in-memory-journal))
             (domain (load-expert-config (%cl-dev-toml) :llm llm))
             (packed (pack-expert domain
                                  :registry (merge-pathnames "oci/" tmp)
                                  :version "1.0.0")))
        (ok (expert-bundle-manifest-p (getf packed :manifest)))
        (clear-expert-registry)
        (let ((result (install-expert packed
                                      :journal journal
                                      :task-id "expert-config-round-trip"
                                      :llm llm)))
          (ok (equal "cl-dev" (getf result :name)))
          (let ((installed (find-expert "cl-dev")))
            (ok (expert-domain-p installed))
            (ok (eq :cl-dev (cap:catalogue-name (expert-catalogue installed))))
            (let ((board (run-expert installed :trigger '(:prompt "hi"))))
              (ok (bb:section-bound-p board :result))
              (ok (stringp (bb:read-section board :result))))))))))

(deftest expert-config-llm-catalog-and-websearch-from-toml
  "[[llm.catalog]] builds a profile catalog; [websearch] binds the backend."
  (with-clean-registry
    (let* ((path (%write-tmp-toml "
[expert]
name = \"catalog-demo\"

[llm]
default-model = \"mock\"

[[llm.catalog]]
name = \"mock\"
kind = \"mock\"
prefix = \"from-toml: \"

[websearch]
kind = \"mock\"
"))
           (domain (load-expert-config path))
           (prof (expert-profile domain))
           (llm (resolve-profile-llm prof)))
      (ok (expert-domain-p domain))
      (ok (deployment-profile-p prof))
      (ok (equal "mock" (profile-default-model prof)))
      (ok (profile-llm-catalog prof))
      (ok (llm:llm-backend-p llm))
      (ok (find "mock" (mapcar #'llm:llm-provider-name
                               (llm:list-providers (profile-llm-catalog prof)))
                :test #'equal))
      (ok (web:websearch-backend-p web:*websearch-backend*))
      (ok (not (web:searxng-backend-p web:*websearch-backend*))))))

(deftest expert-config-openai-compat-and-searxng-from-toml
  "Live kinds in expert.toml construct openai-compat + SearXNG (no env URLs)."
  (with-clean-registry
    (let* ((path (%write-tmp-toml "
[expert]
name = \"live-catalog\"

[llm]
default-model = \"lmstudio\"

[[llm.catalog]]
name = \"lmstudio\"
kind = \"openai-compat\"
base-url = \"http://127.0.0.1:1234/v1\"
model = \"prism-ml/bonsai-27b\"
api-key-env = \"LM_API_TOKEN\"

[websearch]
kind = \"searxng\"
base-url = \"http://127.0.0.1:8888\"
"))
           (domain (load-expert-config path))
           (prof (expert-profile domain))
           (llm (bare-llm-backend (resolve-profile-llm prof)))
           (sum (profile-backend-summary prof)))
      (ok (deployment-profile-p prof))
      (ok (equal "lmstudio" (profile-default-model prof)))
      (ok (find "lmstudio" (getf sum :providers) :test #'equal))
      (ok (eq (type-of llm) (getf sum :llm-class)))
      (ok (equal "prism-ml/bonsai-27b" (getf sum :backend-model)))
      (ok (not (search "MOCK" (string (type-of llm)))))
      (ok (web:searxng-backend-p web:*websearch-backend*))
      (ok (equal "http://127.0.0.1:8888"
                 (web:searxng-base-url web:*websearch-backend*))))))

(deftest ensure-http-backend-prefers-async
  (let ((http.p:*http-backend* nil)
        (http.p:*http-client* nil))
    (ok (demiurge::%ensure-http-backend))
    (ok (typep http.p:*http-backend* 'http.async:async-backend))))

(deftest expert-config-workspace-root
  (let* ((path (%write-tmp-toml "
[expert]
name = \"ws\"
[workspace]
root = \".\"
seed = \"KSAR blackboard\"
"))
         (domain (load-expert-config path))
         (cfg (profile-config (expert-profile domain)))
         (root (demiurge-config-workspace-root cfg)))
    (ok (and root (plusp (length root))))
    (ok (pathlib:directory-p (pathlib:from-string root)))
    (ok (equal "KSAR blackboard" (demiurge-config-workspace-seed cfg)))))

(deftest expert-config-workspace-root-collapses-dotdot
  "demos/deep-research root=\"../../\" must become the checkout, not a lexical ../.. path."
  (with-tmp-dir (root)
    (let* ((nested (ensure-directories-exist
                    (merge-pathnames "demos/deep/" root)))
           (path (merge-pathnames "expert.toml" nested)))
      (%write-tree-file root "note.md" "KSAR")
      (with-open-file (out path :direction :output :if-exists :supersede
                           :if-does-not-exist :create)
        (write-string "[expert]
name = \"ws\"
[workspace]
root = \"../../\"
" out))
      (let* ((domain (load-expert-config path))
             (cfg (profile-config (expert-profile domain)))
             (resolved (demiurge-config-workspace-root cfg)))
        (ok (and resolved (null (search ".." resolved))))
        (ok (pathlib:exists-p (pathlib:join (pathlib:from-string resolved)
                                            "note.md")))))))

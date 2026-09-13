(in-package #:demiurge/tests)

(deftest load-config-defaults
  (let ((demiurge::*demiurge-config* nil))
    (let ((cfg (load-demiurge-config :env nil)))
      (ok (demiurge-config-p cfg))
      (ok (= 4 (demiurge-config-agenda-max-concurrency cfg)))
      (ok (= 10 (demiurge-config-ksar-timeout-seconds cfg)))
      (ok (= 8 (demiurge-config-session-window-turns cfg)))
      (ok (equal "mock" (demiurge-config-llm-default-model cfg)))
      (ok (null (demiurge-config-improve-enabled cfg))))))

(deftest toml-overrides-defaults
  (let* ((path (%write-tmp-toml "
[agenda]
max-concurrency = 2

[ksar]
timeout-seconds = 5

[session]
window-turns = 3

[llm]
default-model = \"local\"

[improve]
enabled = true
"))
         (demiurge::*demiurge-config* nil))
    (let ((cfg (load-demiurge-config :path path :env nil)))
      (ok (= 2 (demiurge-config-agenda-max-concurrency cfg)))
      (ok (= 5 (demiurge-config-ksar-timeout-seconds cfg)))
      (ok (= 3 (demiurge-config-session-window-turns cfg)))
      (ok (equal "local" (demiurge-config-llm-default-model cfg)))
      (ok (eq t (demiurge-config-improve-enabled cfg))))))

(deftest env-overrides-toml
  (let* ((path (%write-tmp-toml "
[agenda]
max-concurrency = 2

[ksar]
timeout-seconds = 5

[llm]
default-model = \"file-model\"
"))
         (demiurge::*demiurge-config* nil)
         (cfg (load-demiurge-config
               :path path
               :prefix "DEMIURGE"
               :env t
               :environ '(("DEMIURGE_AGENDA__MAX-CONCURRENCY" . "9")
                          ("DEMIURGE_KSAR__TIMEOUT-SECONDS" . "15")
                          ("DEMIURGE_LLM__DEFAULT-MODEL" . "env-model")))))
    (ok (= 9 (demiurge-config-agenda-max-concurrency cfg)))
    (ok (= 15 (demiurge-config-ksar-timeout-seconds cfg)))
    (ok (equal "env-model" (demiurge-config-llm-default-model cfg)))))

(deftest env-overrides-defaults-without-file
  (let ((demiurge::*demiurge-config* nil))
    (let ((cfg (load-demiurge-config
                :prefix "DEMIURGE"
                :env t
                :environ '(("DEMIURGE_AGENDA__MAX-CONCURRENCY" . "7")))))
      (ok (= 7 (demiurge-config-agenda-max-concurrency cfg)))
      (ok (= 10 (demiurge-config-ksar-timeout-seconds cfg))))))

(deftest controller-reads-config-concurrency
  (let* ((path (%write-tmp-toml "
[agenda]
max-concurrency = 1
"))
         (demiurge::*demiurge-config* (load-demiurge-config :path path :env nil))
         (domain (make-expert-domain :name "cfg-conc"))
         (controller (make-controller domain)))
    (ok (eql 1 (bb:blackboard-max-concurrency
                (controller-blackboard controller))))))

(defsystem "demiurge"
  :class :package-inferred-system
  :version "0.1.0"
  :license "MIT"
  :author "egao1980"
  :description "Self-improving blackboard agent - core (implementation-agnostic)"
  :depends-on ("alexandria" "bordeaux-threads" "local-time" "log4cl"
               "demiurge/main")
  :in-order-to ((test-op (test-op "demiurge/tests"))))

(defsystem "demiurge/tests"
  :class :package-inferred-system
  :depends-on ("rove" "demiurge"
               "demiurge/tests/blackboard-test"
               "demiurge/tests/capabilities-test"
               "demiurge/tests/workspace-test"
               "demiurge/tests/events-test"
               "demiurge/tests/ks-test"
               "demiurge/tests/introspection-test")
  :perform (test-op (o c) (symbol-call :rove :run c)))

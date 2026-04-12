(defsystem "demiurge-bootstrap"
  :class :package-inferred-system
  :version "0.1.0"
  :license "MIT"
  :author "egao1980"
  :description "Demiurge bootstrap KS implementations"
  :depends-on ("demiurge"
               "cl-json-rpc2" "cl-openai" "cl-mcp-sdk" "cl-a2a"
               "cl-ppcre" "dexador" "yason" "hunchentoot" "lparallel"
               "demiurge-bootstrap/bootstrap/main"))

(defsystem "demiurge-bootstrap/tests"
  :class :package-inferred-system
  :depends-on ("rove" "demiurge-bootstrap"
               "demiurge-bootstrap/tests/bootstrap-test")
  :perform (test-op (o c) (symbol-call :rove :run c)))

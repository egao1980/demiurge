(defsystem "demiurge-bootstrap"
  :class :package-inferred-system
  :version "0.1.0"
  :license "MIT"
  :author "egao1980"
  :description "Demiurge bootstrap KS implementations"
  :depends-on ("demiurge"
               "cl-json-rpc2" "cl-openai" "cl-mcp-sdk" "cl-a2a"
               "cl-ppcre" "dexador" "yason"
               "demiurge-bootstrap/main"))

(defpackage #:demiurge/cli
  (:use #:cl #:demiurge)
  (:nicknames #:stack-demiurge.cli)
  (:local-nicknames (#:cli #:cli-protocol)
                    (#:bb #:blackboard-protocol)
                    (#:serve #:demiurge/serve)
                    (#:ingest #:demiurge/ingest)
                    (#:improve #:demiurge/improve)
                    (#:wf #:demiurge/workflows)
                    (#:bundle #:demiurge/bundle)
                    (#:toml #:toml-protocol)
                    (#:doc #:doc-extract-protocol))
  (:export
   #:*serve-start*
   #:make-app
   #:run-cli
   #:main)
  (:documentation
   "Thin CLI over existing demiurge entry functions (cli-protocol + clingon)."))

(in-package #:demiurge/cli)

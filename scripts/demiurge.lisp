;;;; ros -l scripts/demiurge.lisp -- ask --config examples/cl-dev-expert.toml "question"
(require :asdf)
(asdf:load-system "demiurge/cli")
(demiurge/cli:main)

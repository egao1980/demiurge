(in-package #:demiurge/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'run-decision-demo)
    (load (asdf:system-relative-pathname "demiurge" "examples/decision-expert.lisp"))))

(deftest decision-expert-demo-runs
  (multiple-value-bind (board rec)
      (run-decision-demo (make-broadcast-stream))
    (ok (bb:section-bound-p board :decision))
    (ok (equal "kev-4b" (decision-record-model rec)))
    (ok (= 4/5 (cdr (assoc :allow (decision-record-mass rec)))))))

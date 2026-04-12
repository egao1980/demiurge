(defpackage #:demiurge/src/capabilities/forge
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/protocol #:capability #:capability-operations
                #:make-capability-operation)
  (:import-from #:demiurge/src/capabilities/macros #:defcapability)
  (:export #:forge-capability #:list-issues #:get-issue
           #:create-issue #:create-pr #:comment-on))

(in-package #:demiurge/src/capabilities/forge)

(defcapability :forge
  "Issue and PR tracker integration"
  (:operation list-issues ((repo string))
   :returns list
   :doc "List issues for a repository")
  (:operation get-issue ((repo string) (id t))
   :returns t
   :doc "Get issue details")
  (:operation create-issue ((repo string) (title string) (body string))
   :returns t
   :doc "Create a new issue")
  (:operation create-pr ((repo string) (title string) (body string) (branch string))
   :returns t
   :doc "Create a pull request")
  (:operation comment-on ((repo string) (id t) (body string))
   :returns t
   :doc "Comment on an issue or PR"))

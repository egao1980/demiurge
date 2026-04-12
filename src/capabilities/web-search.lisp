(defpackage #:demiurge/src/capabilities/web-search
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/protocol #:capability #:capability-operations
                #:make-capability-operation)
  (:import-from #:demiurge/src/capabilities/macros #:defcapability)
  (:export #:web-search-capability #:web-search #:fetch-page))

(in-package #:demiurge/src/capabilities/web-search)

(defcapability :web-search
  "Web search and page fetching via SearXNG"
  (:operation web-search ((query string))
   :returns list
   :doc "Search the web, returns list of (:title :url :snippet) plists")
  (:operation fetch-page ((url string))
   :returns string
   :doc "Fetch a URL and return text content"))

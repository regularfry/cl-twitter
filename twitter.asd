;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :twitter
  :serial t
  ;; add new files to this list:
  :components ((:file "package") (:file "twitter"))
  :depends-on (#+nil :cl-ppcre))

;;; -*- mode: lisp; indent-tabs: nil -*-
(defpackage :twitter
  (:use :cl)
  (:shadow "SEARCH")
  (:export #:search
           #:trends
           #:trends-current
           #:trends-daily
           #:trends-weekly
           #:statuses-public-timeline))

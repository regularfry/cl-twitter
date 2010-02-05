;;; -*- mode: lisp; indent-tabs: nil -*-
(defpackage :twitter
  (:use :cl)
  (:shadow "SEARCH")
  (:export #:search
           #:trends
           #:trends-current
           #:trends-daily
           #:trends-weekly
           #:statuses-public-timeline
           #:statuses-friends-timeline
           #:statuses-home-timeline
           #:statuses-mentions
           #:statuses-name-timeline
           #:statuses-retweeted-by-me
           #:statuses-retweeted-to-me
           #:statuses-retweets-of-me
           #:statuses-user-timeline))

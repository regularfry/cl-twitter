;;; -*- mode: lisp; indent-tabs: nil -*-
(defun make-requires ()
  (asdf:oos 'asdf:load-op 'fiveam)
  (asdf:oos 'asdf:load-op 'drakma)
  (asdf:oos 'asdf:load-op 'cl-json))
(make-requires)
(defpackage :twitter
  (:use :cl
	:it.bese.FiveAM
	:json))
(in-package :twitter)

(defun get-credentials (username password)
  (list username password))

(defmacro get-http-string (url &optional credentials)
  `(flexi-streams:octets-to-string
    (drakma:http-request ,url 
			 ,@(if credentials 
			       (list :basic-authorization credentials)
			       () ))))

(defmacro post-http-string (url body-content &optional credentials)
  `(flexi-streams:octets-to-string
    (drakma:http-request ,url
                         :method :post
                         ,@(if credentials
                               (list :basic-authorization credentials)
                               ())
                         ,@(if body-content
                               (list :content body-content)
                               ()))))

(defun get-data-from-json-url (credentials query-url)
  (json:decode-json-from-string 
   (get-http-string query-url credentials)))

(defun post-data-to-json-url (credentials query-url &optional data-alist)
  (json:decode-json-from-string
   (post-http-string query-url (json:encode-json-alist-to-string data-alist) credentials)))

(defun get-timeline (credentials url)
  (get-data-from-json-url url credentials))
(defun get-public-timeline (&optional credentials)
  (get-timeline
   "http://twitter.com/statuses/public_timeline.json"
   credentials)) 

(defvar *private-timeline-urls* 
  '((friends . "http://twitter.com/statuses/friends_timeline.json")
    (home . "http://api.twitter.com/1/statuses/home_timeline.json")
    (user . "http://twitter.com/statuses/user_timeline.json")
    (mentions . "http://twitter.com/statuses/mentions.json")
    (retweeted-by-me . "http://api.twitter.com/1/statuses/retweeted_by_me.json")
    (retweeted-to-me . "http://api.twitter.com/1/statuses/retweeted_to_me.json")
    (retweets-of-me . "http://api.twitter.com/1/statuses/retweets_of_me.json")))

(defun get-private-timeline-names ()
  (mapcar 'car *private-timeline-urls*))

(defun get-private-timeline (credentials timeline-name)
  (let ((timeline-pair (assoc timeline-name *private-timeline-urls*)))
    (if timeline-pair
        (get-timeline credentials (cdr timeline-pair))
        ())))

(defun restful-status-url (&optional op-name status-id)
  (if status-id
      (format nil "http://twitter.com/statuses/~(~a~)/~d.json" op-name status-id)
      "http://twitter.com/statuses/update.json"))

(defun status-get (credentials status-id)
  (get-data-from-json-url credentials (restful-status-url 'show status-id)))

;;; This function builds the alist which will eventually be
;;; json-encoded and posted. It leaves NILs lying around
;;; for missing values, but that doesn't matter - they get stripped
;;; out by json:encode-json-alist-to-string.
(defun build-post-status-alist (status in-reply-to-id lat long)
  `(("status" . ,status)
    ,(if in-reply-to-id `("in_reply_to_status_id" . ,in-reply-to-id))
    ,(if lat `("lat" . ,lat))
    ,(if long `("long" . ,long))))

(defun status-post (credentials status &key in-reply-to-id lat long)
  (post-data-to-json-url credentials
                         (restful-status-url)
                         (build-post-status-alist (status in-reply-to-id lat long))))
(defun status-delete (credentials status-id)
  (post-data-to-json-url credentials
                         (restful-status-url 'destroy status-id)))

(defun status-retweet (credentials status-id)
  (post-data-to-json-url credentials
                         (restful-status-url 'retweet status-id)))

(defun retweets-url (status-id count)
  (format nil
          "http://api.twitter.com/1/statuses/retweets/~d.json~[~:;&count=~d~]"
          status-id (if (numberp count) count 0) count))

(defun status-retweets (credentials status-id &optional count)
  (get-data-from-json-url credentials (retweets-url status-id count)))
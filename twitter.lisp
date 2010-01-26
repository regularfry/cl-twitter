;;; -*- mode: lisp; indent-tabs: nil -*-
(defun make-requires ()
  (asdf:oos 'asdf:load-op 'fiveam)
  (asdf:oos 'asdf:load-op 'drakma)
  (asdf:oos 'asdf:load-op 'cl-json))
(defpackage :twitter
  (:use :cl
	:it.bese.FiveAM
	:cl-json))
(in-package :twitter)

(defmacro get-http-string (url &optional credentials)
  `(flexi-streams:octets-to-string
    (drakma:http-request ,url 
			 ,@(if credentials 
			       (list :basic-authorization credentials)
			       () ))))

(defun get-data-from-json-url (query-url &optional credentials)
  (json:decode-json-from-string 
     (get-http-string query-url credentials)))

(defun get-timeline (url &optional username password)
  (get-data-from-json-url url (list username password)))
(defun get-public-timeline ()
  (get-timeline
   "http://twitter.com/statuses/public_timeline.json")) 

(defvar *private-timeline-urls* 
  '(("friends" "http://twitter.com/statuses/friends_timeline.json")
    ("home" "http://api.twitter.com/1/statuses/home_timeline.json")
    ("user" "http://twitter.com/statuses/user_timeline.json")
    ("mentions" "http://twitter.com/statuses/mentions.json")
    ("retweeted-by-me" "http://api.twitter.com/1/statuses/retweeted_by_me.json")))

(loop for (name url) in *private-timeline-urls*
   do
     (setf (symbol-function 
	    (make-symbol (concatenate 'string "get-private-" name "-timeline")))
	   #'(lambda (un pw) (get-timeline url un pw))))

(defun #(intern "add2") (n)
   (+ n 2))


;;; (in-package :twitter.test)

(test get-xmls
  (is (equal "a" (xmls-tag (get-xmls "<a/>"))))
  (is (equal '(("b" "c")) (xmls-attrs (get-xmls "<a b='c'/>"))))
  (is (equal "b" (xmls-tag (first (xmls-children (get-xmls "<a><b/></a>")))))))





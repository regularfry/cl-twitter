;;; -*- mode: lisp; indent-tabs: nil -*-
(defun make-requires ()
  (asdf:oos 'asdf:load-op 'fiveam)
  (asdf:oos 'asdf:load-op 'drakma)
  (asdf:oos 'asdf:load-op 'cl-json))
(make-requires)
(defpackage :twitter
  (:use :cl)
  (:export :search
           :trends
           :trends-current
           :trends-daily
           :trends-weekly
           ))
(in-package :twitter)

;;; For my reference rather than anything else right now
(defvar *exports*)

(defun get-credentials (username password)
  (list username password))

(defmacro get-http-string (url &optional credentials query-data)
  `(flexi-streams:octets-to-string
    (drakma:http-request ,url 
			 ,@(if credentials 
			       (list :basic-authorization credentials)
			       () )
                         ,@(if query-data
                               (list :parameters query-data)) )))

(defmacro post-http-data (url query-data &optional credentials)
  `(flexi-streams:octets-to-string
    (drakma:http-request ,url
                         :method :post
                         ,@(if credentials
                               (list :basic-authorization credentials)
                               ())
                         ,@(if query-data
                               (list :parameters query-data)
                               ()))))

(defun get-data-from-json-url (credentials query-url &optional query-data)
  (json:decode-json-from-string 
   (get-http-string query-url credentials query-data)))

(defun post-data-to-json-url (credentials query-url &optional data-alist)
  (json:decode-json-from-string
   (post-http-data query-url data-alist credentials)))

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
                         (build-post-status-alist status in-reply-to-id lat long)))

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






;;; Search functions

;;; We want to match the URL name with the function name, which means
;;; that we need to shadow CL:SEARCH. 
(let ((scope (multiple-value-bind (_ s) (find-symbol "SEARCH" :twitter) s)))
  ;; If we're going to have problems, the symbol scope will be :INHERITED.
  (if (eq scope :INHERITED) (shadow "SEARCH" :twitter)))

;;; We can be called with all the usual twitter search parameters
;;; (except callback), and we return a list of alists.
(defun search (query &key lang locale results-per-page page since-id geocode-lat geocode-long geocode-radius show-user)
  (get-data-from-json-url ()
                          "http://search.twitter.com/search.json"
                          `(("q" . ,query)
                            ;; Again, rely on json:encode-blah to
                            ;; get rid of nils for us
                            ,(if lang `("lang" . ,lang))
                            ,(if locale `("locale" . ,locale))
                            ,(if results-per-page `("rpp" . ,results-per-page))
                            ,(if page `("page" . ,page))
                            ,(if since-id `("since_id" . ,since-id))
                            ,(if geocode-lat `("geocode" . ,(format nil "~a,~a,~a" geocode-lat geocode-long geocode-radius)))
                            ,(if show-user '("show_user" . "true")))))

(defun trends-url (&optional period)
  (if period
      (format nil "http://search.twitter.com/trends/~(~a~).json" period)
      "http://search.twitter.com/trends.json"))

(defun trends ()
  (get-data-from-json-url () (trends-url)))

(defun trends-current (&optional exclude-hashtags?)
  (get-data-from-json-url ()
                          (trends-url 'current)
                          (if exclude-hashtags? '(("exclude" . "hashtags")))))
;;; If given, date should be YYYY-MM-DD
(defun trends-daily (&optional date exclude-hashtags?)
  (get-data-from-json-url ()
                          (trends-url 'daily)
                          `(,(if date `("date" . ,date))
                             ,(if exclude-hashtags? '("exclude" . "hashtags")))))
(defun trends-weekly (&optional date exclude-hashtags?)
  (get-data-from-json-url ()
                          (trends-url 'weekly)
                          `(,(if date `("date" . ,date))
                             ,(if exclude-hashtags? '("exclude" . "hashtags")))))

;;; -*- mode: lisp; indent-tabs: nil -*-
(in-package :twitter)


;;; Useful tidbits from On Lisp
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest names)
  `(progn
     ,@(mapcar #'(lambda (pair)
                   `(abbrev ,@pair))
               (group names 2))))

;;; Shonky, but works for twitter urls (not google, though - look into this)
(defmacro decode-http-request (method url &rest args)
  `(flexi-streams:octets-to-string
    (drakma:http-request ,url
                         :method ,method
                         ,@args)))

(abbrevs dbind destructuring-bind
         mvbind multiple-value-bind
         http decode-http-request
         dejson json:decode-json-from-string)


(defun get-credentials (username password)
  (list username password))


(defun kif (key param) (if param (list key param)))
;;; The idea here is basically to filter a list of key-value options
;;; to only those with values
(defun kifs (&rest options)
  (apply #'append (mapcar #'(lambda (x) (apply #'kif x)) (group options 2))))

(defmacro get-http-string (url &optional credentials query-data)
  `(http :get ,url 
         ,@(kifs :basic-authorization credentials
                 :parameters query-data)))

(defmacro post-http-data (url &optional credentials query-data)
  `(http :post ,url
         ,@(kifs :basic-authorization credentials
                 :parameters query-data)))

(defun get-data-from-json-url (credentials query-url &optional query-data)
  (dejson 
   (get-http-string query-url credentials query-data)))

(defun post-data-to-json-url (credentials query-url &optional data-alist)
  (dejson
   (post-http-data query-url credentials data-alist)))

(defun get-timeline (credentials url)
  (get-data-from-json-url credentials url))





(defun statuses-public-timeline (&optional credentials)
  (get-timeline
   "http://twitter.com/statuses/public_timeline.json"
   credentials)) 

(defvar *private-timeline-urls* 
  '((friends-timeline "http://twitter.com/statuses/friends_timeline.json")
    (user-timeline    "http://twitter.com/statuses/user_timeline.json")
    (mentions         "http://twitter.com/statuses/mentions.json")
    (home-timeline    "http://api.twitter.com/1/statuses/home_timeline.json")
    (retweeted-by-me  "http://api.twitter.com/1/statuses/retweeted_by_me.json")
    (retweeted-to-me  "http://api.twitter.com/1/statuses/retweeted_to_me.json")
    (retweets-of-me   "http://api.twitter.com/1/statuses/retweets_of_me.json")))

(defun def-private-timeline (fname url)
  (setf (symbol-function (intern (format nil "STATUSES-~a" fname) *package*))
        #'(lambda (credentials) (get-timeline credentials url))))

;;; Here we make all the timeline functions
(dolist (pair *private-timeline-urls*)
  (dbind (name url) pair
    (def-private-timeline name url)))



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
;;; that we need to shadow CL:SEARCH. This is done in packages.lisp.

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

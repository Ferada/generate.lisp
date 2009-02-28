;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: generate; -*-

(in-package #:generate)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-doctype (root public system)
    (concatenate 'string "<!DOCTYPE " root " PUBLIC \"" public "\" \"" system "\">"))

  (defmacro define-constant (name value &optional doc)
    "Defines a constant even if it's already bound."
    `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
       ,@(when doc (list doc)))))

(define-constant +dtd-xhtml-11-public+ "-//W3C//DTD XHTML 1.1//EN")
(define-constant +dtd-xhtml-11-system+ "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd")
(define-constant +dtd-xhtml-11+ (make-doctype "html" +dtd-xhtml-11-public+ +dtd-xhtml-11-system+))

(define-constant +decl-xml-10+ "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
(define-constant +xmlns-xhtml+ "http://www.w3.org/1999/xhtml")
(define-constant +xmlns-xsl+ "http://www.w3.org/1999/XSL/Transform")

(define-constant +standard-prologue-xhtml+ (concatenate 'string +decl-xml-10+ +dtd-xhtml-11+))

(defvar *web-stream* NIL)
(defvar website-title "Crosslink")
(defvar website-navigation '(("Blag" . "blag-0.xhtml")
			     ("Universität" . "university.xhtml")
			     ("Programmierung" . "programming.xhtml")
			     ("Kontakt" . "contact.xhtml")))
(defvar *website-static* T)
(defvar *website-time-format* +rfc-1123-format+)

(defvar *tags-page* "tags.xhtml")
(defvar *timeline-page* "timeline.xhtml")

(defextension (include-plain :arguments ((pathname :required))
			     :insertp t)
  (ecase phase
    (:parse)
    (:render
     (with-open-file (stream (markdown::find-include-file pathname))
       (let ((seq (make-string (file-length stream))))
	 (read-sequence seq stream)
	 seq)))))

(defun res (file type)
  (ecase type
    (:media (conc "media/" file))
    (:documents (conc "media/documents/" file))))

(defun png (file &optional (type :media))
  (res (conc file ".png") type))

(defmacro with-html (&body body)
  `(with-html-output (*web-stream* *web-stream*) ,@body))

(defun image (src &optional title (alt title))
  (with-html
    (:img :src src :alt alt :title title)))

(defun princ-to-string-downcase (object)
  (string-downcase (princ-to-string object)))

(defun link (url &optional (fun/str url) &key (title fun/str) class id rel rev)
  (with-html
      (:a :href url :title title :class class :id id :rel rel :rev rev
	  (etypecase fun/str
	    (symbol (htm (princ-to-string-downcase fun/str)))
	    (function (funcall fun/str))
	    (string (str fun/str))
	    (null NIL)))))

(defmacro with-link ((url &optional title &key class id rel rev) &body body)
  `(link ,url (lambda () (with-html ,@body))
	 :title ,title :class ,class :id ,id :rel ,rel :rev ,rev))

(defun link/image (url src &optional title (alt title) class id rel rev)
  (with-link (url title :class class :id id :rel rel :rev rev)
    (image src title alt)))

(defun stylesheet (pathname)
  (with-html
    (:link :rel "stylesheet" :type "text/css" :href pathname)))

(defun script (pathname)
  (with-html
    (:script :src pathname :type "text/javascript")))

(defun website-navigation ()
  (with-html
    (:ul :id "site-navigation"
	 (dolist (item website-navigation)
	   (htm (:li (link (cdr item) (car item))))))))

(defun standard-template (fun &optional title (lang 'en) &aux (str-lang (if lang (princ-to-string-downcase lang) "en")))
  (with-html-output (*web-stream* *web-stream* :prologue (str +standard-prologue-xhtml+))
    (:html
     :xmlns +xmlns-xhtml+
     :xmlns\:html +xmlns-xhtml+
     :xml\:lang str-lang
     (:head :profile "http://gmpg.org/xfn/11"
      (:meta :http-equiv "Content-Type" :content "application/xhtml+xml; charset=utf-8")
      (:title (str website-title)
	      (when title (str (conc " - " (princ-to-string title)))))
      (stylesheet "media/light.css"))
     (:body
      (:div :id "header" "H41L 3R1S!")
      (website-navigation)
      (:div :id "content" (funcall fun))
      (:div :id "credits"
	    (:p "created using " (link "http://weitz.de/cl-who/" "CL-WHO") ", " (link "http://common-lisp.net/project/cl-markdown/" "CL-MARKDOWN") " and " (link "http://www.sbcl.org/" "SBCL") ", &copy; 2008-2009 by Olof-Joachim Frahm")
	    (:p "all source code is provided under the GPL version 3.0 or higher, all media content "
		"including the website under a "
		(link "http://creativecommons.org/licenses/by-nc-sa/3.0/de/"
		      "Creative Commons Attribution-Noncommercial-Share Alike 3.0 Germany Licence"
		      :title "Creative Commons Licence"
		      :rel "licence")))
      (:ul :id "icons"
	   (:li (link/image "http://common-lisp.net/"
			    (png "lisp")
			    "Made with secret alien technology"))
	   (:li (link/image "http://creativecommons.org/licenses/by-nc-sa/3.0/de/"
			    (png "cc-by-nc-sa-de-80x15")
			    "Creative Commons Attribution Noncommercial Share Alike 3.0 Germany Licence"
			    "Creative Commons Licence"))
	   (:li (link/image "http://validator.w3.org/check/referer"
			    (png "w3c-xhtml11-valid")
			    "Valid XHTML 1.1 Strict!"))
	   (:li (link/image "http://jigsaw.w3.org/css-validator/check/referer"
			    (png "w3c-css-valid")
			    "Valid CSS 2!"))
	   (:li (link/image "http://www.ccc.de/"
			    (png "hail-discordia")
			    "All Hail Discordia!"))
	   (:li (link/image "http://gmpg.org/xfn/"
			    (png "xfn")
			    "XHTML Friends Network"))
	   (:li (link/image "http://theatreayoo.deviantart.com/"
			    (png "deviantart")
			    "My page at Deviantart"
			    :rel "me"))
	   (:li (link/image "http://www.catb.org/hacker-emblem/"
			    (png "hacker")
			    "Hacker :p"))
	   (:li (image (png "dead-plants") "Plants are living beings too!" "Dead Plants")))
      (:div :id "footer" "4LL H41L D1SCORD14!")))))

(defmacro with-standard-template ((&key title (lang ''en)) &body body)
  `(standard-template (lambda () (with-html ,@body)) ,title ,lang))

;; TODO: memoize and or cache this using modification time
(defun embedded-meta-data (pathname)
  (with-open-file (stream pathname)
    (do ((char (read-char stream nil stream)
	       (read-char stream nil stream)))
	((or (eq char #\Newline) (eq char stream)))
      (when (eql char #\()
	(unread-char char stream)
	(return-from embedded-meta-data (read stream))))))

(defun split-seconds (seconds)
  (setq seconds (floor seconds))
  (let ((min) (hour) (day) (week))
    (let ((tmp (floor (/ seconds (* 60 60 24 7)))))
      (setq week tmp
	    seconds (- seconds (* tmp 60 60 24 7))))
    (let ((tmp (floor (/ seconds (* 60 60 24)))))
      (setq day tmp
	    seconds (- seconds (* tmp 60 60 24))))
    (let ((tmp (floor (/ seconds (* 60 60)))))
      (setq hour tmp
	    seconds (- seconds (* tmp 60 60))))
    (let ((tmp (floor (/ seconds 60))))
      (setq min tmp
	    seconds (- seconds (* tmp 60))))
    (unless (> week 4)
      (values seconds min hour day week))))

(defun format-timediff (action time-a time-b)
  (multiple-value-bind (sec min hour day week)
      (split-seconds (timestamp-difference time-a time-b))
    (when sec
      (let ((fmt-sec (unless (or (eq sec 0)
				 (> hour 0)
				 (> week 0)
				 (> day 0))
		       (format NIL "~:[~D~;~R~] second~:p" (<= sec 12) sec)))
	    (fmt-min (unless (or (eq min 0)
				 (> week 0)
				 (> day 0))
		       (format NIL "~:[~D~;~R~] minute~:p" (<= min 12) min)))
	    (fmt-hour (unless (or (eq hour 0)
				  (> day 0))
			(format NIL "~:[~D~;~R~] hour~:p" (<= hour 12) hour)))
	    (fmt-day (unless (eq day 0)
		       (format NIL "~:[~D~;~R~] day~:p" (<= day 12) day)))
	    (fmt-week (unless (eq week 0)
			(format NIL "~R week~:p" week)))
	    (result NIL))
	(dolist (item (list fmt-sec fmt-min fmt-hour fmt-day fmt-week))
	  (when item (setq result (conc result (unless (eq (length result) 0) ", ") item))))
	(conc action " " result " ago")))))

(defun format-last-changed (timestamp &optional (static *website-static*))
  (let ((unix (timestamp-to-unix timestamp)))
    (aif (and (not static)
	      (format-timediff "last changed" (now) unix))
	 it
	 (conc "last changed on " (format-timestring NIL timestamp :format *website-time-format*)))))

(defun format-created (timestamp &optional (static *website-static*))
  (aif (and (not static)
	    (format-timediff "created" (now) timestamp))
       it
       (conc "created on " (format-timestring NIL timestamp :format *website-time-format*))))

(defun name-from-pathname (pathname)
  (let ((name (pathname-name pathname)))
    (aif (position #\- name)
	 (substitute #\Space #\- (subseq name (1+ it)))
	 name)))

(defun meta-data (pathname)
  (let* ((meta (embedded-meta-data pathname))
	 (stat (sb-posix:stat pathname))
	 (passwd (sb-posix:getpwuid (sb-posix:stat-uid stat))))
    (unless (assoc 'authors meta)
      (setq meta (cons `(authors ,(sb-posix:passwd-gecos passwd)) meta)))
    (unless (assoc 'last-changed meta)
      (setq meta (cons `(last-changed ,(unix-to-timestamp (sb-posix:stat-mtime stat))) meta)))
    (unless (assoc 'title meta)
      (setq meta (cons `(title ,(name-from-pathname pathname)) meta)))
    (awhen (assoc 'created meta)
      (rplacd it (list (unix-to-timestamp (cadr it)))))
    meta))

(defun meta-title (meta)
  (cadr (assoc 'title meta)))

(defun meta-authors (meta)
  (cadr (assoc 'authors meta)))

(defun meta-last-changed (meta)
  (cadr (assoc 'last-changed meta)))

(defun meta-created (meta)
  (cadr (assoc 'created meta)))

(defun meta-lang (meta)
  (cadr (assoc 'lang meta)))

(defun blag-tags (tags)
  (with-html
    (:ul :class "tags"
	 (dolist (tag (mapcar #'princ-to-string-downcase tags))
	   (htm (:li (link (puri:merge-uris (conc "#" tag) *tags-page*) tag)))))))

(defun blag-authors (authors)
  (with-html
    (:span :class "authors"
	   "by " (etypecase authors
		   (string (str authors))))))

(defun blag-time (meta)
  (let ((last-changed (meta-last-changed meta)))
    (awhen (meta-created meta)
      (format-created it))
    ;; actually, this kinda sucks, just keep it simple
    ;; (aif (meta-created meta)
    ;; 	 (if (timestamp> last-changed it)
    ;; 	     (format-last-changed last-changed)
    ;; 	     (format-created it))
    ;; 	 (format-last-changed last-changed))
    ))

(defun blag-meta (meta)
  (if (blag-time meta)
      (with-html
	(:div :class "meta"
	      (awhen (blag-time meta)
		(htm (:span :class "time" (str it))))
	      ;;" "
	      ;; (blag-authors (meta-authors meta))
	      ))))

(defun render-markdown (pathname)
  (markdown:markdown pathname
		     :stream *web-stream*
		     :properties '((:search-locations "../"))
		     :render-extensions '(include-plain)))

(let ((regex (ppcre:create-scanner "[^a-zA-Z0-9\-_.:]")))
  (defun blag-id (pathname)
    (declare (type (or string pathname) pathname))
    (when (pathnamep pathname)
      (setq pathname (pathname-name pathname)))
    (setf pathname (regex-replace-all regex pathname "-"))
    (if (member (aref pathname 0) '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	(conc "post-" pathname)
	pathname)
    ;;(substitute #\- #\Space pathname :test #'eq)
    ;; (ironclad:byte-array-to-hex-string 
    ;;    (ironclad:digest-sequence
    ;;     :md5 (ironclad:ascii-string-to-byte-array (if (pathnamep pathname)
    ;; 						  (princ-to-string pathname)
    ;; 						  pathname))))
    ;;pathname
    ))

(defun blag-post (page pathname)
  ;; TODO: add more information using stat
  ;; TODO: enhance this with git
  ;; sb-posix:stat
  (declare (special *tags*))
  (let* ((meta (meta-data pathname))
	 (title (meta-title meta))
	 (id (blag-id title))
	 (page-ref (conc (princ-to-string page) "#" id)))
    (declare (special *timeline-stream*))
    (with-html-output (stream *timeline-stream*)
      (:li (:a :href page-ref :title title (str title))))
    (with-html
      (:div :class "post" :id id :xml\:lang (awhen (meta-lang meta)
					      (princ-to-string-downcase it))
	    (when title (htm (:h3 :class "title" (str title))))
	    (blag-meta meta)
	    (render-markdown pathname)
	    (awhen (assoc 'tags meta)
	      (dolist (tag (cdr it))
		(push (cons page-ref title)
		      (gethash tag *tags*)))
	      (blag-tags (cdr it)))))))

(defun blag-navigation (index n)
  (with-html
    (:ul :class "blag-navigation"
	 (unless (eq index 0)
	   (htm (:li (link (conc "blag-" (princ-to-string (1- index)) ".xhtml")
			   "prev" :title "Previous Page" :rel "prev"))))
	 (loop
	    ;;until (eq n 1)
	    for i from 0 to (1- n)
	    do (htm (:li (if (eq i index)
			     (str (princ-to-string (1+ i)))
			     (link (conc "blag-" (princ-to-string i) ".xhtml")
				   (princ-to-string (1+ i))
				   :title (format NIL "~:(~:R~) Page" (1+ i)))))))
	 (unless (eq index (1- n))
	   (htm (:li (link (conc "blag-" (princ-to-string (1+ index)) ".xhtml")
			   "next" :title "Next Page" :rel "next")))))))

(defun generate-blag-page (pathname posts index n)
  (with-open-file (*web-stream* pathname
				:if-exists :supersede :if-does-not-exist :create
				:direction :output)
    (with-standard-template (:title (if (> index 0)
					(conc "Blag - Page " (princ-to-string index))
					"Blag"))
      (blag-navigation index n)
      (dolist (post posts)
	(blag-post pathname post))
      (blag-navigation index n))))

(defun generate-blag-tags-page ()
  (declare (special *tags*))
  (with-open-file (*web-stream* *tags-page*
				:if-exists :supersede
				:if-does-not-exist :create
				:direction :output)
    (with-standard-template (:title "Blag - Tags")
      (:dl
       (loop
	  for tag being each hash-key of *tags* using (hash-value values)
	  do
	    (let ((tag (princ-to-string-downcase tag)))
	      (htm (:dt :id tag (str tag))
		   (:dd (:ul
			 (dolist (value values)
			   (htm (:li (link (car value) (cdr value))))))))))))))

(defun generate-blag-pages (pathname posts-directory &optional (posts-per-page 10))
  (let* ((wildcard (merge-pathnames (make-pathname :name :wild :type "md")
				    posts-directory))
	 (posts (reverse (directory wildcard)))
	 (n (ceiling (/ (list-length posts) posts-per-page)))
	 (*tags* (make-hash-table)))
    (declare (special *tags*))
    (with-open-file (*timeline-stream* *timeline-page*
				       :if-exists :supersede
				       :if-does-not-exist :create
				       :direction :output)
      (declare (special *timeline-stream*))
      (let ((*web-stream* *timeline-stream*))
	(with-standard-template (:title "Blag - Timeline")
	  (do* ((index 0 (1+ index))
		(sub-posts (subseq posts 0 (min (list-length posts) posts-per-page))
			   (subseq rest-posts 0 (min (list-length rest-posts) posts-per-page)))
		(rest-posts (when (>= (list-length posts) posts-per-page)
			      (subseq posts posts-per-page))
			    (when (>= (list-length rest-posts) posts-per-page)
			      (subseq rest-posts posts-per-page)))
		(single-pathname (merge-pathnames (make-pathname :name (conc (pathname-name pathname) "-" (princ-to-string index)))
						  pathname)
				 (merge-pathnames (make-pathname :name (conc (pathname-name pathname) "-" (princ-to-string index)))
						  pathname)))
	       ((null sub-posts))
	    (generate-blag-page single-pathname sub-posts index n)))))
    (generate-blag-tags-page)))

(defun generate-static-page (pathname)
  (let ((meta (meta-data pathname)))
    (with-open-file (*web-stream* (merge-pathnames (make-pathname :type "xhtml") pathname)
				  :if-exists :supersede
				  :if-does-not-exist :create
				  :direction :output)
      (with-standard-template (:title (meta-title meta) :lang (meta-lang meta))
	(cl-markdown:markdown pathname
			      :stream *web-stream*
			      :properties '((:search-locations "../"))
			      :render-extensions '(include-plain))))))

;; (defun generate-static-page (pathname)
;;   (with-open-file (page-stream pathname)
;;     (let ((char (read-char page-stream))
;; 	  (alist)
;; 	  (seq))
;;       (unread-char char page-stream)
;;       (when (eql char #\()
;; 	(setf alist (read page-stream)))
;;       (setq seq (make-string (file-length page-stream)))
;;       (read-sequence seq page-stream)
;;       )))

(defun generate-google-sitemap (&optional (baseurl "http://www.students.uni-luebeck.de/~frahmo/"))
  (labels ((loc (location)
	     (with-html (:loc (str (conc baseurl location))))))
    (with-open-file (*web-stream* "sitemap.xml"
				  :if-exists :supersede
				  :if-does-not-exist :create
				  :direction :output)
      (with-html-output (*web-stream* *web-stream* :prologue +decl-xml-10+)
	(:urlset :xmlns "http://www.sitemaps.org/schemas/sitemap/0.9"
		 :xmlns\:xsi "http://www.w3.org/2001/XMLSchema-instance"
		 :xsi\:schemeLocation "http://www.sitemaps.org/schemas/sitemap/0.9
http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd"
		 (:url (loc "blag-0.xhtml")
		       (:lastmod "2009-02-28")
		       (:changefreq "weekly")
		       (:priority 1.0))
		 (:url (loc "blag-1.xhtml")
		       (:lastmod "2009-02-28")
		       (:changefreq "weekly")
		       (:priority 1.0))
		 (:url (loc "blag-2.xhtml")
		       (:lastmod "2009-02-28")
		       (:changefreq "weekly")
		       (:priority 1.0))
		 (:url (loc "tags.xhtml")
		       (:lastmod "2009-02-28")
		       (:changefreq "weekly")
		       (:priority 0.5))
		 (:url (loc "timeline.xhtml")
		       (:lastmod "2009-02-28")
		       (:changefreq "weekly")
		       (:priority 0.5))
		 (:url (loc "university.xhtml")
		       (:lastmod "2008-10-01")
		       (:changefreq "never")
		       (:priority 0.2))
		 (:url (loc "contact.xhtml")
		       (:lastmod "2008-10-01")
		       (:changefreq "never")
		       (:priority 0.2))
		 (:url (loc "programming.xhtml")
		       (:lastmod "2008-10-01")
		       (:changefreq "never")
		       (:priority 0.1)))))))

(defun generate-static-pages ()
  (dolist (file (directory "*.md"))
    (generate-static-page file)))

;;; TODO: clear up timeline using a separate loop for months and years

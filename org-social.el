;; org-social-timeline.el --- Generate timeline from Org-social feeds

(require 'url)
(require 'org)
(require 'cl-lib)

(defgroup org-social nil
	"Org-social timeline generator settings."
	:group 'org
	:prefix "org-social-")

(defcustom org-social-current-file "social.org"
	"Path to your current org-social file."
	:type 'file
	:group 'org-social)

(defvar org-social-buffer-name "*Org-Social Timeline*"
	"Name of the buffer to display the timeline.")

(cl-defstruct org-social-post
	id
	content
	author
	feed-url
	timestamp
	properties)

(defun org-social--parse-datetime (datetime-str)
	"Parse ISO 8601 datetime string and return time value."
	(when (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\([+-][0-9]\\{2\\}:[0-9]\\{2\\}\\)" datetime-str)
		(let ((year (string-to-number (match-string 1 datetime-str)))
			  (month (string-to-number (match-string 2 datetime-str)))
			  (day (string-to-number (match-string 3 datetime-str)))
			  (hour (string-to-number (match-string 4 datetime-str)))
			  (minute (string-to-number (match-string 5 datetime-str)))
			  (second (string-to-number (match-string 6 datetime-str))))
			(encode-time second minute hour day month year))))

(defun org-social--extract-follows ()
	"Extract FOLLOW entries from current org-social file."
	(let ((follows '()))
		(with-temp-buffer
			(insert-file-contents org-social-current-file)
			(goto-char (point-min))
			(while (re-search-forward "^#\\+FOLLOW:[ \t]+\\([^ \t]+\\)[ \t]+\\(.+\\)$" nil t)
				(let ((author (match-string-no-properties 1))
					  (url (match-string-no-properties 2)))
					(push (cons author url) follows))))
		(nreverse follows)))

(defun org-social--download-feed (url)
	"Download content from URL synchronously."
	(with-temp-buffer
		(condition-case err
			(progn
				(url-insert-file-contents url)
				(buffer-string))
			(error
				(message "Error downloading %s: %s" url (error-message-string err))
				nil))))

(defun org-social--parse-feed (content author feed-url)
	"Parse org-social feed CONTENT and return list of posts."
	(let ((posts '()))
		(with-temp-buffer
			(insert content)
			(goto-char (point-min))

			;; Find the Posts section
			(when (re-search-forward "^\\* Posts" nil t)
				(let ((posts-start (point)))
					(goto-char posts-start)

					;; Find each post (headlines starting with **)
					(while (re-search-forward "^\\*\\*" nil t)
						(let ((post-start (point))
							  (properties '())
							  post-id
							  post-content)

							;; Look for properties drawer
							(when (re-search-forward ":PROPERTIES:" nil t)
								(let ((prop-start (point)))
									(when (re-search-forward ":END:" nil t)
										(let ((prop-end (match-beginning 0)))
											(save-excursion
												(goto-char prop-start)
												(while (re-search-forward "^:ID:[ \t]+\\(.+\\)$" prop-end t)
													(setq post-id (match-string-no-properties 1)))
												(goto-char prop-start)
												(while (re-search-forward "^:\\([^:]+\\):[ \t]+\\(.+\\)$" prop-end t)
													(let ((key (match-string-no-properties 1))
														  (value (match-string-no-properties 2)))
														(push (cons key value) properties))))

											;; Get post content (everything after :END: until next post or end)
											(goto-char (1+ prop-end))
											(skip-chars-forward " \t\n")
											(let ((content-start (point)))
												(if (re-search-forward "^\\*\\*" nil t)
													(goto-char (match-beginning 0))
													(goto-char (point-max)))
												(setq post-content (string-trim (buffer-substring-no-properties content-start (point))))))))

							;; Create post if we have required data
							(when (and post-id post-content)
								(push (make-org-social-post
									   :id post-id
									   :content post-content
									   :author author
									   :feed-url feed-url
									   :timestamp (org-social--parse-datetime post-id)
									   :properties properties)
									  posts)))))))
		(nreverse posts)))

(defun org-social--format-post (post)
	"Format a POST for display in the timeline."
	(let ((timestamp (org-social-post-timestamp post))
		  (author (org-social-post-author post))
		  (content (org-social-post-content post))
		  (feed-url (org-social-post-feed-url post))
		  (properties (org-social-post-properties post)))
		(concat
		 "** " (if timestamp (format-time-string "%Y-%m-%d %H:%M" timestamp) "Unknown date")
		 " - " author "\n"
		 ":PROPERTIES:\n"
		 ":ID: " (org-social-post-id post) "\n"
		 ":AUTHOR: " author "\n"
		 ":FEED_URL: " feed-url "\n"
		 (mapconcat (lambda (prop) (format ":%s: %s\n" (upcase (car prop)) (cdr prop))) properties "")
		 ":END:\n\n"
		 content "\n\n")))

(defun org-social-generate-timeline ()
	"Generate timeline from all followed feeds."
	(interactive)
	(unless (file-exists-p org-social-current-file)
		(error "Org-social file not found: %s" org-social-current-file))

	(let ((follows (org-social--extract-follows))
		  (all-posts '()))

		(unless follows
			(error "No FOLLOW entries found in %s" org-social-current-file))

		(message "Loading feeds from %s..." (file-name-nondirectory org-social-current-file))

		;; Download and parse each feed
		(dolist (follow follows)
			(let ((author (car follow))
				  (url (cdr follow)))
				(message "Downloading feed for %s..." author)
				(let ((content (org-social--download-feed url)))
					(when content
						(let ((posts (org-social--parse-feed content author url)))
							(setq all-posts (append all-posts posts))
							(message "Found %d posts from %s" (length posts) author))))))

		;; Sort posts by timestamp (newest first)
		(setq all-posts (sort all-posts
							  (lambda (a b)
								(let ((time-a (org-social-post-timestamp a))
									  (time-b (org-social-post-timestamp b)))
									(cond
									 ((and time-a time-b) (time-less-p time-b time-a))
									 (time-a t)
									 (time-b nil)
									 (t nil))))))

		;; Create timeline buffer
		(with-current-buffer (get-buffer-create org-social-buffer-name)
			(erase-buffer)
			(insert "#+TITLE: Org-Social Timeline\n")
			(insert "#+AUTHOR: Timeline Generator\n")
			(insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
			(insert (format "Timeline generated from %d feeds with %d total posts.\n\n"
						   (length follows) (length all-posts)))

			(insert "* Timeline\n\n")

			;; Insert all posts
			(dolist (post all-posts)
				(insert (org-social--format-post post)))

			(org-mode)
			(goto-char (point-min)))

		;; Display the buffer
		(pop-to-buffer org-social-buffer-name)
		(message "Timeline generated with %d posts from %d feeds" (length all-posts) (length follows))))

(defun org-social-refresh-timeline ()
	"Refresh the current timeline."
	(interactive)
	(org-social-generate-timeline))

;; Key bindings (optional)
(define-key org-mode-map (kbd "C-c s t") 'org-social-generate-timeline)
(define-key org-mode-map (kbd "C-c s r") 'org-social-refresh-timeline)

(provide 'org-social-timeline)

;;; org-social-timeline.el ends here

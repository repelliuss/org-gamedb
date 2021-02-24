;;; org-gamedb.el --- A Giant Bomb API client to work with Emacs org-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 repelliuss
;;
;; Author: repelliuss <https://github.com/repelliuss>
;; Maintainer: repelliuss <repelliuss@gmail.com>
;; Created: February 17, 2021
;; Modified: February 17, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/repelliuss/org-gamedb
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  A Giant Bomb API client to work with Emacs org-mode
;;
;;; Code:

;; TODO: If not at org header, open org buffer
(require 'dash)
(require 'json)
(require 'url)
(require 'org)
(require 'org-element)

(defgroup org-gamedb nil
  "A Giant Bomb API client to work with Emacs org-mode."
  :group 'org
  :prefix "org-gamedb-"
  :package-version '(Org-Gamedb . "0.0.1"))

;; This API Key is used for testing purposes
(defcustom org-gamedb-api-key "6023fb372e7fcafed56149e47c24226e8dac4234"
  "Giant Bomb API key.
Set this to your API key you got from Giant Bomb. You can get an API key at
URL `https://www.giantbomb.com/api/' . See 'Terms of Use'."
  :link '(url-link :tag "Get an API key" "https://www.giantbomb.com/api/")
  :type 'string)

(defcustom org-gamedb-candidate-sort '((original_release_date . desc)
                                       (name . asc))
  "How results are sorted by for all queries.
If there are more than one result for a query, then candidates will be
sorted by the first field according to its value iff the queried resource has
that field and supports sorting for it.

Available values are in this form: '(field . order)' where field is one of the
fields for resources and order is either 'asc' or 'desc'.
See URL `https://www.giantbomb.com/api/documentation/' for available fields.
It is recommended to pick a common field like 'name' though it doesn't have to
be included in all resources."
  :type '(repeat :tag "Alist" (cons :tag "Field" symbol
                                    (choice (const 'asc)
                                            (const 'desc)))))

(defcustom org-gamedb-field-query-list '(name original_release_date)
  "Fetched fields for all queries.
If there is more than one resource for a query, then user is prompted to select
a resource from candidates. Candidates will have these fields as information
seperated by variable `org-gamedb-field-seperator'.

Available fields are whose value is a string.
For available fields, see URL `https://www.giantbomb.com/api/documentation/'."
  :type '(repeat :tag "Field" symbol))

(defcustom org-gamedb-field-seperator " - "
  "Seperator for each field of a candidate."
  :type 'string)

(defcustom org-gamedb-filter-field "name"
  "Field to filter query as a key.
Search will be filtered by this field as a key and input as a value that is
given by user. Currently only 'single filter' supported.

Available values are fields.
See URL `https://www.giantbomb.com/api/documentation/' for available fields.
It is recommended to pick a common field like 'name' though it doesn't
have to be included in all resources."
  :type 'string)

(defcustom org-gamedb-include-image t
  "Inserts an image if t wherever possible."
  :type 'boolean)

(defcustom org-gamedb-use-org-header t
  "Try to query org header if non-nil.
Otherwise always prompt the query."
  :type 'boolean)

(defcustom org-gamedb-correct-header t
  "If t, then update the heading with the queried resource's name."
  :type 'boolean)

(defcustom org-gamedb-field-property-list
  '(original_release_date developers publishers genres themes
                          developed_games)
  "Fields that will be inserted as properties to org-header for a query.
These fields will be fetched and inserted to the property drawer of org header
named with value of 'name' field of result if there is one.

Available values are fields.
See URL `https://www.giantbomb.com/api/documentation/' for available fields.
It is recommended to pick common fields though it doesn't have to be included
in all resources."
  :type '(repeat :tag "Field" symbol))

(defcustom org-gamedb-image-type 'medium
  "Type of image inserted after a query.
If `org-gamedb-include-image' is t then inserted image will be this type
Available values are icon, medium, screen, screen_large, small, super,
thumb, tiny, original."
  :type '(choice (const icon)
                 (const medium)
                 (const screen)
                 (const screen_large)
                 (const small)
                 (const super)
                 (const thumb)
                 (const tiny)
                 (const original)))

(defcustom org-gamedb-display-image-after t
  "Display inserted image after a query if t."
  :type 'boolean)

(defcustom org-gamedb-descriptor-type 'deck
  "How detailed will be descriptor.
`deck' for short, `description' for long descriptors."
  :type '(choice (const :tag "Short" deck)
                 (const :tag "Long" description)))

(defcustom org-gamedb-include-descriptor t
  "Include a descriptor for queried resource with `org-gamedb-descriptor-type'."
  :type 'boolean)

(defcustom org-gamedb-cache-dir-generator #'org-gamedb--get-cache-dir
  "Function that will return path to cache directory.
Function takes no args."
  :type 'function)

(defcustom org-gamedb-store-images-explicitly t
  "Store images at `org-gamedb-cache-dir-generator'."
  :type 'boolean)

(defconst org-gamedb--api-url "https://www.giantbomb.com/api/"
  "Base URL of API.")

(defconst org-gamedb--request-format "json"
  "Response format of API.")

(defconst org-gamedb--resource-list
  '(accessories characters companies concepts dlcs games
                game_ratings locations objects people platforms promos
                rating_boards regions releases reviews themes user_reviews
                videos video_categories video_shows)
  "List of resources to query.")

(defun org-gamedb--get-cache-dir ()
  "Return cache directory for images."
  "~/.cache/org-gamedb/")

(defun org-gamedb--encode-field-list (fields)
  "Return a string of FIELDS seperated by a comma for request URL."
  (--reduce (format "%s,%s" acc it) fields))

(defun org-gamedb--require-guid-p (resource)
  "Return t if RESOURCE requires a guid, otherwise nil.
Giant Bomb API takes a guid most of the time if requested resource doesn't
end with 's."
  (not (or (eql (aref resource (- (length resource) 1)) ?s)
           (string= "people" resource))))

(defun org-gamedb--encode-candidate-sort ()
  "Return a sort value for a request to API."
  (let ((encoded ""))
    (dolist (element (reverse org-gamedb-candidate-sort) encoded)
      (setq encoded
            (concat encoded (format "%s:%s," (car element) (cdr element)))))))

(defun org-gamedb--encode-url (resource field-list &optional filter-val guid)
  "Return a request url to Giant Bomb.
RESOURCE is interested category about games.
See URL `https://www.giantbomb.com/api/documentation/' for available resources.

FIELD-LIST is list of fields to fetch.

FILTER-VAL is value taken from user to filter query with
`org-gamedb-filter-field'.

A GUID is required if given resource is for search purposes, decided by
`org-gamedb--require-guid-p'."
  (if (org-gamedb--require-guid-p resource)
      (format "%s%s/%s/?api_key=%s&format=%s&sort=%s&field_list=%s"
              org-gamedb--api-url
              resource
              guid
              org-gamedb-api-key
              org-gamedb--request-format
              (org-gamedb--encode-candidate-sort)
              field-list)
    (format "%s%s/?api_key=%s&format=%s&sort=%s&field_list=%s&filter=%s:%s"
            org-gamedb--api-url
            resource
            org-gamedb-api-key
            org-gamedb--request-format
            (org-gamedb--encode-candidate-sort)
            field-list
            org-gamedb-filter-field
            filter-val)))

(defun org-gamedb--complement-resource (resource)
  "Take a plural RESOURCE and return its singular endpoint."
  (pcase resource
    ((pred (string-match "people"))
     "person")
    ((and (pred (string-match "\\(.+\\)ies\\'"))
          (app (match-string 1) singular-root)
          (let singular (concat singular-root "y")))
     singular)
    (plural (substring plural 0 (- (length plural) 1)))))

(defun org-gamedb--mk-results-collection (results)
  "Construct descriptors for each of RESULTS.
Return list of strings according to `org-gamedb-field-query-list'."
  (mapcar (lambda (a-result)
            (let ((info-str (cdr (assq (car org-gamedb-field-query-list)
                                       a-result))))
              (dolist (field (cdr org-gamedb-field-query-list) info-str)
                (setq info-str (concat info-str
                                       org-gamedb-field-seperator
                                       (cdr (assq field a-result)))))))
          results))

(defun org-gamedb--get-guid (choice results)
  "Take user input CHOICE and all RESULTS and return its guid."
  (let ((choice-values (split-string choice org-gamedb-field-seperator)))
    (cdr
     (assq 'guid
           (seq-find (lambda (a-result)
                       (let ((field (car org-gamedb-field-query-list))
                             (rest-fields (cdr org-gamedb-field-query-list))
                             (value (car choice-values))
                             (rest-values (cdr choice-values)))
                         (while (and rest-fields
                                     (string= value (cdr (assq field a-result))))
                           (setq field (car rest-fields)
                                 rest-fields (cdr rest-fields)
                                 value (car rest-values)
                                 rest-values (cdr rest-values)))
                         (and (null rest-fields)
                              (let ((v (cdr (assq field a-result))))
                                (or (if (null v) (string= value ""))
                                    (string= value v))))))
                     results)))))

(defun org-gamedb--prompt-results (results)
  "Prompt RESULTS to user and return its guid.
Each resource appears according to `org-gamedb-field-query-list' and
`org-gamedb-field-seperator'."
  (let ((this-command 'org-gamedb--prompt-results)) ; fixes counsel-M-x-transformer problem
    (org-gamedb--get-guid
     (completing-read "I meant: "
                      (org-gamedb--mk-results-collection results)
                      nil t)
     results)))

(defun org-gamedb--on-success-query (data resource)
  "Handle success DATA taken from RESOURCE endpoint.
If there is no result notify it.
If there is one result then gets its GUID and make a second request to get
values for `org-gamedb-field-property-list'.
If there is more than one result then prompt user to select one with each
resource in the form according to `org-gamedb-field-query-list'. Then make
a second request with selected resource's guid."
  (let ((results-count (cdr (assq 'number_of_total_results data)))
        (results (cdr (assq 'results data))))
    (cond
     ((< results-count 1) (message "No resource found."))
     ((= results-count 1) (org-gamedb--mk-request
                           (org-gamedb--complement-resource resource)
                           'get
                           nil
                           (cdr (assq 'guid (aref results 0)))))
     (t (org-gamedb--mk-request (org-gamedb--complement-resource resource)
                                'get
                                nil
                                (org-gamedb--prompt-results results))))))

;; FIXME: when there is property
(defun org-gamedb--insert-image (url name)
  "Insert image of queried resource from URL with its NAME as description." ; TODO: improve doc
  (let ((beg (point)))
    (if (not org-gamedb-store-images-explicitly)
        (insert (format "\n[[%s][Poster]]\n\n" url))
      (let* ((dir (funcall org-gamedb-cache-dir-generator))
             (file-path (concat dir
                                name
                                (url-file-extension url))))
        (make-directory dir t)
        (url-copy-file url file-path t)
        (insert (format "\n[[file:%s][Poster]]\n\n" file-path))))
    (if org-gamedb-display-image-after
        (org-display-inline-images t t beg (point)))))

(defun org-gamedb--add-descriptor (descriptor)
  "Add DESCRIPTOR to the end of heading."
  (insert (format "%s\n" descriptor)))

(defun org-gamedb--on-success-get (data _)
  (let ((results (cdr (assq 'results data))))
    (when org-gamedb-field-property-list
      (dolist (field org-gamedb-field-property-list)
        (let ((value (cdr (assq field results))))
          (cond
           ((or (stringp value)
                (integerp value))
            (org-entry-put nil (format "%s" field) value))
           ((vectorp value)
            (org-entry-put
             nil
             (format "%s" field)
             (seq-mapcat (lambda (a-value-assoc)
                           (format "%s " (cdr (assq 'name a-value-assoc))))
                         value
                         'string))))))
      (org-back-to-heading-or-point-min)
      (forward-line)
      (org-cycle))
    (let ((resource-name (cdr (assq 'name results))))
      (if org-gamedb-correct-header
          (org-edit-headline resource-name))
      (re-search-forward org-property-end-re)
      (if org-gamedb-include-image
          (org-gamedb--insert-image
           (cdr (assq (intern (format "%s_url" org-gamedb-image-type))
                      (cdr (assq 'image results))))
           resource-name))
      (if org-gamedb-include-descriptor
          (org-gamedb--add-descriptor
           (cdr (assq org-gamedb-descriptor-type results))))
      (goto-char (point-max)))))

(defun org-gamedb--handle-request (status callback resource excursion)
  "Handle request errors and let control to CALLBACK on success.
STATUS is response to a request returned by `url-retrieve' functions.

CALLBACK is a function with 2 args to call on success with a data and RESOURCE
endpoint to the request."
  (cond
   ((plist-member status :error)
    (error (buffer-substring (search-forward " " nil nil 2) (point-at-eol))))
   ((not (search-forward "\n\n" nil t))
    (error "Missing headers, bad response!"))
   (t (let ((data (json-read)))
        (if (= (cdr (assq 'status_code data)) 1)
            (with-current-buffer (car excursion) ; restore user's buffer
              (save-excursion
                (goto-char (cdr excursion))         ; goto point where query called
                (funcall callback data resource)))
          (error (assq 'error data))))))) ; TODO: check error in json obj

(defun org-gamedb--mk-request (resource type &optional query guid)
  "Make a request to RESOURCE endpoint.
If TYPE is symbol `get' then a GUID is required. This request will try to get
a single item asynchronously and will make a call to
`org-gamedb--on-success-get'.

Otherwise a QUERY required. This request will try to get all results with
`org-gamedb-filter-field' and QUERY as its value to do filtering asyncronously
and will make a call to `org-gamedb--on-success-query'."
  (let ((url-request-method "GET")
        (field-list)
        (cbargs `(,resource (,(current-buffer) . ,(point)))))
    (if (eq type 'get)
        (progn
          (setq field-list (org-gamedb--encode-field-list
                            (append `(,org-gamedb-descriptor-type image name)
                                    org-gamedb-field-property-list)))
          (push #'org-gamedb--on-success-get cbargs))
      (setq field-list (org-gamedb--encode-field-list
                        (cons 'guid org-gamedb-field-query-list)))
      (push #'org-gamedb--on-success-query cbargs))
    (url-retrieve
     (org-gamedb--encode-url resource field-list query guid)
     #'org-gamedb--handle-request
     cbargs
     'silent
     'inhibit-cookies)))

(defun org-gamedb--get-query ()
  "Return input query for the next resource query."
  (if org-gamedb-use-org-header
      (let ((heading (org-entry-get nil "ITEM")))
        (if heading
            heading
          (read-string "Query: ")))
    (read-string "Query: ")))

;;;###autoload
(defun org-gamedb-query (resource query)
  "Make a QUERY to RESOURCE.
QUERY is a string and can be anything. Example queries are \"quantic\" for
companies and \"stardew\" for games.

RESOURCE is a resource defined by API. See available resources at
URL `https://www.giantbomb.com/api/documentation/'."
  (interactive
   (list (completing-read "Pick a resource: "
                          org-gamedb--resource-list
                          nil t)
         (org-gamedb--get-query)))
  (org-gamedb--mk-request resource 'query query))

;;;###autoload
(defun org-gamedb-games-query (query)
  "Make a QUERY to games resource.
QUERY is a string and can be anything. Example queries are \"quantic\" for
companies and \"stardew\" for games."
  (interactive
   (list (org-gamedb--get-query)))
  (org-gamedb--mk-request "games" 'query query))

(provide 'org-gamedb)
;;; org-gamedb.el ends here

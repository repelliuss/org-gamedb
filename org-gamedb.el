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

(require 'dash)
(require 'json)
(require 'url)
(require 'org)

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
                                    (choice (const asc)
                                            (const desc)))))

(defcustom org-gamedb-field-query-list '(name original_release_date)
  "Fetched fields for all queries.
If there is more than one resource for a query, then user is prompted to select
a resource from candidates. Candidates will have these fields as information
seperated by variable `org-gamedb-field-seperator'.

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

(defcustom org-gamedb-field-property-list
  '(deck original_release_date image)
  "Fields that will be inserted as properties to org-header for a query.
These fields will be fetched and inserted to the property drawer of org header
named with value of 'name' field of result if there is one.

Available values are fields.
See URL `https://www.giantbomb.com/api/documentation/' for available fields.
It is recommended to pick common fields though it doesn't have to be included
in all resources."
  :type '(repeat :tag "Field" symbol))

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

(defun org-gamedb--encode-field-list (fields &optional include-guid)
  "Return a string of FIELDS seperated by a comma for request URL.
Append guid field if INCLUDE-GUID is non-nil."
  (let ((encoded (--reduce (format "%s,%s" acc it) fields))) ; TODO: remove if no more dash
    (if include-guid
        (format "%s,%s"
                encoded
                ",guid")
      encoded)))

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
      (setf encoded (concat encoded (format "%s:%s," (car element) (cdr element)))))))

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
  (if (string= resource "people")
      "person"
    (substring resource 0 (- (length resource) 1))))

(defun org-gamedb--on-success-query (data resource)
  (with-output-to-temp-buffer "*rps*"
    (pp data)
    (let ((results-count (cdr (assq 'number_of_total_results data)))
          (results (cdr (assq 'results data))))
      (cond
       ((< results-count 1) (message "No resource found."))
       ((= results-count 1) (org-gamedb--mk-request
                             (org-gamedb--complement-resource resource)
                             'get
                             nil
                             (cdr (assq 'guid (aref results 0)))))))))

(defun org-gamedb--on-success-get (data resource)
  (with-output-to-temp-buffer "*rps*"
    (pp data)))

(defun org-gamedb--handle-request (status callback resource)
  (cond
   ((plist-member status :error)
    (error (buffer-substring (search-forward " " nil nil 2) (point-at-eol))))
   ((not (search-forward "\n\n" nil t))
    (error "Missing headers, bad response!"))
   (t (let ((data (json-read)))
        (kill-buffer (current-buffer))
        (if (= (cdr (assq 'status_code data)) 1)
            (funcall callback data resource)
          (error (assq 'error data))))))) ; TODO: check error in json obj

;; TODO: make only one request at a time
(defun org-gamedb--mk-request (resource type &optional query guid)
  (let ((url-request-method "GET")
        (field-list)
        (cbargs))
    (if (eq type 'get)
        (setq field-list (org-gamedb--encode-field-list
                          org-gamedb-field-property-list)
              cbargs (list #'org-gamedb--on-success-get resource))
      (setq field-list (org-gamedb--encode-field-list
                        org-gamedb-field-query-list t)
            cbargs (list #'org-gamedb--on-success-query resource)))
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

(defun org-gamedb-games-query (query)
  "Make a QUERY to games resource.
QUERY is a string and can be anything. Example queries are \"quantic\" for
companies and \"stardew\" for games."
  (interactive
   (list (org-gamedb--get-query)))
  (org-gamedb--mk-request "games" 'query query))

(provide 'org-gamedb)
;;; org-gamedb.el ends here

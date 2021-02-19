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

(defcustom org-gamedb-candidate-sort "name:asc"
  "How results are sorted by for all queries.
If there is more than one resource result for a query, then candidates will be
sorted by a field according to this variable iff queried resource has the field
and supports sorting.

Available values are in this form: 'field:order' where order is either 'asc' or
'desc'. See URL `https://www.giantbomb.com/api/documentation/' for available
fields. It is recommended to pick a common field like 'name' though it doesn't
have to be included in all resources."
  :type 'string)

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

(defcustom org-gamedb-field-property-list
  '(description original_game_rating original_release_date)
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

(defun org-gamedb--encode-field-list (fields &optional include-guid)
  "Return a string of FIELDS seperated by a comma for request URL.
Append guid field if INCLUDE-GUID is non-nil."
  (let ((encoded (--reduce (format "%s,%s" acc it) fields)))
    (if include-guid
        (format "%s,%s"
                encoded
                ",guid")
      encoded)))

(defun org-gamedb--require-guid-p (resource)
  "Return t if RESOURCE requires a guid, otherwise nil.
Giant Bomb API takes a guid iff requested resource doesn't end with 's."
  (not (eql (aref resource (- (length resource) 1)) ?s)))

(defun org-gamedb--encode-url (resource filter-val &optional guid)
  "Return a request url to Giant Bomb.
RESOURCE is interested category about games.
See URL `https://www.giantbomb.com/api/documentation/' for available resources.

FILTER-VAL is value taken from user to filter query with
`org-gamedb-filter-field'.

A GUID is required if given resource is for search purposes, decided by
`org-gamedb--search-or-get-p'."
  (if (org-gamedb--search-or-get-p resource)
      (format "%s%s/%s/?api_key=%s&format=%s&sort=%s&field_list=%s&filter=%s:%s"
              org-gamedb--api-url
              resource
              guid
              org-gamedb-api-key
              org-gamedb--request-format
              org-gamedb-candidate-sort
              (org-gamedb--encode-field-list org-gamedb-field-query-list)
              org-gamedb-filter-field
              filter-val)
    (format "%s%s/?api_key=%s&format=%s&sort=%s&field_list=%s&filter=%s:%s"
            org-gamedb--api-url
            resource
            org-gamedb-api-key
            org-gamedb--request-format
            org-gamedb-candidate-sort
            (org-gamedb--encode-field-list org-gamedb-field-query-list)
            org-gamedb-filter-field
            filter-val)))
(provide 'org-gamedb)
;;; org-gamedb.el ends here

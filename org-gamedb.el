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

(defcustom org-gamedb-field-list '(name original_release_date)
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
(provide 'org-gamedb)
;;; org-gamedb.el ends here

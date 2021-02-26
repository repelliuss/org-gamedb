;;; org-gamedb.el --- A Game API client to work with org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021 repelliuss

;; Author: repelliuss <https://github.com/repelliuss>
;; Maintainer: repelliuss <repelliuss@gmail.com>
;; Created: February 17, 2021
;; Modified: February 17, 2021
;; Version: 0.0.1
;; Keywords: org, game, api
;; Homepage: https://github.com/repelliuss/org-gamedb

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;  A Game API client to work with Emacs org-mode. Current API is provided
;;  by Giant Bomb. See https://www.giantbomb.com/.

;;; Code:

;;; TODO: remove api key
(require 'json)
(require 'url)
(require 'org)
(require 'seq)

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

(defcustom org-gamedb-candidate-sorting '((original_release_date . desc)
                                          (name . asc))
  "How results are sorted by for all queries.
If there are more than one result for a query, then candidates will be
sorted by the first field according to its value iff the queried resource has
that field and supports sorting for it.

Available values are in this form: (FIELD . ORDER) where field is one of the
fields for resources and order is either `asc' or `desc'.
See URL `https://www.giantbomb.com/api/documentation/' for available fields.
It is recommended to pick a common field like 'name' though it doesn't have to
be included in all resources."
  :type '(repeat :tag "Alist" (cons :tag "Field" symbol
                                    (choice (const 'asc)
                                            (const 'desc)))))

(defcustom org-gamedb-query-fields '(name original_release_date)
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
  "If non-nil, insert an image wherever possible."
  :type 'boolean)

(defcustom org-gamedb-use-org-headline t
  "If non-nil, try to query with org headline.
Otherwise always prompt the query and insert a new headline for query."
  :type 'boolean)

(defcustom org-gamedb-correct-headline t
  "If non-nil, update the headline with the queried resource's name."
  :type 'boolean)

(defcustom org-gamedb-property-fields
  '((original_release_date :tag "Release")
    (developers)
    (publishers)
    (genres)
    (birth_date)
    (hometown)
    (install_base)
    (gender)
    (themes)
    (platform)
    (date_founded :transform (lambda (v) (substring v 0 10)))
    (location_country :tag "Country")
    (location_city :tag "City")
    (birthday))
  "Fields that will be inserted as properties to a org headline after a query.
If there is one, these fields will be fetched and inserted to the property
drawer of org header named with value of 'name' field of result.

This is an association list where each association in the form of
\(FIELD . PLIST\) . For available fields,
see URL `https://www.giantbomb.com/api/documentation/'.
PLIST may be empty or it may have a `:tag' property that will be replaced
with name of the field and `:transform' property that will be applied to
each value of that field. This must be a function which takes 1 string
argument and return a string.

It is recommended to pick common fields though it doesn't have to be included
in all resources."
  :type '(alist :key-type (symbol :tag "Field")
                :value-type (choice (plist :key-type (choice (const :tag)
                                                             (const :transform))
                                           :value-type (choice (string :tag "Tag String")
                                                               (function :tag "Transform Function")))
                                    (const nil))))

(defcustom org-gamedb-plain-list-fields
  '((developed_games)
    (games))
  "Fields that will be inserted as plain lists to a org headline after a query.
See `org-gamedb-property-fields' for details. They are functionally same
except fields in this list will be inserted as plain list."
  :type '(alist :key-type (symbol :tag "Field")
                :value-type (choice (plist :key-type (choice (const :tag)
                                                             (const :transform))
                                           :value-type (choice (string :tag "Tag String")
                                                               (function :tag "Transform Function")))
                                    (const nil))))

(defcustom org-gamedb-image-type 'original
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
  "If non-nil, display inserted image after a query."
  :type 'boolean)

(defcustom org-gamedb-include-descriptor t
  "If non-nil, include a descriptor for queried resource."
  :type 'boolean)

(defcustom org-gamedb-cache-dir-generator #'org-gamedb--get-cache-dir
  "Function that will return a path to cache directory.
Function must take no args and return a string."
  :type 'function)

(defcustom org-gamedb-store-images-explicitly t
  "If non-nil, store images at `org-gamedb-cache-dir-generator'."
  :type 'boolean)

(defcustom org-gamedb-always-create-buffer nil
  "If non-nil, always create a new buffer for resource contents."
  :type 'boolean)

(defcustom org-gamedb-always-insert-heading nil
  "If non-nil, always insert a new heading for resource contents.
If user will make repetitive async queries in one go, then this is recommended
to be set t, preferably lexically in custom function. This is because if user
is in an org headline, then all query results will be inserted to that headline
asynchronously, possible loss of headlines for queries."
  :type 'boolean)

(defconst org-gamedb--api-url "https://www.giantbomb.com/api/"
  "Base URL of API.")

(defconst org-gamedb--request-format "json"
  "Response format of API.")

(defconst org-gamedb--resource-list
  '(accessories
    characters companies concepts dlcs games
    game_ratings locations objects people platforms promos
    rating_boards regions releases reviews themes user_reviews
    videos video_categories video_shows)
  "List of resources to query.")

(defun org-gamedb--get-cache-dir ()
  "Return default cache directory for images."
  "~/.cache/org-gamedb/")

(defun org-gamedb--encode-field-list (fields)
  "Return a string of FIELDS seperated by a comma for request URL."
  (seq-reduce (lambda (acc it)
                (format "%s,%s" acc it))
              (cdr fields) (car fields)))

(defun org-gamedb--require-guid-p (resource)
  "Return t if RESOURCE requires a guid, otherwise nil.
Giant Bomb API takes a guid most of the time if requested resource doesn't
end with 's."
  (not (or (eql (aref resource (- (length resource) 1)) ?s)
           (string= "people" resource))))

(defun org-gamedb--encode-candidate-sort ()
  "Return a proper sort value for request."
  (let ((encoded ""))
    (dolist (element (reverse org-gamedb-candidate-sorting) encoded)
      (setq encoded
            (concat encoded (format "%s:%s," (car element) (cdr element)))))))

(defun org-gamedb--encode-url (resource field-list &optional filter-val guid)
  "Return a request url to API.
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
Return list of strings according to `org-gamedb-query-fields'."
  (mapcar (lambda (a-result)
            (let ((info-str (cdr (assq (car org-gamedb-query-fields)
                                       a-result))))
              (dolist (field (cdr org-gamedb-query-fields) info-str)
                (setq info-str (concat info-str
                                       org-gamedb-field-seperator
                                       (let ((val (cdr (assq field a-result))))
                                         (if val val "N/A")))))))
          results))

(defun org-gamedb--matching-value-choice-p (value choice)
  "Return t if VALUE and CHOICE match in meaning.
If VALUE is nil, then CHOICE should be \"N/A\", otherwise
they should be string equal."
  (or (if (null value) (string= choice "N/A"))
      (string= value choice)))

(defun org-gamedb--get-guid (choice results)
  "Take user input CHOICE and all RESULTS and return its guid."
  (let ((choice-values (split-string choice org-gamedb-field-seperator)))
    (cdr
     (assq 'guid
           (seq-find (lambda (a-result)
                       (let ((field (car org-gamedb-query-fields))
                             (rest-fields (cdr org-gamedb-query-fields))
                             (ch-value (car choice-values))
                             (rest-ch-values (cdr choice-values)))
                         (while (and rest-fields
                                     (org-gamedb--matching-value-choice-p
                                      (cdr (assq field a-result)) ch-value))
                           (setq field (car rest-fields)
                                 rest-fields (cdr rest-fields)
                                 ch-value (car rest-ch-values)
                                 rest-ch-values (cdr rest-ch-values)))
                         (and (null rest-fields)
                              (org-gamedb--matching-value-choice-p
                               (cdr (assq field a-result)) ch-value))))
                     results)))))

(defun org-gamedb--prompt-results (results)
  "Prompt RESULTS to user and return its guid.
Each resource appears according to `org-gamedb-query-fields' and
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
values for `org-gamedb-property-fields'.
If there is more than one result then prompt user to select one with each
resource in the form according to `org-gamedb-query-fields'. Then make
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

(defun org-gamedb--insert-image (url name)
  "Insert image of queried resource from URL with its NAME as description."
  (let ((beg (point)))
    (if (not org-gamedb-store-images-explicitly)
        (insert (format "\n\n[[%s][Image]]\n" url))
      (let* ((dir (funcall org-gamedb-cache-dir-generator))
             (file-path (concat dir
                                name
                                (url-file-extension url))))
        (make-directory dir t)
        (url-copy-file url file-path t)
        (insert (format "\n\n[[file:%s][Poster]]\n" file-path))))
    (if org-gamedb-display-image-after
        (org-display-inline-images t t beg (point)))))

(defun org-gamedb--add-descriptor (descriptor)
  "Add DESCRIPTOR of resource to org headline."
  (insert (format "\n%s\n" descriptor)))

(defun org-gamedb--get-field-tag (field-assoc rep)
  "Return tag of FIELD-ASSOC if there is, otherwise prettified field name.
Prettied field name is field name with \"_\" replaced with REP and capitalized.
REP is \" \" in plain lists and \"-\" in property drawer."
  (let ((plist (cdr field-assoc)))
    (if plist
        (let ((tag (plist-get plist :tag)))
          (if tag tag (capitalize
                       (replace-regexp-in-string
                        "_" rep (symbol-name (car field-assoc))))))
      (capitalize
       (replace-regexp-in-string "_" rep (symbol-name (car field-assoc)))))))

(defun org-gamedb--get-field-transformed-value (field-assoc value)
  "Apply transformation in FIELD-ASSOC to VALUE.
Apply transformation in FIELD-ASSOC to VALUE and return it if there is one,
otherwise return VALUE itself."
  (let ((plist (cdr field-assoc)))
    (if plist
        (let ((transform (plist-get plist :transform)))
          (if transform
              (funcall transform value)
            value))
      value)))

(defun org-gamedb--add-property-values (results)
  "Add values from RESULTS for fields in `org-gamedb-property-fields'.
Create a property drawer and seperate each value with a comma then blank."
  (dolist (field-assoc org-gamedb-property-fields)
    (let ((value (cdr (assq (car field-assoc) results))))
      (cond
       ((or (stringp value)
            (integerp value))
        (org-entry-put
         nil
         (org-gamedb--get-field-tag field-assoc "-")
         (org-gamedb--get-field-transformed-value field-assoc value)))
       ((vectorp value)
        (org-entry-put
         nil
         (org-gamedb--get-field-tag field-assoc "-")
         (string-remove-suffix
          ", "
          (seq-mapcat (lambda (a-value-assoc)
                        (format "%s, " (org-gamedb--get-field-transformed-value
                                        field-assoc
                                        (cdr (assq 'name a-value-assoc)))))
                      value
                      'string))))))))

(defun org-gamedb--add-plain-list-values (results)
  "Add values from RESULTS for fields in `org-gamedb-plain-list-fields'.
Create a plain list for each field with a value or values in
`org-gamedb-plain-list-fields'. If value is an integer or a string then insert
it in descriptor form. If there are values then insert them as sub-lists."
  (insert "\n")
  (dolist (field-assoc org-gamedb-plain-list-fields)
    (insert (format "- %s" (org-gamedb--get-field-tag field-assoc " ")))
    (let ((value (cdr (assq (car field-assoc) results))))
      (cond
       ((or (stringp value)
            (integerp value))
        (insert
         (format " :: %s\n"
                 (org-gamedb--get-field-transformed-value field-assoc value))))
       ((vectorp value)
        (if (seq-empty-p value)
            (delete-region (point-at-bol) (point-at-eol))
          (org-insert-item)
          (org-indent-item)
          (goto-char (point-at-eol))
          (seq-do (lambda (a-value-assoc)
                    (insert (org-gamedb--get-field-transformed-value
                             field-assoc
                             (cdr (assq 'name a-value-assoc))))
                    (org-insert-item))
                  value)
          (kill-whole-line)))
       ((null value)
        (delete-region (point-at-bol) (point-at-eol))))))
  ;; When there is many async calls in one go,
  ;; `org-insert-heading-respect-content'
  ;; goes crazy with inserting blank lines. This just /tries/ to fix it.
  (if (= (point-max) (point))
      (forward-line -1))
  (unless (= (point-max) (point))
    (while (= (point-at-bol) (point-at-eol))
      (kill-whole-line)
      (forward-line -1))))

(defun org-gamedb--on-success-get (data _)
  "Parse DATA and insert values to a org headline.
This function is called after a successfull query and responsible function to
insert queried resource's values.

If cursor is not inside an org headline or `org-gamedb-always-create-buffer'
is non-nil, create a buffer named \"*Org GameDB*\" in `org-mode' and insert
values there appropriately.

If `org-gamedb-always-insert-heading' is non-nil, `org-gamedb-use-org-headline'
is nil or cursor is in \"*Org GameDB*\" buffer but not in a headline, then
insert a new headline with name of the resource and insert values there
appropriately.

If `org-gamedb-property-fields' is non-nil and there is a value for at least
one of them, insert each value in the property drawer.

If `org-gamedb-correct-headline' is non-nil, update heading with resource's
name.

If `org-gamedb-include-image' is non-nil, insert an image of resource if there
is.

If `org-gamedb-include-descriptor' is non-nil, insert a descriptor text of
resource if there is.

If `org-gamedb-plain-list-fields' is non-nil and there is a value for at least
one of them, insert each value in a plain list."
  (let* ((at-heading (org-entry-get nil "ITEM"))
         (results (cdr (assq 'results data)))
         (resource-name (cdr (assq 'name results))))
    (when (or (not at-heading)
              org-gamedb-always-create-buffer)
      (pop-to-buffer "*Org GameDB*" nil)
      (org-mode))
    (save-excursion
      (when (or (and (string= (buffer-name (current-buffer))  "*Org GameDB*")
                     (not at-heading))
                (not org-gamedb-use-org-headline)
                org-gamedb-always-insert-heading)
        (org-insert-heading-respect-content)
        (insert (format "%s\n" resource-name)))
      (org-back-to-heading-or-point-min)
      (when org-gamedb-property-fields
        (org-gamedb--add-property-values results)
        (forward-line)
        (org-cycle))
      (if org-gamedb-correct-headline
          (org-edit-headline resource-name))
      (if (org-at-property-drawer-p)
          (re-search-forward org-property-end-re)
        (goto-char (point-at-eol)))
      (if org-gamedb-include-image
          (org-gamedb--insert-image
           (cdr (assq (intern (format "%s_url" org-gamedb-image-type))
                      (cdr (assq 'image results))))
           resource-name))
      (if org-gamedb-include-descriptor
          (org-gamedb--add-descriptor
           (cdr (assq 'deck results))))
      (when org-gamedb-plain-list-fields
        (org-gamedb--add-plain-list-values results)))))

(defun org-gamedb--handle-request (status callback resource excursion)
  "Handle request errors and let control to CALLBACK on success.
STATUS is response to a request returned by `url-retrieve' functions.

CALLBACK must be a function with 2 args to call on success with a data
and RESOURCE endpoint to the request."
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
          (error (assq 'error data)))))))

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
                            (append '(deck image name)
                                    (mapcar #'car org-gamedb-property-fields)
                                    (mapcar #'car org-gamedb-plain-list-fields))))
          (push #'org-gamedb--on-success-get cbargs))
      (setq field-list (org-gamedb--encode-field-list
                        (cons 'guid org-gamedb-query-fields)))
      (push #'org-gamedb--on-success-query cbargs))
    (url-retrieve
     (org-gamedb--encode-url resource field-list query guid)
     #'org-gamedb--handle-request
     cbargs
     'silent
     'inhibit-cookies)))

(defun org-gamedb--get-query ()
  "Return input query for the next resource query."
  (if org-gamedb-use-org-headline
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
URL `https://www.giantbomb.com/api/documentation/'.

If you don't know what to query, just make an empty query!"
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
companies and \"stardew\" for games.

If you don't know what to query, just make an empty query!"
  (interactive
   (list (org-gamedb--get-query)))
  (org-gamedb--mk-request "games" 'query query))

;;;###autoload
(defun org-gamedb-get-api-key ()
  "Browse \"https://www.giantbomb.com/api/\" to get an api key.
You need to create a Giant Bomb account for it. Set your API key
to `org-gamedb-api-key' in your customization file.

Please respect \"Terms of Use\"."
  (interactive)
  (browse-url "https://www.giantbomb.com/api/")
  (message "Set your API key to `org-gamedb-api-key'."))

(provide 'org-gamedb)
;;; org-gamedb.el ends here

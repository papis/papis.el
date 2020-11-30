(require 'ol)
(require 'json)

(defcustom papis--temp-output
  :type 'string)

(defcustom papis--binary
  "papis"
  "The binary for papis"
  :type 'string)

(defcustom papis-ivy-format-function
  #'papis-default-ivy-format-function
  "Function taking a papis document (hashmap) and outputing a
   string representation of it to be fed into ivy.")

(org-link-set-parameters "papis"
                         :follow #'ol-papis-open
                         :export #'ol-papis-export
                         )

(org-link-set-parameters "papis+doi"
                         :follow #'ol-papis+doi-open
                         :export #'ol-papis+doi-export
                         :complete #'org-link-papis-store-doi
                         )

(defun org-papis-store-doi-link (query)
  (interactive "sPapis Query: ")
  (let ((doc (papis-ivy query)))
    (insert (format "[[papis+doi:%s][%s]]"
                    (gethash "doi" doc)
                    (gethash "title" doc)))))

(defun ol-papis-open (link)
  (let* ((doc (papis-ivy link)))
    (cond
     (doc (papis--open-doc doc))
     (t (error "No doc found"))
     )
    )
  )

(defun ol-papis+doi-open (doi)
  (papis-open (format "doi:%s" doi)))

(defun ol-papis+doi-export (doi description format)
  (cond
   ((eq format 'html) (format (concat "<a target='_blank'"
                                      " href='https://doi.org/%s'>"
                                      "%s"
                                      "</a>") doi description))
   ((eq format 'md) (format "[%s](https://doi.org/%s)" description doi))
   ((eq format 'org) (format "[[doi:%s][%s]]" doi description))
   (t description)))

(defun org-papis-store-url-link (query)
  (interactive "sPapis Query: ")
  (let ((doc (papis-ivy query)))
    (insert (format "[[%s][%s]]"
                    (gethash "url" doc)
                    (gethash "title" doc)))))

;; TODO: improve for many cites
(defun org-papis-cite (query)
  (interactive "sPapis Query: ")
  (let ((doc (papis-ivy query)))
    (insert (format "cite:%s"
                    (gethash "ref" doc)))))

(defun papis--doc-get-folder (doc)
  (gethash "_papis_local_folder" doc))

(defun papis-open (query)
  (interactive "sPapis Query: ")
  (papis--open-doc (papis-ivy query))
  )

(defun papis--get-file-paths (doc)
  (mapcar #'(lambda (f) (concat (papis--doc-get-folder doc)
                               "/" f))
          (gethash "files" doc)))

(defun papis--open-doc (doc)
  (split-window-horizontally)
  (find-file (ivy-read "file: " (papis--get-file-paths doc)))
  )

(defun org-papis-store-file-link (query)
  (interactive "sPapis Query: ")
  (let ((doc (papis-ivy query)))
    (insert (format "[[file:%s][%s]]"
                    (concat (gethash "_papis_local_folder" doc)
                            "/"
                            (ivy-read "file: " (gethash "files" doc)))
                    (gethash "title" doc)))))

(defun org-link-papis-store-doi (&optional arg)
  (format "papis+doi:%s" (gethash "doi" (papis-ivy ".*"))))

(defvar papis-ivy-format
  (concat "{doc[title]:50.50} ∷ "
          "{doc[author]:20.20} "
          "«{doc[year]}» "
          "+{doc[tags]} "
          "papis:doi:{doc[doi]}")
  "Format that gets output in the ivy selection list")

(defun ol-papis-export (link description format)
  (let ((doi (papis-get-doi description)))
    (cond
     ((eq format 'html) (format (concat "<a target='_blank'"
                                        " href='https://doi.org/%s'>"
                                        "%s"
                                        "</a>") doi description))
     ((eq format 'md) (format "[%s](https://doi.org/%s)" description doi))
     ((eq format 'org) (format "[[doi:%s][%s]]" doi description))
     (t description))))

(defun papis-get-doi (query)
  (interactive "sPapis Query: ")
  (let ((papis-command (concat "papis list --format "
                               "{doc[doi]}"
                               " --all "
                               "'" query "'")))
    (car (s-lines
          (shell-command-to-string
           papis-command)))))

(defun papis--get-libs (&optional library)
  (papis--cmd "list --libraries" library)
  )

(defun papis-edit (query)
  (interactive "sPapis Query: ")
  (let* ((doc (papis-ivy query))
         (folder (papis--doc-get-folder doc))
         (info (concat folder "/" "info.yaml")))
    (find-file info)
    ;; TODO: implement waiting after editing the file like
    ;; with a C-c C-c binding
    (papis--doc-update doc)
    )
  )

(defun papis--doc-update (doc)
  (let ((folder (papis--doc-get-folder doc)))
    (papis--cmd (concat "update --doc-folder " folder))
    )
  )

(defun papis--cmd (cmd &optional library)
  "Helping function to run papis commands"
  (let ((lib-flags (if library (concat "-l " library) "")
        ))
    (shell-command-to-string
     (format "%s %s %s" papis--binary lib-flags cmd))
    )
  )

(defun papis-json (query outfile)
  (shell-command (format "papis export --all --format json '%s' -o %s"
                         query
                         outfile)))

(defun papis-query (query)
  "Make a general papis query:
   it returns a list of hashtables where every hashtable is a papis document"
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (papis--temp-output (make-temp-file "papis-emacs-"))
         (exit-code (papis-json query papis--temp-output)))
    (if (not (eq exit-code 0))
        (error "Something happened running the papis command"))
    (json-read-file papis--temp-output)
    )
  )

(defun papis-default-ivy-format-function (doc)
  `(
    ,(format "%s\n\t%s\n\t«%s» +%s %s"
             (gethash "title" doc)
             (gethash "author" doc)
             (gethash "year" doc)
             (or (gethash "tags" doc) "")
             (let ((n (gethash "_note" doc))) (if n (concat ":note " n) "")))
    .
    ,doc
    )
  )

(defun papis-ivy (query)
  (interactive "sPapis Query: ")
  (let* ((results (papis-query query))
         (formatted-results (mapcar papis-ivy-format-function results))
         (ivy-add-newline-after-prompt t)
         )
    (cdr (assoc
          (ivy-read "Select an entry: " formatted-results)
          formatted-results
          )
         )
    )
  )

(provide 'papis)

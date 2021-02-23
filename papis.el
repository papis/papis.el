;; Generalities

;; - We interact with papis through the papis' json exporter.
;; - We use ~org-links~ to get information directly from papis.

;; The libraries that we will need are therefore:

;; [[file:README.org::*Generalities][Generalities:1]]
(require 'ol)
(require 'json)
;; Generalities:1 ends here

;; Variables


;; [[file:README.org::*Variables][Variables:1]]
(defcustom papis--temp-output-file
  nil
  "This variable holds the papis temporary output file where the json
  output is dumped"
  :type 'string)

(defcustom papis-binary-path
  "papis"
  "The binary path for papis.

   You might have papis installed for instance in some
   virtual environment"
  :type 'string)

(defcustom papis-ivy-format-function
  #'papis-default-ivy-format-function
  "Function taking a papis document (hashmap) and outputing a
   string representation of it to be fed into ivy.")
;; Variables:1 ends here

;; papis+doi

;; We define the link

;; [[file:README.org::*papis+doi][papis+doi:1]]
(org-link-set-parameters "papis+doi"
                         :follow #'ol-papis+doi-open
                         :export #'ol-papis+doi-export
                         :complete #'org-link-papis-store-doi
                         )
(defun ol-papis+doi-open (doi)
  "Open papis document by doi"
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
;; papis+doi:1 ends here

;; Queries


;; [[file:README.org::*Queries][Queries:1]]
(org-link-set-parameters "papis"
                         :follow #'ol-papis-open
                         :export #'ol-papis-export
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
     (t (error "No doc found")))))


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
  (papis--open-doc (papis-ivy query)))

(defun papis--get-file-paths (doc)
  (mapcar #'(lambda (f) (concat (papis--doc-get-folder doc)
                               "/" f))
          (gethash "files" doc)))

(defun papis--get-ref (doc)
  (gethash "ref" doc))


(defun papis--open-doc (doc)
  (split-window-horizontally)
  (find-file (ivy-read "file: " (papis--get-file-paths doc))))

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
  (papis--cmd "list --libraries" library))

(defun papis-edit (query)
  (interactive "sPapis Query: ")
  (let* ((doc (papis-ivy query))
         (folder (papis--doc-get-folder doc))
         (info (concat folder "/" "info.yaml")))
    (find-file info)
    ;; TODO: implement waiting after editing the file like
    ;; with a C-c C-c binding
    (papis--doc-update doc)))

(defun papis--doc-update (doc)
  (let ((folder (papis--doc-get-folder doc)))
    (papis--cmd (concat "update --doc-folder " folder))))

(defun papis--cmd (cmd &optional library)
  "Helping function to run papis commands"
  (let ((lib-flags (if library (concat "-l " library) "")))
    (shell-command-to-string
     (format "%s %s %s" papis-binary-path lib-flags cmd))))

(defun papis-json (query outfile)
  (shell-command (format "%s export --all --format json '%s' -o %s"
                         papis-binary-path
                         query
                         outfile)))

(defun papis-bibtex (query outfile)
  (shell-command (format "%s export --all --format bibtex '%s' -o %s"
                         papis-binary-path
                         query
                         outfile)))

(defvar papis--refs-to-bibtex-script
"
import argparse
import papis.api
from papis.bibtex import to_bibtex

parser = argparse.ArgumentParser(formatter_class=argparse.RawTextHelpFormatter,
                                 description='')
parser.add_argument('refs', help='References', action='store', nargs='*')
args = parser.parse_args()

docs = []

for ref in args.refs:
    docs.extend(papis.api.get_documents_in_lib(library=None, search=ref))

for d in docs:
    print(to_bibtex(d))
")

(defun papis-exec (python-file &optional arguments)
  (let ((fmt "%s exec %s %s"))
    (shell-command-to-string (format fmt
                                     papis-binary-path
                                     python-file
                                     (or arguments "")))))

(defun papis--refs-to-bibtex (refs)
  (let ((py-script (make-temp-file "papis-bibtex-script" nil ".py")))
    (with-temp-buffer
      (insert papis--refs-to-bibtex-script)
      (write-file py-script))
    (papis-exec py-script (s-join " " refs))))

(defun papis-bibtex-to-string (query)
  (let ((tmp (make-temp-file "")))
    (with-temp-buffer
      (papis-bibtex query tmp)
      (insert-file-contents tmp)
      (buffer-string))))

(defun papis-query (query)
  "Make a general papis query:
   it returns a list of hashtables where every hashtable is a papis document"
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (papis--temp-output-file (make-temp-file "papis-emacs-"))
         (exit-code (papis-json query papis--temp-output-file)))
    (if (not (eq exit-code 0))
        (error "Something happened running the papis command"))
    (json-read-file papis--temp-output-file)))

(defun papis-default-ivy-format-function (doc)
  `(
    ,(format "%s\n\t%s\n\t«%s» +%s %s"
             (gethash "title" doc)
             (gethash "author" doc)
             (gethash "year" doc)
             (or (gethash "tags" doc) "")
             (let ((n (gethash "_note" doc))) (if n (concat ":note " n) "")))
    .
    ,doc))

(defun papis-ivy (query)
  (interactive "sPapis Query: ")
  (let* ((results (papis-query query))
         (formatted-results (mapcar papis-ivy-format-function results))
         (ivy-add-newline-after-prompt t))
    (cdr (assoc
          (ivy-read "Select an entry: " formatted-results)
          formatted-results))))

(provide 'papis)
;; Queries:1 ends here



;; Its implementation is given below:

;; [[file:README.org::*Open pdfs][Open pdfs:2]]
(defun papis-org-ref-get-pdf-filename (key)
    (interactive)
    (let* ((docs (papis-query (format "ref:'%s'" key)))
           (doc (car docs))
           (files (papis--get-file-paths doc)))
      (pcase (length files)
        (1 (car files))
        (_ (ivy-read "" files)))))
;; Open pdfs:2 ends here

;; Citations
;; In general it is recommended to use the citation mechanisms of
;; =org-ref=, however, if for some reason you would like to cite
;; directly from =papis=, you can use the function


;; [[file:README.org::*Citations][Citations:1]]
(defun papis-org-ref-insert-citation-from-query (query)
  (interactive "sPapis Query: ")
  (let* ((doc (papis-ivy query))
         (ref (papis--get-ref doc)))
    (insert (format "cite:%s" ref))))
;; Citations:1 ends here

;; Bibtex entries


;; [[file:README.org::*Bibtex entries][Bibtex entries:1]]
(defun papis-create-papis-bibtex-refs-dblock (bibfile)
  (insert (format "#+begin: papis-bibtex-refs :tangle %s" bibfile))
  (insert "\n")
  (insert "#+end:"))

(defun papis-extract-citations-into-dblock (&optional bibfile)
  (interactive)
  (if (org-find-dblock "papis-bibtex-refs")
      (progn
        (org-show-entry)
        (org-update-dblock))
    (papis-create-papis-bibtex-refs-dblock
     (or bibfile (read-file-name "Bib file: " nil "main.bib")))))
;; Bibtex entries:1 ends here

;; [[file:README.org::*Bibtex entries][Bibtex entries:2]]
(defun org-dblock-write:papis-bibtex-refs (params)
  (let ((tangle-file (or (plist-get params :tangle)
                         (buffer-file-name)))
        (exports ":exports none"))
    (insert
     (format "#+begin_src bibtex %s :tangle %s\n"
             exports
             tangle-file)))
  (let* ((refs (org-ref-get-bibtex-keys))
         (queries (mapcar (lambda (r) (format "ref:\"%s\"" r))
                          refs)))
    (insert (papis--refs-to-bibtex queries)))
  (insert "#+end_src\n"))
;; Bibtex entries:2 ends here

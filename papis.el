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

(defcustom papis--query-prompt
  "sPapis Query: "
  "The prompt to show users in order to accept a query
  "
  :type 'string)
;; Variables:1 ends here

;; [[file:README.org::*=papis-library=][=papis-library=:2]]
(defcustom papis-library
  nil
  "papis library to be used in commands.
   If it is set to nil then the default library of your system will
   be used.
  "
  :type 'string)
;; =papis-library=:2 ends here

;; Document

;; [[file:README.org::*Document][Document:1]]
(defun papis--doc-get-folder (doc)
  (papis--doc-get doc "_papis_local_folder"))
;; Document:1 ends here

;; [[file:README.org::*Document][Document:2]]
(defun papis--get-file-paths (doc)
  (mapcar (lambda (f) (concat (papis--doc-get-folder doc) "/" f))
          (papis--doc-get doc "files")))

(defun papis--doc-get (doc key)
  (gethash key doc))

(defun papis--get-ref (doc)
  (papis--doc-get doc "ref"))
;; Document:2 ends here

;; [[file:README.org::*Document][Document:3]]
(defun papis--doc-update (doc)
  (let ((folder (papis--doc-get-folder doc)))
    (papis--cmd (concat "update --doc-folder " folder))))
;; Document:3 ends here

;; Introduction
;; Most papis commands will need a query, the macro =@papis-query= will
;; take care of having the same query prompt in all commands.

;; [[file:README.org::*Introduction][Introduction:1]]
(defmacro @papis-query ()
  `(interactive ,papis--query-prompt))
;; Introduction:1 ends here

;; Issuing commands to the shell
;;  The main interface with papis commands will be =papis--cmd=
;;  which is a function intended for library writers.

;; [[file:README.org::*Issuing commands to the shell][Issuing commands to the shell:1]]
(defun papis--cmd (cmd &optional with-stdout)
  "Helping function to run papis commands"
  (let ((lib-flags (if papis-library
                       (concat "-l " papis-library)
                     ""))
        (sys (if with-stdout
                 #'shell-command-to-string
               #'shell-command)))
    (funcall sys
     (format "%s %s %s" papis-binary-path lib-flags cmd))))
;; Issuing commands to the shell:1 ends here

;; =papis-query=

;; A papis document object is represented in =papis.el=
;; as a =hashtable=, and the command that turns a query
;; into a list of hashtables is =papis-query=.
;; This is done via the papis' =json= exporter, i.e.,
;; we query python and get a json document with the documents that
;; emacs reads in.


;; [[file:README.org::*=papis-query=][=papis-query=:1]]
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
;; =papis-query=:1 ends here

;; =papis-open=

;; The cornerstone of papis is opening documents, in emacs
;; the command is also available:



;; [[file:README.org::*=papis-open=][=papis-open=:1]]
(defun papis--open-doc (doc)
  (split-window-horizontally)
  (find-file (ivy-read "file: " (papis--get-file-paths doc))))

(defun papis-open (query)
  (@papis-query)
  (papis--open-doc (papis-ivy query)))
;; =papis-open=:1 ends here

;; TODO =papis-edit=

;; You can edit the info files using =papis-edit=,
;; notice that commiting the
;; Implement waiting after editing the file like

;; [[file:README.org::*=papis-edit=][=papis-edit=:1]]
(defun papis-edit (query)
  (@papis-query)
  (let* ((doc (papis-ivy query))
         (folder (papis--doc-get-folder doc))
         (info (concat folder "/" "info.yaml")))
    (find-file info)
    (papis--doc-update doc)))
;; =papis-edit=:1 ends here

;; =papis-exec=


;; [[file:README.org::*=papis-exec=][=papis-exec=:1]]
(defun papis-exec (python-file &optional arguments)
  (let ((fmt "exec %s %s"))
    (papis--cmd (format fmt
                        python-file
                        (or arguments ""))
                t)))
;; =papis-exec=:1 ends here

;; =papis-export=


;; [[file:README.org::*=papis-export=][=papis-export=:1]]
(defun papis-json (query outfile)
  (papis--cmd (format "export --all --format json '%s' -o %s"
                      query
                      outfile)))

(defun papis-bibtex (query outfile)
  (papis--cmd (format "export --all --format bibtex '%s' -o %s"
                      query
                      outfile)))
;; =papis-export=:1 ends here

;; ivy
;; The main dynamic searcher used in papis is [[https://oremacs.com/swiper/][ivy]].

;; [[file:README.org::*ivy][ivy:1]]
(defun papis-default-ivy-format-function (doc)
  `(
    ,(format "%s\n\t%s\n\t«%s» +%s %s"
             (papis--doc-get doc "title")
             (papis--doc-get doc "author")
             (papis--doc-get doc "year")
             (or (papis--doc-get doc "tags") "")
             (let ((n (papis--doc-get doc "_note"))) (if n (concat ":note " n) "")))
    .
    ,doc))

(defun papis-ivy (query)
  (@papis-query)
  (let* ((results (papis-query query))
         (formatted-results (mapcar papis-ivy-format-function results))
         (ivy-add-newline-after-prompt t))
    (cdr (assoc
          (ivy-read "Select an entry: " formatted-results)
          formatted-results))))
;; ivy:1 ends here

;; =papis+doi=

;; We define the link

;; [[file:README.org::*=papis+doi=][=papis+doi=:1]]
(org-link-set-parameters "papis+doi"
                         :follow #'ol-papis+doi-open
                         :export #'ol-papis+doi-export
                         :complete #'org-link-papis-store-doi)

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
;; =papis+doi=:1 ends here

;; =papis=


;; [[file:README.org::*=papis=][=papis=:1]]
(org-link-set-parameters "papis"
                         :follow #'ol-papis-open
                         :export #'ol-papis-export)

(defun ol-papis-open (link)
  (let ((doc (papis-ivy link)))
    (cond
     (doc (papis--open-doc doc))
     (t (error "No doc found")))))

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
  (@papis-query)
  (let ((papis-command (concat "papis list --format "
                               "{doc[doi]}"
                               " --all "
                               "'" query "'")))
    (car (s-lines
          (shell-command-to-string
           papis-command)))))

(defun org-papis-store-doi-link (query)
  (@papis-query)
  (let ((doc (papis-ivy query)))
    (insert (format "[[papis+doi:%s][%s]]"
                    (papis--doc-get doc "doi")
                    (papis--doc-get doc "title")))))

(defun org-papis-store-url-link (query)
  (@papis-query)
  (let ((doc (papis-ivy query)))
    (insert (format "[[%s][%s]]"
                    (papis--doc-get doc "url")
                    (papis--doc-get doc "title")))))
;; =papis=:1 ends here

;; [[file:README.org::*=papis=][=papis=:2]]
(defun org-papis-store-file-link (query)
  (@papis-query)
  (let ((doc (papis-ivy query)))
    (insert (format "[[file:%s][%s]]"
                    (ivy-read "file: " (papis--get-file-paths doc))
                    (papis--doc-get doc "title")))))
;; =papis=:2 ends here



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
  (@papis-query)
  (let* ((doc (papis-ivy query))
         (ref (papis--get-ref doc)))
    (insert (format "cite:%s" ref))))
;; Citations:1 ends here

;; [[file:README.org::*Convert references into bibtex entries][Convert references into bibtex entries:2]]
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
;; Convert references into bibtex entries:2 ends here

;; [[file:README.org::*Convert references into bibtex entries][Convert references into bibtex entries:3]]
(defun papis--refs-to-bibtex (refs)
  (let ((py-script (make-temp-file "papis-bibtex-script" nil ".py")))
    (with-temp-buffer
      (insert papis--refs-to-bibtex-script)
      (write-file py-script))
    (papis-exec py-script (s-join " " refs))))
;; Convert references into bibtex entries:3 ends here

;; The =papis-bibtex-refs= dynamic block


;; [[file:README.org::*The =papis-bibtex-refs= dynamic block][The =papis-bibtex-refs= dynamic block:1]]
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
;; The =papis-bibtex-refs= dynamic block:1 ends here

;; [[file:README.org::*The =papis-bibtex-refs= dynamic block][The =papis-bibtex-refs= dynamic block:2]]
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
;; The =papis-bibtex-refs= dynamic block:2 ends here

;; End

;; [[file:README.org::*End][End:1]]
(provide 'papis)
;; End:1 ends here

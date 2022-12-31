

;; - We interact with papis through the papis' json exporter.
;; - We use ~org-links~ to get information directly from papis.

;; The libraries that we will need are therefore:

;; [[file:README.org::*Generalities][Generalities:1]]
(require 'ol)
(require 'json)
;; Generalities:1 ends here

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

(defcustom papis-read-format-function
  #'papis-default-read-format-function
  "Function taking a papis document (hashmap) and outputing a
   string representation of it to be fed into the reader.")

(defcustom papis--query-prompt
  "Papis Query: "
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

;; [[file:README.org::*Document][Document:1]]
(defun papis--doc-get-folder (doc)
  (papis--doc-get doc "_papis_local_folder"))

(defun papis--id (doc)
  (let ((id (papis--doc-get doc "papis_id")))
    (unless id
      (error "Document '%s' does not have an id!"
             doc))
    id))

(defun papis--id-query (doc)
  (format "papis_id:%s" (papis--id doc)))
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


;; Most papis commands will need a query, the macro =@papis-query= will
;; take care of having the same query prompt in all commands.

;; [[file:README.org::*Introduction][Introduction:1]]
(defmacro @papis-query ()
  `(interactive ,papis--query-prompt))
;; Introduction:1 ends here


;; The main interface with papis commands will be =papis--cmd=
;; which is a function intended for library writers.

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



;; The cornerstone of papis is opening documents, in emacs
;; the command is also available:



;; [[file:README.org::*=papis-open=][=papis-open=:1]]
(defun papis-open (doc)
  (interactive (list (papis--read-doc)))
  (let* ((files (papis--get-file-paths doc))
         (file (pcase (length files)
                 (1 (car files))
                 (0 (error "Doc has no files"))
                 (_ (completing-read "file: " files)))))
    (split-window-horizontally)
    (find-file file)))
;; =papis-open=:1 ends here

;; [[file:README.org::*=papis-notes=][=papis-notes=:1]]
(defun papis-notes (doc)
  (interactive (list (papis--read-doc)))
  (let (
        ;; (folder (papis--cmd (format "list %s" (papis--id-query doc)) t))
        (folder (papis--doc-get-folder doc))
        (maybe-notes (papis--doc-get doc "notes")))
    (when maybe-notes
      (find-file (f-join folder maybe-notes)))))
;; =papis-notes=:1 ends here



;; You can edit the info files using =papis-edit=,
;; notice that commiting the
;; Implement waiting after editing the file like

;; [[file:README.org::*=papis-edit=][=papis-edit=:1]]
(defun papis-edit (doc)
  (interactive (list (papis--read-doc)))
  (let* ((folder (papis--doc-get-folder doc))
         (info (concat folder "/" "info.yaml")))
    (find-file info)
    (papis--doc-update doc)))
;; =papis-edit=:1 ends here

;; [[file:README.org::*=papis-exec=][=papis-exec=:1]]
(defun papis-exec (python-file &optional arguments)
  (let ((fmt "exec %s %s"))
    (papis--cmd (format fmt
                        python-file
                        (or arguments ""))
                t)))
;; =papis-exec=:1 ends here

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


;; The main dynamic searcher used in papis is [[https://oremacs.com/swiper/][ivy]].

;; [[file:README.org::*Document reader][Document reader:1]]
(defun papis-default-read-format-function (doc)
  `(
    ,(format "%s\n\t%s\n\t«%s» +%s %s"
             (papis--doc-get doc "title")
             (papis--doc-get doc "author")
             (papis--doc-get doc "year")
             (or (papis--doc-get doc "tags") "")
             (let ((n (papis--doc-get doc "_note"))) (if n (concat ":note " n) "")))
    .
    ,doc))

(defun papis--read-doc ()
  (let* ((results (papis-query (read-string papis--query-prompt
                                            nil 'papis)))
         (formatted-results (mapcar papis-read-format-function results)))
    (cdr (assoc
          (completing-read "Select an entry: " formatted-results)
          formatted-results))))

(defun papis--from-id (papis-id)
  (let* ((query (format "papis_id:%s" papis-id))
         (results (papis-query query)))
    (pcase (length results)
      (0 (error "No documents found with papis_id '%s'"
                papis-id))
      (1 (car results))
      (_ (error "Too many documents (%d) found with papis_id '%s'"
                (length results) papis-id)))))
;; Document reader:1 ends here

;; [[file:README.org::*=papis=][=papis=:1]]
(require 'ol-doi)
(org-link-set-parameters "papis"
                         :follow (lambda (papis-id)
                                   (papis-open (papis--from-id papis-id)))
                         :export #'ol-papis-export
                         :complete (lambda (&optional arg)
                                     (format "papis:%s"
                                             (papis--doc-get (papis--read-doc)
                                                             "papis_id")))
                         :insert-description
                         (lambda (link desc)
                           (let* ((papis-id (string-replace "papis:"  "" link))
                                  (doc (papis--from-id papis-id)))
                             (papis--doc-get doc "title"))))

(defun ol-papis-export (papis-id description format info)
  (let* ((doc (papis--from-id papis-id))
        (doi (papis--doc-get doc "doi"))
        (url (papis--doc-get doc "url")))
    (cond
      (doi (org-link-doi-export doi description format info)))))
;; =papis=:1 ends here


;; When doing research, often you would like to create some notes on every paper
;; and write some sections with the section titles being links to the papers
;; with some properties so that you can use org-mode's colum mode.

;; You can use the following function to create a link with properties


;; [[file:README.org::*Paper sections][Paper sections:1]]
(defun papis-org-insert-heading (doc)
  (interactive (list (papis--read-doc)))
  (let ((title (papis--doc-get doc "title"))
        (author (papis--doc-get doc "author"))
        (year (papis--doc-get doc "year"))
        (doi (papis--doc-get doc "doi"))
        (papis-id (papis--doc-get doc "papis_id")))
    (org-insert-heading)
    (insert (format "[[papis:%s][%s]]" papis-id title))
    (org-set-property "PAPIS_ID" papis-id)
    (org-set-property "AUTHOR" author)
    (org-set-property "TITLE" title)
    (org-set-property "YEAR" (format "%s" year))
    (org-set-property "DOI" doi)))
;; Paper sections:1 ends here



;; Its implementation is given below:

;; [[file:README.org::*Open pdfs][Open pdfs:2]]
(defun papis-org-ref-get-pdf-filename (key)
    (interactive)
    (let* ((docs (papis-query (format "ref:'%s'" key)))
           (doc (car docs))
           (files (papis--get-file-paths doc)))
      (pcase (length files)
        (1 (car files))
        (_ (completing-read "" files)))))
;; Open pdfs:2 ends here


;; In general it is recommended to use the citation mechanisms of
;; =org-ref=, however, if for some reason you would like to cite
;; directly from =papis=, you can use the function


;; [[file:README.org::*Citations][Citations:1]]
(defun papis-insert-citation (doc)
  (interactive (list (papis--read-doc)))
  (let* ((ref (papis--get-ref doc)))
    (if (fboundp 'citar-insert-citation)
        (citar-insert-citation (list ref))
      (insert (format "[cite:@%s]" ref)))))
;; Citations:1 ends here



;; and we will need also a way of listing all the keys of the document
;; for further functions. I took this from the good =citar= package


;; [[file:README.org::*Citations][Citations:2]]
(defun papis-org-list-keys ()
  "List citation keys in the org buffer."
  (let ((org-tree (org-element-parse-buffer)))
    (delete-dups
     (org-element-map org-tree 'citation-reference
       (lambda (r) (org-element-property :key r))
       org-tree))))
;; Citations:2 ends here



;; #+RESULTS: references-to-bibtex-python-script



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
  (let* ((refs (papis-org-list-keys))
         (queries (mapcar (lambda (r) (format "ref:\"%s\"" r))
                          refs)))
    (insert (papis--refs-to-bibtex queries)))
  (insert "#+end_src\n"))
;; The =papis-bibtex-refs= dynamic block:2 ends here

;; [[file:README.org::*End][End:1]]
(provide 'papis)
;; End:1 ends here

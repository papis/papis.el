
# Table of Contents

1.  [Papis.el](#org0d40bfb)
    1.  [Motivation](#org09ca56d)
    2.  [Disclaimer](#org89b7d5b)
    3.  [What is implemented](#org6426e45)
2.  [Implementation](#org1c8a459)
    1.  [Generalities](#orga5d6334)
    2.  [Variables](#orge5ec143)
    3.  [General functions](#org7f78cc6)
    4.  [Commands](#org91038be)
    5.  [Papis-ivy](#org2b54c4e)
    6.  [Org-links](#org791173c)
        1.  [papis+doi](#org0ef2168)
        2.  [Queries](#orgee55549)
    7.  [Org ref compatibility](#org0ade50a)
        1.  [Open pdfs](#orgce608f2)
        2.  [Citations](#org8082e95)
        3.  [Bibtex entries](#org1196a47)
3.  [Bibliography](#org8429e30)


<a id="org0d40bfb"></a>

# Papis.el

![img](https://papis.github.io/images/emacs-papis.gif)


<a id="org09ca56d"></a>

## Motivation

The main motivation of this package is to make it
easy to interact with `org-mode` and
`papis`.

We do not want to reinvent the wheel, so this project
should be thought to play well with the very good
package [org-ref](https://github.com/jkitchin/org-ref).


<a id="org89b7d5b"></a>

## Disclaimer

If you're an emacs lisp hacker, feel free to chip in.
Otherwise this project should be treated as β software
and it might just completely change in the future.


<a id="org6426e45"></a>

## What is implemented

I have implemented some functions that I use in my day-to-day
research and worfklow.

There is a \`papis-ivy\` function to open and so on,
and org mode links.

At some point it will get documented&#x2026;


<a id="org1c8a459"></a>

# Implementation

`papis.el` is written as a literate program <sup id="39f041f6b1d2d698620dbd1d6c83c888"><a href="#LiteratePrograKnuth1984" title="Knuth, Literate Programming, {The Computer Journal}, v(), 97--111 (1984).">LiteratePrograKnuth1984</a></sup>.


<a id="orga5d6334"></a>

## Generalities

-   We interact with papis through the papis' json exporter.
-   We use `org-links` to get information directly from papis.

The libraries that we will need are therefore:

    (require 'ol)
    (require 'json)


<a id="orge5ec143"></a>

## Variables

    
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


<a id="org7f78cc6"></a>

## General functions


<a id="org91038be"></a>

## Commands


<a id="org2b54c4e"></a>

## Papis-ivy


<a id="org791173c"></a>

## Org-links


<a id="org0ef2168"></a>

### papis+doi

We define the link

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


<a id="orgee55549"></a>

### Queries

    
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


<a id="org0ade50a"></a>

## Org ref compatibility


<a id="orgce608f2"></a>

### Open pdfs

`org-ref` can open the pdf of a publicaction
from the `cite:my-reference` link, but in the case of papis
this pdf lives in an isolated folder of its own.

However in `org-ref` you can customize how you get the pdf
from the `cite` link through the
<org-ref-get-pdf-filename-function>.
Therefore, in order to use papis to open the pdf of the referenced
documents you can set

    (setq org-ref-get-pdf-filename-function
          #'papis-org-ref-get-pdf-filename)

Its implementation is given below:

    (defun papis-org-ref-get-pdf-filename (key)
        (interactive)
        (let* ((docs (papis-query (format "ref:'%s'" key)))
               (doc (car docs))
               (files (papis--get-file-paths doc)))
          (pcase (length files)
            (1 (car files))
            (_ (ivy-read "" files)))))


<a id="org8082e95"></a>

### Citations

In general it is recommended to use the citation mechanisms of
`org-ref`, however, if for some reason you would like to cite
directly from `papis`, you can use the function

    (defun papis-org-ref-insert-citation-from-query (query)
      (interactive "sPapis Query: ")
      (let* ((doc (papis-ivy query))
             (ref (papis--get-ref doc)))
        (insert (format "cite:%s" ref))))


<a id="org1196a47"></a>

### Bibtex entries

    
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


<a id="org8429e30"></a>

# Bibliography

# Bibliography
<a id="LiteratePrograKnuth1984"></a>[LiteratePrograKnuth1984] Knuth, Literate Programming, <i>The Computer Journal</i>, <b>27</b>, 97-111 (1984). <a href="http://dx.doi.org/10.1093/comjnl/27.2.97">link</a>. <a href="http://dx.doi.org/10.1093/comjnl/27.2.97">doi</a>. [↩](#39f041f6b1d2d698620dbd1d6c83c888)



# Table of Contents

1.  [Papis.el](#org0d6544c)
    1.  [Motivation](#orga2f3b34)
    2.  [Disclaimer](#org5a804e9)
    3.  [What is implemented](#orgd49bd91)
2.  [Implementation](#orgda3d290)
    1.  [Generalities](#orge802992)
    2.  [Variables](#org074e444)
    3.  [General functions](#orgb0d14f9)
    4.  [Commands](#orgf97744c)
    5.  [Papis-ivy](#org256c3b2)
    6.  [Org-links](#orgfd45a4a)
        1.  [papis+doi](#org8a6265d)
        2.  [Queries](#org61b1973)
    7.  [Org ref compatibility](#org5347904)
        1.  [Citations](#org019184b)
        2.  [Open pdfs](#org018a8e9)


<a id="org0d6544c"></a>

# Papis.el

![img](https://papis.github.io/images/emacs-papis.gif)


<a id="orga2f3b34"></a>

## Motivation

The main motivation of this package is to make it
easy to interact with `org-mode` and
`papis`.

We do not want to reinvent the wheel, so this project
should be thought to play well with the very good
package [org-ref](https://github.com/jkitchin/org-ref).


<a id="org5a804e9"></a>

## Disclaimer

If you're an emacs lisp hacker, feel free to chip in.
Otherwise this project should be treated as β software
and it might just completely change in the future.


<a id="orgd49bd91"></a>

## What is implemented

I have implemented some functions that I use in my day-to-day
research and worfklow.

There is a \`papis-ivy\` function to open and so on,
and org mode links.

At some point it will get documented&#x2026;


<a id="orgda3d290"></a>

# Implementation

`papis.el` is written as a literate program


<a id="orge802992"></a>

## Generalities

-   We interact with papis through the papis' json exporter.
-   We use `org-links` to get information directly from papis.

The libraries that we will need are therefore:

    (require 'ol)
    (require 'json)


<a id="org074e444"></a>

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


<a id="orgb0d14f9"></a>

## General functions


<a id="orgf97744c"></a>

## Commands


<a id="org256c3b2"></a>

## Papis-ivy


<a id="orgfd45a4a"></a>

## Org-links


<a id="org8a6265d"></a>

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


<a id="org61b1973"></a>

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
      (shell-command (format "papis export --all --format json '%s' -o %s"
                             query
                             outfile)))
    
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


<a id="org5347904"></a>

## Org ref compatibility


<a id="org019184b"></a>

### Citations

    (defun papis-org-ref-insert-citation-from-query (query)
      (interactive "sPapis Query: ")
      (let* ((doc (papis-ivy query))
             (ref (papis--get-ref doc)))
        (insert (format "cite:%s" ref))))


<a id="org018a8e9"></a>

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


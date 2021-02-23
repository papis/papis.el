
# Table of Contents

1.  [Papis.el](#org61ca650)
    1.  [Motivation](#org4591779)
    2.  [Disclaimer](#org69fe7c0)
    3.  [What is implemented](#org195d11a)
2.  [Implementation](#org273e1ef)
    1.  [Generalities](#orgf14e21f)
    2.  [Variables](#orgff06721)
        1.  [`papis-library`](#org02c313f)
    3.  [Document](#orge89d870)
    4.  [Commands](#org5a9939d)
        1.  [Introduction](#org2e34b4a)
        2.  [`papis-open`](#orgecfbbbd)
        3.  [`papis-edit`](#orgefe265a)
        4.  [`papis-exec`](#org24da47e)
        5.  [`papis-export`](#org7fe7b9d)
    5.  [ivy](#orgdec440c)
    6.  [Org-links](#org0fb2d63)
        1.  [`papis+doi`](#orgff8d7d5)
        2.  [`papis`](#orgad04c5a)
    7.  [`org-ref` compatibility](#orge6c8d37)
        1.  [Open pdfs](#orga5eeb29)
        2.  [Citations](#orge1b7335)
    8.  [Bibtex entries](#org9200aa0)
        1.  [Convert references into bibtex entries](#org68bfc1d)
        2.  [The `papis-bibtex-refs` dynamic block](#org197585c)
    9.  [End](#orgead4604)
3.  [Bibliography](#org062b68a)


<a id="org61ca650"></a>

# Papis.el

![img](https://papis.github.io/images/emacs-papis.gif)


<a id="org4591779"></a>

## Motivation

The main motivation of this package is to make it
easy to interact with `org-mode` and
`papis`.

We do not want to reinvent the wheel, so this project
should be thought to play well with the very good
package [org-ref](https://github.com/jkitchin/org-ref).


<a id="org69fe7c0"></a>

## Disclaimer

If you're an emacs lisp hacker, feel free to chip in.
Otherwise this project should be treated as β software
and it might just completely change in the future.


<a id="org195d11a"></a>

## What is implemented

I have implemented some functions that I use in my day-to-day
research and worfklow.

There is a \`papis-ivy\` function to open and so on,
and org mode links.

At some point it will get documented&#x2026;


<a id="org273e1ef"></a>

# Implementation

`papis.el` is written as a literate program <sup id="39f041f6b1d2d698620dbd1d6c83c888"><a href="#LiteratePrograKnuth1984" title="Knuth, Literate Programming, {The Computer Journal}, v(), 97--111 (1984).">LiteratePrograKnuth1984</a></sup>.


<a id="orgf14e21f"></a>

## Generalities

-   We interact with papis through the papis' json exporter.
-   We use `org-links` to get information directly from papis.

The libraries that we will need are therefore:

    (require 'ol)
    (require 'json)


<a id="orgff06721"></a>

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
    
    (defcustom papis--query-prompt
      "sPapis Query: "
      "The prompt to show users in order to accept a query
      "
      :type 'string)

You can set the


<a id="org02c313f"></a>

### `papis-library`

You can set the main library used in papis by setting

    (setq papis-library "my-other-library")

    (defcustom papis-library
      nil
      "papis library to be used in commands.
       If it is set to nil then the default library of your system will
       be used.
      "
      :type 'string)


<a id="orge89d870"></a>

## Document

    (defun papis--doc-get-folder (doc)
      (papis--doc-get doc "_papis_local_folder"))

    (defun papis--get-file-paths (doc)
      (mapcar (lambda (f) (concat (papis--doc-get-folder doc) "/" f))
              (papis--doc-get doc "files")))
    
    (defun papis--doc-get (doc key)
      (gethash key doc))
    
    (defun papis--get-ref (doc)
      (papis--doc-get doc "ref"))

    (defun papis--doc-update (doc)
      (let ((folder (papis--doc-get-folder doc)))
        (papis--cmd (concat "update --doc-folder " folder))))


<a id="org5a9939d"></a>

## Commands


<a id="org2e34b4a"></a>

### Introduction

Most papis commands will need a query, the macro `@papis-query` will
take care of having the same query prompt in all commands.

    (defmacro @papis-query ()
      `(interactive ,papis--query-prompt))

1.  Issuing commands to the shell

    The main interface with papis commands will be `papis--cmd`
    which is a function intended for library writers.
    
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

2.  `papis-query`

    A papis document object is represented in `papis.el`
    as a `hashtable`, and the command that turns a query
    into a list of hashtables is `papis-query`.
    This is done via the papis' `json` exporter, i.e.,
    we query python and get a json document with the documents that
    emacs reads in.
    
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


<a id="orgecfbbbd"></a>

### `papis-open`

The cornerstone of papis is opening documents, in emacs
the command is also available:

    (defun papis--open-doc (doc)
      (split-window-horizontally)
      (find-file (ivy-read "file: " (papis--get-file-paths doc))))
    
    (defun papis-open (query)
      (@papis-query)
      (papis--open-doc (papis-ivy query)))


<a id="orgefe265a"></a>

### TODO `papis-edit`

You can edit the info files using `papis-edit`,
notice that commiting the
Implement waiting after editing the file like

    (defun papis-edit (query)
      (@papis-query)
      (let* ((doc (papis-ivy query))
             (folder (papis--doc-get-folder doc))
             (info (concat folder "/" "info.yaml")))
        (find-file info)
        (papis--doc-update doc)))


<a id="org24da47e"></a>

### `papis-exec`

    (defun papis-exec (python-file &optional arguments)
      (let ((fmt "exec %s %s"))
        (papis--cmd (format fmt
                            python-file
                            (or arguments ""))
                    t)))


<a id="org7fe7b9d"></a>

### `papis-export`

    (defun papis-json (query outfile)
      (papis--cmd (format "export --all --format json '%s' -o %s"
                          query
                          outfile)))
    
    (defun papis-bibtex (query outfile)
      (papis--cmd (format "export --all --format bibtex '%s' -o %s"
                          query
                          outfile)))


<a id="orgdec440c"></a>

## ivy

The main dynamic searcher used in papis is [ivy](https://oremacs.com/swiper/).

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


<a id="org0fb2d63"></a>

## Org-links


<a id="orgff8d7d5"></a>

### `papis+doi`

We define the link

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


<a id="orgad04c5a"></a>

### `papis`

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

    (defun org-papis-store-file-link (query)
      (@papis-query)
      (let ((doc (papis-ivy query)))
        (insert (format "[[file:%s][%s]]"
                        (ivy-read "file: " (papis--get-file-paths doc))
                        (papis--doc-get doc "title")))))


<a id="orge6c8d37"></a>

## `org-ref` compatibility


<a id="orga5eeb29"></a>

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


<a id="orge1b7335"></a>

### Citations

In general it is recommended to use the citation mechanisms of
`org-ref`, however, if for some reason you would like to cite
directly from `papis`, you can use the function

    (defun papis-org-ref-insert-citation-from-query (query)
      (@papis-query)
      (let* ((doc (papis-ivy query))
             (ref (papis--get-ref doc)))
        (insert (format "cite:%s" ref))))


<a id="org9200aa0"></a>

## Bibtex entries

<div class="warning">
Note that this needs the command `papis-exec`,
which is available in papis from version `0.12` onwards.

</div>

In this section we want to develop a way to generate a bibtex bibliography
from references appearing in the document currently being edited.


<a id="org68bfc1d"></a>

### Convert references into bibtex entries

First we need a script that accepts a list of

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

    (defvar papis--refs-to-bibtex-script
    "
    <<references-to-bibtex-python-script>>
    ")

    (defun papis--refs-to-bibtex (refs)
      (let ((py-script (make-temp-file "papis-bibtex-script" nil ".py")))
        (with-temp-buffer
          (insert papis--refs-to-bibtex-script)
          (write-file py-script))
        (papis-exec py-script (s-join " " refs))))


<a id="org197585c"></a>

### The `papis-bibtex-refs` dynamic block

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


<a id="orgead4604"></a>

## End

    (provide 'papis)


<a id="org062b68a"></a>

# Bibliography

# Bibliography
<a id="LiteratePrograKnuth1984"></a>[LiteratePrograKnuth1984] Knuth, Literate Programming, <i>The Computer Journal</i>, <b>27</b>, 97-111 (1984). <a href="http://dx.doi.org/10.1093/comjnl/27.2.97">link</a>. <a href="http://dx.doi.org/10.1093/comjnl/27.2.97">doi</a>. [↩](#39f041f6b1d2d698620dbd1d6c83c888)


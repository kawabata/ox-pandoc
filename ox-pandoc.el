;;; ox-pandoc.el --- org exporter for pandoc.        -*- lexical-binding: t; -*-

;; Copyright (C) 2014 KAWABATA, Taichi

;; Filename: ox-pandoc.el
;; Description: Another org exporter for Pandoc
;; Author: KAWABATA, Taichi <kawabata.taichi@gmail.com>
;; Created: 2014-07-20
;; Version: 1.140721
;; Package-Requires: ((org "8.2") (dash "1.0") (ht "1.0"))
;; Keywords: tools
;; URL: https://github.com/kawabata/ox-pandoc

;;; Commentary:

;; * ox-pandoc
;;
;; This is another exporter for Org 8.0 that outputs to various formats
;; via Pandoc (version 1.12.4 or later).
;;
;; * Usage
;;
;; Various exporters are available, include:
;;
;; - =org-pandoc-export-as-html=  :: Exports the HTML format to a buffer.
;; - =org-pandoc-export-to-beamer=  :: Exports the Beamer LaTeX format to a file.
;;
;; * Customization
;;
;; ** Variables
;;
;; - =org-pandoc-options= :: General Pandoc options.
;; - =org-pandoc-options-for-FORMAT= :: Format-specific options.
;; - =org-pandoc-epub-right= :: EPub Copyright Statement.
;;
;; Document-specific options can be set to "#+PANDOC_OPTIONS:" in the
;; document.  Latter options will override former options.
;;
;; Below are examples:
;;
;; : (setq org-pandoc-options '((standalone . t))) ;; for all output
;; : (setq org-pandoc-options-docx '((standalone . nil))) ;; cancel general settings.
;; : # following will only apply to specific document.
;; : #+PANDOC_OPTIONS: standalone:t
;;
;; In PANDOC_OPTIONS specification, if option value includes space, you
;; can surround entire setting with quote. e.g.
;;
;; : #+PANDOC_OPTIONS: "epub-cover-image:/home/a/test file.png" standalone:nil
;;
;; ** In-File Options
;;
;; - #+PANDOC_OPTIONS :: Add command line options to the Pandoc process.
;; - #+EPUB_RIGHTS: :: copyright info to be embedded to ePub metadata.
;; - #+EPUB_CHAPTER_LEVEL: :: same as 'epub-chapter-level' PANDOC_OPTION.
;; - #+EPUB_COVER: :: same as 'epub-cover-image' PANDOC_OPTION.
;; - #+EPUB_EMBED_FONT: :: same as 'epub-embed-font' PANDOC_OPTION.
;; - #+EPUB_METADATA: :: same as 'epub-metadata' PANDOC_OPTION.
;; - #+EPUB_STYLESHEET :: same as 'epub-stylesheet' PANDOC_OPTION.
;; - #+BIBLIOGRAPHY :: same as 'bibliography' PANDOC_OPTION.
;;
;; ** Note
;;
;; This is based on [[https://github.com/robtillotson/org-pandoc][org-pandoc]], but entirely re-written.
;;
;; ** TODO-list
;;
;; - Currently, citation style is [@num], rather than {\item}, so we may need some preprocessor to convert this.

;;; Code:

(require 'ox-org)
(require 'dash)
(require 'ht)

(defgroup org-pandoc nil
  "Options specific to Pandoc export back-end."
  :tag "Org Pandoc"
  :group 'org-export
  :version "24.3"
  :package-version '(Org . "8.2"))

(defconst org-pandoc-valid-options
  '(ascii atx-headers base-header-level biblatex bibliography chapters
    citation-abbreviations columns csl css data-dir
    default-image-extension dump-args email-obfuscation
    epub-chapter-level epub-cover-image epub-embed-font epub-metadata
    epub-stylesheet filter gladtex highlight-style html-q-tags html5
    id-prefix ignore-args include-after-body include-before-body
    include-in-header incremental indented-code-classes jsmath
    latex-engine latexmathml listings mathjax mathml metadata mimetex
    natbib no-highlight no-tex-ligatures no-wrap normalize number-offset
    number-sections offline old-dashes output parse-raw preserve-tabs
    print-default-data-file print-default-template read reference-docx
    reference-links reference-odt section-divs self-contained
    slide-level smart standalone tab-stop table-of-contents template
    title-prefix to toc-depth variable version webtex))

(defconst org-pandoc-extensions
  '((markdown_github . md) (beamer . tex)
    (dzslides . html) (epub3 . epub) (html5 . html) (latex . tex)
    (markdown . md) (markdown_github . md) (markdown_mmd . md)
    (markdown_strict . md) (opendocument . odt) (revealjs . html)
    (s5 . html) (slideous . html)))

(defcustom org-pandoc-options '((standalone . t)
                                (table-of-contents . t))
  "Pandoc options."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-command "pandoc"
  "Pandoc command."
  :group 'org-export-pandoc
  :type 'string)

(org-export-define-derived-backend 'pandoc 'org
  :translate-alist '((template . org-pandoc-template))
  :export-block "PANDOC"
  :menu-entry
  '(?p "export via pandoc"
       (
        (?a "as asciidoc." org-pandoc-export-as-asciidoc)
        (?A "to asciidoc." org-pandoc-export-to-asciidoc)
        (?b "as beamer." org-pandoc-export-as-beamer)
        (?B "to beamer." org-pandoc-export-to-beamer)
        (?c "as context." org-pandoc-export-as-context)
        (?C "to context." org-pandoc-export-to-context)
        (?d "as docbook." org-pandoc-export-as-docbook)
        (?D "to docbook." org-pandoc-export-to-docbook)
        (?d "as docx." org-pandoc-export-as-docx)
        (?d "as dzslides." org-pandoc-export-as-dzslides)
        (?D "to dzslides." org-pandoc-export-to-dzslides)
        (?e "as epub." org-pandoc-export-as-epub)
        (?e "as epub3." org-pandoc-export-as-epub3)
        (?f "as fb2." org-pandoc-export-as-fb2)
        (?F "to fb2." org-pandoc-export-to-fb2)
        (?h "as html." org-pandoc-export-as-html)
        (?H "to html." org-pandoc-export-to-html)
        (?h "as html5." org-pandoc-export-as-html5)
        (?H "to html5." org-pandoc-export-to-html5)
        (?i "as icml." org-pandoc-export-as-icml)
        (?I "to icml." org-pandoc-export-to-icml)
        (?j "as json." org-pandoc-export-as-json)
        (?J "to json." org-pandoc-export-to-json)
        (?l "as latex." org-pandoc-export-as-latex)
        (?L "to latex." org-pandoc-export-to-latex)
        (?m "as man." org-pandoc-export-as-man)
        (?M "to man." org-pandoc-export-to-man)
        (?m "as markdown." org-pandoc-export-as-markdown)
        (?M "to markdown." org-pandoc-export-to-markdown)
        (?m "as markdown_github." org-pandoc-export-as-markdown_github)
        (?M "to markdown_github." org-pandoc-export-to-markdown_github)
        (?m "as markdown_mmd." org-pandoc-export-as-markdown_mmd)
        (?M "to markdown_mmd." org-pandoc-export-to-markdown_mmd)
        (?m "as markdown_phpextra." org-pandoc-export-as-markdown_phpextra)
        (?M "to markdown_phpextra." org-pandoc-export-to-markdown_phpextra)
        (?m "as markdown_strict." org-pandoc-export-as-markdown_strict)
        (?M "to markdown_strict." org-pandoc-export-to-markdown_strict)
        (?m "as mediawiki." org-pandoc-export-as-mediawiki)
        (?M "to mediawiki." org-pandoc-export-to-mediawiki)
        (?n "as native." org-pandoc-export-as-native)
        (?N "to native." org-pandoc-export-to-native)
        (?o "as odt." org-pandoc-export-as-odt)
        (?O "to odt." org-pandoc-export-to-odt)
        (?o "as opendocument." org-pandoc-export-as-opendocument)
        (?o "as opml." org-pandoc-export-as-opml)
        (?O "to opml." org-pandoc-export-to-opml)
        (?o "as org." org-pandoc-export-as-org)
        (?O "to org." org-pandoc-export-to-org)
        (?p "as plain." org-pandoc-export-as-plain)
        (?P "to plain." org-pandoc-export-to-plain)
        (?r "as revealjs." org-pandoc-export-as-revealjs)
        (?R "to revealjs." org-pandoc-export-to-revealjs)
        (?r "as rst." org-pandoc-export-as-rst)
        (?R "to rst." org-pandoc-export-to-rst)
        (?r "as rtf." org-pandoc-export-as-rtf)
        (?R "to rtf." org-pandoc-export-to-rtf)
        (?s "as s5." org-pandoc-export-as-s5)
        (?S "to s5." org-pandoc-export-to-s5)
        (?s "as slideous." org-pandoc-export-as-slideous)
        (?S "to slideous." org-pandoc-export-to-slideous)
        (?s "as slidy." org-pandoc-export-as-slidy)
        (?S "to slidy." org-pandoc-export-to-slidy)
        (?t "as texinfo." org-pandoc-export-as-texinfo)
        (?T "to texinfo." org-pandoc-export-to-texinfo)
        (?t "as textile." org-pandoc-export-as-textile)
        (?T "to textile." org-pandoc-export-to-textile)
        ))
  :options-alist
  '((:pandoc-options "PANDOC_OPTIONS" nil nil space)
    (:epub-chapter-level "EPUB_CHAPTER_LEVEL" nil nil t)
    (:epub-cover-image "EPUB_COVER" nil nil t)
    (:epub-embed-font "EPUB_EMBED_FONT" nil nil t)
    (:epub-metadata "EPUB_METADATA" nil nil t)
    (:epub-stylesheet "EPUB_STYLESHEET" nil nil t)
    (:epub-rights "EPUB_RIGHTS" nil nil newline)
    (:bibliography "BIBLIOGRAPHY")))

(defcustom org-pandoc-epub-rights
  (concat "Copyright " (format-time-string "%Y")
          (if user-full-name (concat " " user-full-name))
          (if user-mail-address (concat " <" user-mail-address ">")))
  "Pandoc option for EPUB copyrihgt statement."
  :group 'org-export-pandoc
  :type 'string)

;;; each backend processor

(defcustom org-pandoc-options-for-asciidoc nil
  "Pandoc options for asciidoc."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-asciidoc (&optional a s v b e)
  (interactive) (org-pandoc-export 'asciidoc a s v b e))

;;;###autoload
(defun org-pandoc-export-as-asciidoc (&optional a s v b e)
  (interactive) (org-pandoc-export 'asciidoc a s v b e t))

(defcustom org-pandoc-options-for-beamer nil
  "Pandoc options for beamer."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-beamer (&optional a s v b e)
  (interactive) (org-pandoc-export 'beamer a s v b e))

;;;###autoload
(defun org-pandoc-export-as-beamer (&optional a s v b e)
  (interactive) (org-pandoc-export 'beamer a s v b e t))

(defcustom org-pandoc-options-for-context nil
  "Pandoc options for context."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-context (&optional a s v b e)
  (interactive) (org-pandoc-export 'context a s v b e))

;;;###autoload
(defun org-pandoc-export-as-context (&optional a s v b e)
  (interactive) (org-pandoc-export 'context a s v b e t))

(defcustom org-pandoc-options-for-docbook nil
  "Pandoc options for docbook."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-docbook (&optional a s v b e)
  (interactive) (org-pandoc-export 'docbook a s v b e))

;;;###autoload
(defun org-pandoc-export-as-docbook (&optional a s v b e)
  (interactive) (org-pandoc-export 'docbook a s v b e t))

(defcustom org-pandoc-options-for-docx nil
  "Pandoc options for docx."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-docx (&optional a s v b e)
  (interactive) (org-pandoc-export 'docx a s v b e))

(defcustom org-pandoc-options-for-dzslides nil
  "Pandoc options for dzslides."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-dzslides (&optional a s v b e)
  (interactive) (org-pandoc-export 'dzslides a s v b e))

;;;###autoload
(defun org-pandoc-export-as-dzslides (&optional a s v b e)
  (interactive) (org-pandoc-export 'dzslides a s v b e t))

(defcustom org-pandoc-options-for-epub nil
  "Pandoc options for epub."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-epub (&optional a s v b e)
  (interactive) (org-pandoc-export 'epub a s v b e))

(defcustom org-pandoc-options-for-epub3 nil
  "Pandoc options for epub3."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-epub3 (&optional a s v b e)
  (interactive) (org-pandoc-export 'epub3 a s v b e))

(defcustom org-pandoc-options-for-fb2 nil
  "Pandoc options for fb2."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-fb2 (&optional a s v b e)
  (interactive) (org-pandoc-export 'fb2 a s v b e))

;;;###autoload
(defun org-pandoc-export-as-fb2 (&optional a s v b e)
  (interactive) (org-pandoc-export 'fb2 a s v b e t))

(defcustom org-pandoc-options-for-html nil
  "Pandoc options for html."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-html (&optional a s v b e)
  (interactive) (org-pandoc-export 'html a s v b e))

;;;###autoload
(defun org-pandoc-export-as-html (&optional a s v b e)
  (interactive) (org-pandoc-export 'html a s v b e t))

(defcustom org-pandoc-options-for-html5 nil
  "Pandoc options for html5."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-html5 (&optional a s v b e)
  (interactive) (org-pandoc-export 'html5 a s v b e))

;;;###autoload
(defun org-pandoc-export-as-html5 (&optional a s v b e)
  (interactive) (org-pandoc-export 'html5 a s v b e t))

(defcustom org-pandoc-options-for-icml nil
  "Pandoc options for icml."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-icml (&optional a s v b e)
  (interactive) (org-pandoc-export 'icml a s v b e))

;;;###autoload
(defun org-pandoc-export-as-icml (&optional a s v b e)
  (interactive) (org-pandoc-export 'icml a s v b e t))

(defcustom org-pandoc-options-for-json nil
  "Pandoc options for json."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-json (&optional a s v b e)
  (interactive) (org-pandoc-export 'json a s v b e))

;;;###autoload
(defun org-pandoc-export-as-json (&optional a s v b e)
  (interactive) (org-pandoc-export 'json a s v b e t))

(defcustom org-pandoc-options-for-latex nil
  "Pandoc options for latex."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-latex (&optional a s v b e)
  (interactive) (org-pandoc-export 'latex a s v b e))

;;;###autoload
(defun org-pandoc-export-as-latex (&optional a s v b e)
  (interactive) (org-pandoc-export 'latex a s v b e t))

(defcustom org-pandoc-options-for-man nil
  "Pandoc options for man."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-man (&optional a s v b e)
  (interactive) (org-pandoc-export 'man a s v b e))

;;;###autoload
(defun org-pandoc-export-as-man (&optional a s v b e)
  (interactive) (org-pandoc-export 'man a s v b e t))

(defcustom org-pandoc-options-for-markdown nil
  "Pandoc options for markdown."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-markdown (&optional a s v b e)
  (interactive) (org-pandoc-export 'markdown a s v b e))

;;;###autoload
(defun org-pandoc-export-as-markdown (&optional a s v b e)
  (interactive) (org-pandoc-export 'markdown a s v b e t))

(defcustom org-pandoc-options-for-markdown_github nil
  "Pandoc options for markdown_github."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-markdown_github (&optional a s v b e)
  (interactive) (org-pandoc-export 'markdown_github a s v b e))

;;;###autoload
(defun org-pandoc-export-as-markdown_github (&optional a s v b e)
  (interactive) (org-pandoc-export 'markdown_github a s v b e t))

(defcustom org-pandoc-options-for-markdown_mmd nil
  "Pandoc options for markdown_mmd."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-markdown_mmd (&optional a s v b e)
  (interactive) (org-pandoc-export 'markdown_mmd a s v b e))

;;;###autoload
(defun org-pandoc-export-as-markdown_mmd (&optional a s v b e)
  (interactive) (org-pandoc-export 'markdown_mmd a s v b e t))

(defcustom org-pandoc-options-for-markdown_phpextra nil
  "Pandoc options for markdown_phpextra."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-markdown_phpextra (&optional a s v b e)
  (interactive) (org-pandoc-export 'markdown_phpextra a s v b e))

;;;###autoload
(defun org-pandoc-export-as-markdown_phpextra (&optional a s v b e)
  (interactive) (org-pandoc-export 'markdown_phpextra a s v b e t))

(defcustom org-pandoc-options-for-markdown_strict nil
  "Pandoc options for markdown_strict."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-markdown_strict (&optional a s v b e)
  (interactive) (org-pandoc-export 'markdown_strict a s v b e))

;;;###autoload
(defun org-pandoc-export-as-markdown_strict (&optional a s v b e)
  (interactive) (org-pandoc-export 'markdown_strict a s v b e t))

(defcustom org-pandoc-options-for-mediawiki nil
  "Pandoc options for mediawiki."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-mediawiki (&optional a s v b e)
  (interactive) (org-pandoc-export 'mediawiki a s v b e))

;;;###autoload
(defun org-pandoc-export-as-mediawiki (&optional a s v b e)
  (interactive) (org-pandoc-export 'mediawiki a s v b e t))

(defcustom org-pandoc-options-for-native nil
  "Pandoc options for native."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-native (&optional a s v b e)
  (interactive) (org-pandoc-export 'native a s v b e))

;;;###autoload
(defun org-pandoc-export-as-native (&optional a s v b e)
  (interactive) (org-pandoc-export 'native a s v b e t))

(defcustom org-pandoc-options-for-odt nil
  "Pandoc options for odt."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-odt (&optional a s v b e)
  (interactive) (org-pandoc-export 'odt a s v b e))

;;;###autoload
(defun org-pandoc-export-as-odt (&optional a s v b e)
  (interactive) (org-pandoc-export 'odt a s v b e t))

(defcustom org-pandoc-options-for-opendocument nil
  "Pandoc options for opendocument."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-opendocument (&optional a s v b e)
  (interactive) (org-pandoc-export 'opendocument a s v b e))

(defcustom org-pandoc-options-for-opml nil
  "Pandoc options for opml."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-opml (&optional a s v b e)
  (interactive) (org-pandoc-export 'opml a s v b e))

;;;###autoload
(defun org-pandoc-export-as-opml (&optional a s v b e)
  (interactive) (org-pandoc-export 'opml a s v b e t))

(defcustom org-pandoc-options-for-org nil
  "Pandoc options for org."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-org (&optional a s v b e)
  (interactive) (org-pandoc-export 'org a s v b e))

;;;###autoload
(defun org-pandoc-export-as-org (&optional a s v b e)
  (interactive) (org-pandoc-export 'org a s v b e t))

(defcustom org-pandoc-options-for-plain nil
  "Pandoc options for plain."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-plain (&optional a s v b e)
  (interactive) (org-pandoc-export 'plain a s v b e))

;;;###autoload
(defun org-pandoc-export-as-plain (&optional a s v b e)
  (interactive) (org-pandoc-export 'plain a s v b e t))

(defcustom org-pandoc-options-for-revealjs nil
  "Pandoc options for revealjs."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-revealjs (&optional a s v b e)
  (interactive) (org-pandoc-export 'revealjs a s v b e))

;;;###autoload
(defun org-pandoc-export-as-revealjs (&optional a s v b e)
  (interactive) (org-pandoc-export 'revealjs a s v b e t))

(defcustom org-pandoc-options-for-rst nil
  "Pandoc options for rst."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-rst (&optional a s v b e)
  (interactive) (org-pandoc-export 'rst a s v b e))

;;;###autoload
(defun org-pandoc-export-as-rst (&optional a s v b e)
  (interactive) (org-pandoc-export 'rst a s v b e t))

(defcustom org-pandoc-options-for-rtf nil
  "Pandoc options for rtf."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-rtf (&optional a s v b e)
  (interactive) (org-pandoc-export 'rtf a s v b e))

;;;###autoload
(defun org-pandoc-export-as-rtf (&optional a s v b e)
  (interactive) (org-pandoc-export 'rtf a s v b e t))

(defcustom org-pandoc-options-for-s5 nil
  "Pandoc options for s5."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-s5 (&optional a s v b e)
  (interactive) (org-pandoc-export 's5 a s v b e))

;;;###autoload
(defun org-pandoc-export-as-s5 (&optional a s v b e)
  (interactive) (org-pandoc-export 's5 a s v b e t))

(defcustom org-pandoc-options-for-slideous nil
  "Pandoc options for slideous."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-slideous (&optional a s v b e)
  (interactive) (org-pandoc-export 'slideous a s v b e))

;;;###autoload
(defun org-pandoc-export-as-slideous (&optional a s v b e)
  (interactive) (org-pandoc-export 'slideous a s v b e t))

(defcustom org-pandoc-options-for-slidy nil
  "Pandoc options for slidy."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-slidy (&optional a s v b e)
  (interactive) (org-pandoc-export 'slidy a s v b e))

;;;###autoload
(defun org-pandoc-export-as-slidy (&optional a s v b e)
  (interactive) (org-pandoc-export 'slidy a s v b e t))

(defcustom org-pandoc-options-for-texinfo nil
  "Pandoc options for texinfo."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-texinfo (&optional a s v b e)
  (interactive) (org-pandoc-export 'texinfo a s v b e))

;;;###autoload
(defun org-pandoc-export-as-texinfo (&optional a s v b e)
  (interactive) (org-pandoc-export 'texinfo a s v b e t))

(defcustom org-pandoc-options-for-textile nil
  "Pandoc options for textile."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-textile (&optional a s v b e)
  (interactive) (org-pandoc-export 'textile a s v b e))

;;;###autoload
(defun org-pandoc-export-as-textile (&optional a s v b e)
  (interactive) (org-pandoc-export 'textile a s v b e t))


;;; OX-Pandoc Main routine

(defvar org-pandoc-format nil)
(defvar org-pandoc-option-table nil)
(defvar org-pandoc-epub-metadata nil)

(defun org-pandoc-export (format a s v b e &optional buf)
  "General interface for Pandoc Export."
  (unless (equal major-mode 'org-mode)
    (error "You must run this command in org-mode!"))
  (setq org-pandoc-format format)
  (org-export-to-file 'pandoc (make-temp-file "org-pandoc" nil ".org")
    a s v b e (lambda (f) (org-pandoc-run-to-buffer-or-file f format s buf))))

(defun org-pandoc-template (contents info)
  "Template processor for CONTENTS and INFO.
Option table is created in this stage."
  (setq org-pandoc-option-table (make-hash-table))
  ;; default options
  (org-pandoc-put-options org-pandoc-options)
  (org-pandoc-put-options
   (symbol-value (intern (format "org-pandoc-options-for-%s" org-pandoc-format))))
  ;; file options
  (-when-let (pandoc-options (plist-get info :pandoc-options))
    (org-pandoc-put-options
     (--map (let* ((split (split-string it ":"))
                   (name (intern (car split)))
                   (value (cadr split)))
              (cons name value))
            (split-string-and-unquote pandoc-options))))
  (setq org-pandoc-epub-metadata
        (concat
         (-when-let (epub-rights (or (plist-get info :epub-rights)
                                     org-pandoc-epub-rights))
           (concat "<dc:rights>" epub-rights "</dc:rights>\n"))
         (-when-let (description (plist-get info :description))
           (concat "<dc:description>" description "</dc:description>\n"))
         (-when-let (description (plist-get info :keywords))
           (concat "<dc:subject>" description "</dc:subject>\n"))))
  (dolist (epub-option '((epub-chapter-level . :epub-chapter-level)
                         (epub-cover-image . :epub-cover-image)
                         (epub-embed-font . :epub-embed-font)
                         (epub-metadata . :epub-metadata)
                         (epub-stylesheet . :epub-stylesheet)
                         (bibliography . :bibliography)))
    (-when-let (val (plist-get info (cdr epub-option)))
      (puthash (car epub-option) val org-pandoc-option-table)))
  contents)

(defun org-pandoc-put-options (options)
  (dolist (option options)
    (let ((name (car option))
          (value (cdr option)))
      (if (equal "t" value) (setq value t))
      (if (equal "nil" value) (setq value nil))
      (unless (memq name org-pandoc-valid-options)
        (error "Org-Pandoc: Improper Option Name! %s" name))
      (puthash name value org-pandoc-option-table))))

(defun org-pandoc-run-to-buffer-or-file
    (input-file format subtreep &optional bufferp)
  (let ((output-buffer-or-file
         (if bufferp (get-buffer-create "*Pandoc Output*")
           (org-export-output-file-name
            (concat "." (symbol-name
                         (assoc-default format org-pandoc-extensions nil format)))
            subtreep)))
        (metadata-file (make-temp-file "org-pandoc" nil ".xml")))
    (when (bufferp output-buffer-or-file)
      (with-current-buffer output-buffer-or-file (erase-buffer)))
    (when (and (or (equal org-pandoc-format 'epub) (equal org-pandoc-format 'epub3))
               (null (gethash 'epub-metadata org-pandoc-option-table)))
      (puthash 'epub-metadata metadata-file org-pandoc-option-table)
      (with-temp-file metadata-file
        (insert org-pandoc-epub-metadata)))
    (org-pandoc-run input-file output-buffer-or-file format org-pandoc-option-table)
    (delete-file input-file)
    (if (file-exists-p metadata-file) (delete-file metadata-file))
    (when (bufferp output-buffer-or-file)
      (pop-to-buffer output-buffer-or-file)
      (set-auto-mode))))

(defun org-pandoc-run (input-file buffer-or-file format &optional options)
  "Run pandoc command with INPUT-FILE (org), BUFFER-OR-FILE, FORMAT and OPTIONS.
If BUFFER-OR-FILE is buffer, then output to specified buffer.
OPTIONS is a hashtable."
  (let ((args
         `("-f" "org"
           "-t" ,(symbol-name format)
           ,@(unless (bufferp buffer-or-file)
               (list "-o" (expand-file-name buffer-or-file)))
           ,@(--mapcat
              (-when-let (val (gethash it options))
                (list
                 (concat "--" (symbol-name it)
                         (when (stringp val) (concat "=" val)))))
              (ht-keys options))
           ,(expand-file-name input-file))))
    (message "Running pandoc with args: %s" args)
    (let ((return
           (apply 'call-process
                  `(,org-pandoc-command nil ,(when (bufferp buffer-or-file)
                                               buffer-or-file)
                                        nil ,@args))))
      (if (/= 0 return) (message "Error occured.")
        (unless (bufferp buffer-or-file)
          (message "Exported to %s." buffer-or-file))))))

(provide 'ox-pandoc)

;;; ox-pandoc.el ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:

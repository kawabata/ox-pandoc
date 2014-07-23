;;; ox-pandoc.el --- org exporter for pandoc.        -*- lexical-binding: t; -*-

;; Copyright (C) 2014 KAWABATA, Taichi

;; Filename: ox-pandoc.el
;; Description: Another org exporter for Pandoc
;; Author: KAWABATA, Taichi <kawabata.taichi@gmail.com>
;; Created: 2014-07-20
;; Version: 1.140723
;; Package-Requires: ((org "8.2") (emacs "24") (dash "2.8") (ht "2.0"))
;; Keywords: tools
;; URL: https://github.com/kawabata/ox-pandoc

;;; Commentary:

;; * ox-pandoc
;;
;; This is another exporter for Org 8 that translates org-mode file to
;; various other formats via Pandoc (version 1.12.4 or later is needed).
;;
;; This file is inspired by [[https://github.com/robtillotson/org-pandoc][org-pandoc]], but entirely re-written.
;;
;; * Usage
;;
;; Exporters for following formats are available.
;;
;; | format            | buffer | file |
;; |-------------------+--------+------|
;; | asciidoc          | t      | t    |
;; | beamer            | t      | t    |
;; | beamer-pdf        | nil    | t    |
;; | context           | t      | t    |
;; | docbook           | t      | t    |
;; | docx              | nil    | t    |
;; | dzslides          | t      | t    |
;; | epub              | nil    | t    |
;; | epub3             | nil    | t    |
;; | fb2               | t      | t    |
;; | html              | t      | t    |
;; | html5             | t      | t    |
;; | icml              | t      | t    |
;; | json              | t      | t    |
;; | latex             | t      | t    |
;; | latex-pdf         | nil    | t    |
;; | man               | t      | t    |
;; | markdown          | t      | t    |
;; | markdown_github   | t      | t    |
;; | markdown_mmd      | t      | t    |
;; | markdown_phpextra | t      | t    |
;; | markdown_strict   | t      | t    |
;; | mediawiki         | t      | t    |
;; | native            | t      | t    |
;; | odt               | nil    | t    |
;; | opendocument      | nil    | t    |
;; | opml              | t      | t    |
;; | org               | t      | t    |
;; | plain             | t      | t    |
;; | revealjs          | t      | t    |
;; | rst               | t      | t    |
;; | rtf               | t      | t    |
;; | s5                | t      | t    |
;; | slideous          | t      | t    |
;; | slidy             | t      | t    |
;; | texinfo           | t      | t    |
;; | textile           | t      | t    |
;;
;; For example, for 'html5' format, two exporters are prepared.
;;
;; - =org-pandoc-export-as-html5= :: Exports the HTML text to a buffer.
;; - =org-pandoc-export-to-html5= :: Exports the HTML text to a file.
;; - =org-pandoc-export-to-html5-and-open= :: Export and open HTML file.
;;
;; * Customizations
;;
;; ** Option Variables
;;
;; - =org-pandoc-options= :: General Pandoc options.
;; - =org-pandoc-options-for-FORMAT= :: Format-specific options.
;;
;; Options should be specified by an alist. List of valid options are
;; defined in 'org-pandoc-valid-options'. Note that shortened form can
;; not be used.
;;
;; Document-specific options can be set to "#+PANDOC_OPTIONS:" in the
;; document. Latter options will override former options. Value 'nil'
;; overrides preceding option setting. Value 't' means only specify
;; option, but not its value.
;;
;; Following is an example.
;;
;; : ;; default options for all output formats
;; : (setq org-pandoc-options '((standalone . t)))
;; : ;; cancel above settings only for 'docx' format
;; : (setq org-pandoc-options-for-docx '((standalone . nil)))
;;
;; Each document can set its own customization variable.
;;
;; : # following will only apply to specific document.
;; : #+PANDOC_OPTIONS: standalone:t
;;
;; In PANDOC_OPTIONS specification, if option value includes space, you
;; can surround entire setting with quote. e.g.
;;
;; : #+PANDOC_OPTIONS: "epub-cover-image:/home/a/test file.png" standalone:nil
;;
;; ** Other Variables
;;
;; - =org-pandoc-after-processing-FORMAT-hook= :: Hook after processing
;;      FORMAT. This hook is only available to text-file FORMAT.
;;
;; - =org-pandoc-epub-right= :: EPUB Copyright Statement.
;;
;; ** In-File Options
;;
;; Followings are in-flie options. For EPUB outputs, various in-file
;; options can be used.
;;
;; - =PANDOC_OPTIONS= :: Add command line options to the Pandoc process.
;; - =EPUB_RIGHTS:= :: copyright info to be embedded to EPUB metadata.
;; - =EPUB_CHAPTER_LEVEL:= :: same as 'epub-chapter-level' pandoc-option.
;; - =EPUB_COVER:= :: same as 'epub-cover-image' pandoc-option.
;; - =EPUB_EMBED_FONT:= :: same as 'epub-embed-font' pandoc-option.
;; - =EPUB_METADATA:= :: same as 'epub-metadata' pandoc-option.
;; - =EPUB_STYLESHEET= :: same as 'epub-stylesheet' pandoc-option.
;; - =BIBLIOGRAPHY= :: same as 'bibliography' pandoc-option.
;;
;; ** Citation
;;
;; Currently, Pandoc citation key is expressed as /@key/, rather than
;; /{\cite key}/ . You may need some text converter if you want to use
;; Pandoc citation styles in org document.
;;
;; * Note
;;
;; This file creates and removes "XXXX.tmpZZZZ.org" temprary file in
;; working directory. (ZZZZ is random string.)

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

(defconst org-pandoc-file-options
  '(template include-in-header include-before-body include-after-body
    reference-odt reference-docx epub-stylesheet epub-cover-image
    epub-metadata epub-embed-font bibliography csl
    citation-abbreviations data-dir))

(defconst org-pandoc-extensions
  '((asciidoc . txt) (beamer . tex) (beamer-pdf . pdf) (context . tex)
    (dzslides . html) (epub3 . epub) (html5 . html) (latex . tex)
    (latex-pdf . pdf) (markdown . md) (markdown_github . md)
    (markdown_mmd . md) (markdown_strict . md) (native . hs)
    (opendocument . xml) (plain . txt) (revealjs . html) (s5 . html)
    (slideous . html) (slidy . html)))

(defconst org-pandoc-translate-output-format
  '((beamer-pdf . beamer) (latex-pdf . latex)))

(defcustom org-pandoc-options '((standalone . t)
                                (table-of-contents . t))
  "Pandoc options."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-command "pandoc"
  "Pandoc command."
  :group 'org-export-pandoc
  :type 'string)

(defcustom org-pandoc-menu-entry
  '(
    (?a "to asciidoc and open." org-pandoc-export-to-asciidoc-and-open)
    (?A "as asciidoc." org-pandoc-export-as-asciidoc)
    (?b "to beamer-pdf and open." org-pandoc-export-to-beamer-pdf-and-open)
    (?B "as beamer." org-pandoc-export-as-beamer)
    (?c "to context and open." org-pandoc-export-to-context-and-open)
    (?C "as context." org-pandoc-export-as-context)
    ;;(?d "to docbook and open." org-pandoc-export-to-docbook-and-open)
    ;;(?D "as docbook." org-pandoc-export-as-docbook)
    (?x "to docx and open." org-pandoc-export-to-docx-and-open)
    ;;(?z "to dzslides and open." org-pandoc-export-to-dzslides-and-open)
    (?Z "as dzslides." org-pandoc-export-as-dzslides)
    (?e "to epub and open." org-pandoc-export-to-epub-and-open)
    (?E "to epub3 and open." org-pandoc-export-to-epub3-and-open)
    ;;(?f "to fb2 and open." org-pandoc-export-to-fb2-and-open)
    ;;(?F "to fb2." org-pandoc-export-as-fb2)
    ;;(?g "to html and open." org-pandoc-export-to-html-and-open)
    ;;(?G "as html." org-pandoc-export-as-html)
    (?h "to html5 and open." org-pandoc-export-to-html5-and-open)
    (?H "to html5." org-pandoc-export-as-html5)
    ;;(?i "to icml and open." org-pandoc-export-to-icml-and-open)
    ;;(?I "as icml." org-pandoc-export-as-icml)
    (?j "to json and open." org-pandoc-export-to-json-and-open)
    (?J "as json." org-pandoc-export-as-json)
    (?l "to latex-pdf and open." org-pandoc-export-to-latex-pdf-and-open)
    (?L "as latex." org-pandoc-export-as-latex)
    (?1 "to man and open." org-pandoc-export-to-man-and-open)
    (?! "as man." org-pandoc-export-as-man)
    (?m "to markdown and open." org-pandoc-export-to-markdown-and-open)
    (?M "as markdown." org-pandoc-export-as-markdown)
    (?k "to markdown_github and open." org-pandoc-export-to-markdown_github-and-open)
    (?K "as markdown_github." org-pandoc-export-as-markdown_github)
    ;;(?m "to markdown_mmd and open." org-pandoc-export-to-markdown_mmd-and-open)
    ;;(?M "as markdown_mmd." org-pandoc-export-as-markdown_mmd)
    ;;(?m "to markdown_phpextra and open." org-pandoc-export-to-markdown_phpextra-and-open)
    ;;(?M "as markdown_phpextra." org-pandoc-export-as-markdown_phpextra)
    ;;(?m "to markdown_strict and open." org-pandoc-export-to-markdown_strict-and-open)
    ;;(?M "as markdown_strict." org-pandoc-export-as-markdown_strict)
    (?w "to mediawiki and open." org-pandoc-export-to-mediawiki-and-open)
    (?W "as mediawiki." org-pandoc-export-as-mediawiki)
    (?n "to native and open." org-pandoc-export-to-native-and-open)
    (?N "as native." org-pandoc-export-as-native)
    (?o "to odt and open." org-pandoc-export-to-odt-and-open)
    (?O "to opendocument and open." org-pandoc-export-to-opendocument-and-open)
    (?2 "to opml and open." org-pandoc-export-to-opml-and-open)
    (?@ "as opml." org-pandoc-export-as-opml)
    ;;(?3 "to org and open." org-pandoc-export-to-org-and-open)
    ;;(?# "as org." org-pandoc-export-as-org)
    ;;(?p "to plain and open." org-pandoc-export-to-plain-and-open)
    ;;(?P "as plain." org-pandoc-export-as-plain)
    (?v "to revealjs and open." org-pandoc-export-to-revealjs-and-open)
    (?V "as revealjs." org-pandoc-export-as-revealjs)
    ;;(?r "to rst and open." org-pandoc-export-to-rst-and-open)
    ;;(?R "as rst." org-pandoc-export-as-rst)
    (?4 "to rtf and open." org-pandoc-export-to-rtf-and-open)
    (?$ "as rtf." org-pandoc-export-as-rtf)
    (?s "to s5 and open." org-pandoc-export-to-s5-and-open)
    (?S "to s5." org-pandoc-export-as-s5)
    (?5 "to slideous and open." org-pandoc-export-to-slideous-and-open)
    (?% "as slideous." org-pandoc-export-as-slideous)
    (?6 "to slidy and open." org-pandoc-export-to-slidy-and-open)
    (?^ "as slidy." org-pandoc-export-as-slidy)
    ;;(?t "to texinfo and open." org-pandoc-export-to-texinfo-and-open)
    ;;(?T "as texinfo." org-pandoc-export-as-texinfo)
    ;;(?v "to textile and open." org-pandoc-export-to-textile-and-open)
    ;;(?V "as textile." org-pandoc-export-as-textile)
    )
  "Pandoc menu-entry."
  :group 'org-export-pandoc
  :type 'list)

(org-export-define-derived-backend 'pandoc 'org
  :translate-alist '((template . org-pandoc-template))
  :export-block "PANDOC"
  :menu-entry
  `(?p "export via pandoc"
       ,org-pandoc-menu-entry)
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

(defcustom org-pandoc-after-processing-asciidoc-hook nil
  "Hook called after processing asciidoc."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-asciidoc (&optional a s v b e)
  "Export to asciidoc."
  (interactive) (org-pandoc-export 'asciidoc a s v b e))

;;;###autoload
(defun org-pandoc-export-to-asciidoc-and-open (&optional a s v b e)
  "Export to asciidoc and open."
  (interactive) (org-pandoc-export 'asciidoc a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-asciidoc (&optional a s v b e)
  "Export as asciidoc."
  (interactive) (org-pandoc-export 'asciidoc a s v b e t))

(defcustom org-pandoc-options-for-beamer nil
  "Pandoc options for beamer."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-beamer-hook nil
  "Hook called after processing beamer."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-beamer (&optional a s v b e)
  "Export to beamer."
  (interactive) (org-pandoc-export 'beamer a s v b e))

;;;###autoload
(defun org-pandoc-export-to-beamer-and-open (&optional a s v b e)
  "Export to beamer and open."
  (interactive) (org-pandoc-export 'beamer a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-beamer (&optional a s v b e)
  "Export as beamer."
  (interactive) (org-pandoc-export 'beamer a s v b e t))

(defcustom org-pandoc-options-for-beamer-pdf nil
  "Pandoc options for beamer-pdf."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-beamer-pdf (&optional a s v b e)
  "Export to beamer-pdf."
  (interactive) (org-pandoc-export 'beamer-pdf a s v b e))

;;;###autoload
(defun org-pandoc-export-to-beamer-pdf-and-open (&optional a s v b e)
  "Export to beamer-pdf and open."
  (interactive) (org-pandoc-export 'beamer-pdf a s v b e 0))

(defcustom org-pandoc-options-for-context nil
  "Pandoc options for context."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-context-hook nil
  "Hook called after processing context."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-context (&optional a s v b e)
  "Export to context."
  (interactive) (org-pandoc-export 'context a s v b e))

;;;###autoload
(defun org-pandoc-export-to-context-and-open (&optional a s v b e)
  "Export to context and open."
  (interactive) (org-pandoc-export 'context a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-context (&optional a s v b e)
  "Export as context."
  (interactive) (org-pandoc-export 'context a s v b e t))

(defcustom org-pandoc-options-for-docbook nil
  "Pandoc options for docbook."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-docbook-hook nil
  "Hook called after processing docbook."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-docbook (&optional a s v b e)
  "Export to docbook."
  (interactive) (org-pandoc-export 'docbook a s v b e))

;;;###autoload
(defun org-pandoc-export-to-docbook-and-open (&optional a s v b e)
  "Export to docbook and open."
  (interactive) (org-pandoc-export 'docbook a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-docbook (&optional a s v b e)
  "Export as docbook."
  (interactive) (org-pandoc-export 'docbook a s v b e t))

(defcustom org-pandoc-options-for-docx nil
  "Pandoc options for docx."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-docx (&optional a s v b e)
  "Export to docx."
  (interactive) (org-pandoc-export 'docx a s v b e))

;;;###autoload
(defun org-pandoc-export-to-docx-and-open (&optional a s v b e)
  "Export to docx and open."
  (interactive) (org-pandoc-export 'docx a s v b e 0))

(defcustom org-pandoc-options-for-dzslides nil
  "Pandoc options for dzslides."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-dzslides-hook nil
  "Hook called after processing dzslides."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-dzslides (&optional a s v b e)
  "Export to dzslides."
  (interactive) (org-pandoc-export 'dzslides a s v b e))

;;;###autoload
(defun org-pandoc-export-to-dzslides-and-open (&optional a s v b e)
  "Export to dzslides and open."
  (interactive) (org-pandoc-export 'dzslides a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-dzslides (&optional a s v b e)
  "Export as dzslides."
  (interactive) (org-pandoc-export 'dzslides a s v b e t))

(defcustom org-pandoc-options-for-epub nil
  "Pandoc options for epub."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-epub (&optional a s v b e)
  "Export to epub."
  (interactive) (org-pandoc-export 'epub a s v b e))

;;;###autoload
(defun org-pandoc-export-to-epub-and-open (&optional a s v b e)
  "Export to epub and open."
  (interactive) (org-pandoc-export 'epub a s v b e 0))

(defcustom org-pandoc-options-for-epub3 nil
  "Pandoc options for epub3."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-epub3 (&optional a s v b e)
  "Export to epub3."
  (interactive) (org-pandoc-export 'epub3 a s v b e))

;;;###autoload
(defun org-pandoc-export-to-epub3-and-open (&optional a s v b e)
  "Export to epub3 and open."
  (interactive) (org-pandoc-export 'epub3 a s v b e 0))

(defcustom org-pandoc-options-for-fb2 nil
  "Pandoc options for fb2."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-fb2-hook nil
  "Hook called after processing fb2."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-fb2 (&optional a s v b e)
  "Export to fb2."
  (interactive) (org-pandoc-export 'fb2 a s v b e))

;;;###autoload
(defun org-pandoc-export-to-fb2-and-open (&optional a s v b e)
  "Export to fb2 and open."
  (interactive) (org-pandoc-export 'fb2 a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-fb2 (&optional a s v b e)
  "Export as fb2."
  (interactive) (org-pandoc-export 'fb2 a s v b e t))

(defcustom org-pandoc-options-for-html nil
  "Pandoc options for html."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-html-hook nil
  "Hook called after processing html."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-html (&optional a s v b e)
  "Export to html."
  (interactive) (org-pandoc-export 'html a s v b e))

;;;###autoload
(defun org-pandoc-export-to-html-and-open (&optional a s v b e)
  "Export to html and open."
  (interactive) (org-pandoc-export 'html a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-html (&optional a s v b e)
  "Export as html."
  (interactive) (org-pandoc-export 'html a s v b e t))

(defcustom org-pandoc-options-for-html5 nil
  "Pandoc options for html5."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-html5-hook nil
  "Hook called after processing html5."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-html5 (&optional a s v b e)
  "Export to html5."
  (interactive) (org-pandoc-export 'html5 a s v b e))

;;;###autoload
(defun org-pandoc-export-to-html5-and-open (&optional a s v b e)
  "Export to html5 and open."
  (interactive) (org-pandoc-export 'html5 a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-html5 (&optional a s v b e)
  "Export as html5."
  (interactive) (org-pandoc-export 'html5 a s v b e t))

(defcustom org-pandoc-options-for-icml nil
  "Pandoc options for icml."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-icml-hook nil
  "Hook called after processing icml."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-icml (&optional a s v b e)
  "Export to icml."
  (interactive) (org-pandoc-export 'icml a s v b e))

;;;###autoload
(defun org-pandoc-export-to-icml-and-open (&optional a s v b e)
  "Export to icml and open."
  (interactive) (org-pandoc-export 'icml a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-icml (&optional a s v b e)
  "Export as icml."
  (interactive) (org-pandoc-export 'icml a s v b e t))

(defcustom org-pandoc-options-for-json nil
  "Pandoc options for json."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-json-hook nil
  "Hook called after processing json."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-json (&optional a s v b e)
  "Export to json."
  (interactive) (org-pandoc-export 'json a s v b e))

;;;###autoload
(defun org-pandoc-export-to-json-and-open (&optional a s v b e)
  "Export to json and open."
  (interactive) (org-pandoc-export 'json a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-json (&optional a s v b e)
  "Export as json."
  (interactive) (org-pandoc-export 'json a s v b e t))

(defcustom org-pandoc-options-for-latex nil
  "Pandoc options for latex."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-latex-hook nil
  "Hook called after processing latex."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-latex (&optional a s v b e)
  "Export to latex."
  (interactive) (org-pandoc-export 'latex a s v b e))

;;;###autoload
(defun org-pandoc-export-to-latex-and-open (&optional a s v b e)
  "Export to latex and open."
  (interactive) (org-pandoc-export 'latex a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-latex (&optional a s v b e)
  "Export as latex."
  (interactive) (org-pandoc-export 'latex a s v b e t))

(defcustom org-pandoc-options-for-latex-pdf nil
  "Pandoc options for latex-pdf."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-latex-pdf (&optional a s v b e)
  "Export to latex-pdf."
  (interactive) (org-pandoc-export 'latex-pdf a s v b e))

;;;###autoload
(defun org-pandoc-export-to-latex-pdf-and-open (&optional a s v b e)
  "Export to latex-pdf and open."
  (interactive) (org-pandoc-export 'latex-pdf a s v b e 0))

(defcustom org-pandoc-options-for-man nil
  "Pandoc options for man."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-man-hook nil
  "Hook called after processing man."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-man (&optional a s v b e)
  "Export to man."
  (interactive) (org-pandoc-export 'man a s v b e))

;;;###autoload
(defun org-pandoc-export-to-man-and-open (&optional a s v b e)
  "Export to man and open."
  (interactive) (org-pandoc-export 'man a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-man (&optional a s v b e)
  "Export as man."
  (interactive) (org-pandoc-export 'man a s v b e t))

(defcustom org-pandoc-options-for-markdown nil
  "Pandoc options for markdown."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-markdown-hook nil
  "Hook called after processing markdown."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-markdown (&optional a s v b e)
  "Export to markdown."
  (interactive) (org-pandoc-export 'markdown a s v b e))

;;;###autoload
(defun org-pandoc-export-to-markdown-and-open (&optional a s v b e)
  "Export to markdown and open."
  (interactive) (org-pandoc-export 'markdown a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-markdown (&optional a s v b e)
  "Export as markdown."
  (interactive) (org-pandoc-export 'markdown a s v b e t))

(defcustom org-pandoc-options-for-markdown_github nil
  "Pandoc options for markdown_github."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-markdown_github-hook nil
  "Hook called after processing markdown_github."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-markdown_github (&optional a s v b e)
  "Export to markdown_github."
  (interactive) (org-pandoc-export 'markdown_github a s v b e))

;;;###autoload
(defun org-pandoc-export-to-markdown_github-and-open (&optional a s v b e)
  "Export to markdown_github and open."
  (interactive) (org-pandoc-export 'markdown_github a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-markdown_github (&optional a s v b e)
  "Export as markdown_github."
  (interactive) (org-pandoc-export 'markdown_github a s v b e t))

(defcustom org-pandoc-options-for-markdown_mmd nil
  "Pandoc options for markdown_mmd."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-markdown_mmd-hook nil
  "Hook called after processing markdown_mmd."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-markdown_mmd (&optional a s v b e)
  "Export to markdown_mmd."
  (interactive) (org-pandoc-export 'markdown_mmd a s v b e))

;;;###autoload
(defun org-pandoc-export-to-markdown_mmd-and-open (&optional a s v b e)
  "Export to markdown_mmd and open."
  (interactive) (org-pandoc-export 'markdown_mmd a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-markdown_mmd (&optional a s v b e)
  "Export as markdown_mmd."
  (interactive) (org-pandoc-export 'markdown_mmd a s v b e t))

(defcustom org-pandoc-options-for-markdown_phpextra nil
  "Pandoc options for markdown_phpextra."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-markdown_phpextra-hook nil
  "Hook called after processing markdown_phpextra."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-markdown_phpextra (&optional a s v b e)
  "Export to markdown_phpextra."
  (interactive) (org-pandoc-export 'markdown_phpextra a s v b e))

;;;###autoload
(defun org-pandoc-export-to-markdown_phpextra-and-open (&optional a s v b e)
  "Export to markdown_phpextra and open."
  (interactive) (org-pandoc-export 'markdown_phpextra a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-markdown_phpextra (&optional a s v b e)
  "Export as markdown_phpextra."
  (interactive) (org-pandoc-export 'markdown_phpextra a s v b e t))

(defcustom org-pandoc-options-for-markdown_strict nil
  "Pandoc options for markdown_strict."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-markdown_strict-hook nil
  "Hook called after processing markdown_strict."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-markdown_strict (&optional a s v b e)
  "Export to markdown_strict."
  (interactive) (org-pandoc-export 'markdown_strict a s v b e))

;;;###autoload
(defun org-pandoc-export-to-markdown_strict-and-open (&optional a s v b e)
  "Export to markdown_strict and open."
  (interactive) (org-pandoc-export 'markdown_strict a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-markdown_strict (&optional a s v b e)
  "Export as markdown_strict."
  (interactive) (org-pandoc-export 'markdown_strict a s v b e t))

(defcustom org-pandoc-options-for-mediawiki nil
  "Pandoc options for mediawiki."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-mediawiki-hook nil
  "Hook called after processing mediawiki."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-mediawiki (&optional a s v b e)
  "Export to mediawiki."
  (interactive) (org-pandoc-export 'mediawiki a s v b e))

;;;###autoload
(defun org-pandoc-export-to-mediawiki-and-open (&optional a s v b e)
  "Export to mediawiki and open."
  (interactive) (org-pandoc-export 'mediawiki a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-mediawiki (&optional a s v b e)
  "Export as mediawiki."
  (interactive) (org-pandoc-export 'mediawiki a s v b e t))

(defcustom org-pandoc-options-for-native nil
  "Pandoc options for native."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-native-hook nil
  "Hook called after processing native."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-native (&optional a s v b e)
  "Export to native."
  (interactive) (org-pandoc-export 'native a s v b e))

;;;###autoload
(defun org-pandoc-export-to-native-and-open (&optional a s v b e)
  "Export to native and open."
  (interactive) (org-pandoc-export 'native a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-native (&optional a s v b e)
  "Export as native."
  (interactive) (org-pandoc-export 'native a s v b e t))

(defcustom org-pandoc-options-for-odt nil
  "Pandoc options for odt."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-odt (&optional a s v b e)
  "Export to odt."
  (interactive) (org-pandoc-export 'odt a s v b e))

;;;###autoload
(defun org-pandoc-export-to-odt-and-open (&optional a s v b e)
  "Export to odt and open."
  (interactive) (org-pandoc-export 'odt a s v b e 0))

(defcustom org-pandoc-options-for-opendocument nil
  "Pandoc options for opendocument."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-opendocument (&optional a s v b e)
  "Export to opendocument."
  (interactive) (org-pandoc-export 'opendocument a s v b e))

;;;###autoload
(defun org-pandoc-export-to-opendocument-and-open (&optional a s v b e)
  "Export to opendocument and open."
  (interactive) (org-pandoc-export 'opendocument a s v b e 0))

(defcustom org-pandoc-options-for-opml nil
  "Pandoc options for opml."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-opml-hook nil
  "Hook called after processing opml."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-opml (&optional a s v b e)
  "Export to opml."
  (interactive) (org-pandoc-export 'opml a s v b e))

;;;###autoload
(defun org-pandoc-export-to-opml-and-open (&optional a s v b e)
  "Export to opml and open."
  (interactive) (org-pandoc-export 'opml a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-opml (&optional a s v b e)
  "Export as opml."
  (interactive) (org-pandoc-export 'opml a s v b e t))

(defcustom org-pandoc-options-for-org nil
  "Pandoc options for org."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-org-hook nil
  "Hook called after processing org."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-org (&optional a s v b e)
  "Export to org."
  (interactive) (org-pandoc-export 'org a s v b e))

;;;###autoload
(defun org-pandoc-export-to-org-and-open (&optional a s v b e)
  "Export to org and open."
  (interactive) (org-pandoc-export 'org a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-org (&optional a s v b e)
  "Export as org."
  (interactive) (org-pandoc-export 'org a s v b e t))

(defcustom org-pandoc-options-for-plain nil
  "Pandoc options for plain."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-plain-hook nil
  "Hook called after processing plain."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-plain (&optional a s v b e)
  "Export to plain."
  (interactive) (org-pandoc-export 'plain a s v b e))

;;;###autoload
(defun org-pandoc-export-to-plain-and-open (&optional a s v b e)
  "Export to plain and open."
  (interactive) (org-pandoc-export 'plain a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-plain (&optional a s v b e)
  "Export as plain."
  (interactive) (org-pandoc-export 'plain a s v b e t))

(defcustom org-pandoc-options-for-revealjs nil
  "Pandoc options for revealjs."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-revealjs-hook nil
  "Hook called after processing revealjs."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-revealjs (&optional a s v b e)
  "Export to revealjs."
  (interactive) (org-pandoc-export 'revealjs a s v b e))

;;;###autoload
(defun org-pandoc-export-to-revealjs-and-open (&optional a s v b e)
  "Export to revealjs and open."
  (interactive) (org-pandoc-export 'revealjs a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-revealjs (&optional a s v b e)
  "Export as revealjs."
  (interactive) (org-pandoc-export 'revealjs a s v b e t))

(defcustom org-pandoc-options-for-rst nil
  "Pandoc options for rst."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-rst-hook nil
  "Hook called after processing rst."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-rst (&optional a s v b e)
  "Export to rst."
  (interactive) (org-pandoc-export 'rst a s v b e))

;;;###autoload
(defun org-pandoc-export-to-rst-and-open (&optional a s v b e)
  "Export to rst and open."
  (interactive) (org-pandoc-export 'rst a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-rst (&optional a s v b e)
  "Export as rst."
  (interactive) (org-pandoc-export 'rst a s v b e t))

(defcustom org-pandoc-options-for-rtf nil
  "Pandoc options for rtf."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-rtf-hook nil
  "Hook called after processing rtf."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-rtf (&optional a s v b e)
  "Export to rtf."
  (interactive) (org-pandoc-export 'rtf a s v b e))

;;;###autoload
(defun org-pandoc-export-to-rtf-and-open (&optional a s v b e)
  "Export to rtf and open."
  (interactive) (org-pandoc-export 'rtf a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-rtf (&optional a s v b e)
  "Export as rtf."
  (interactive) (org-pandoc-export 'rtf a s v b e t))

(defcustom org-pandoc-options-for-s5 nil
  "Pandoc options for s5."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-s5-hook nil
  "Hook called after processing s5."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-s5 (&optional a s v b e)
  "Export to s5."
  (interactive) (org-pandoc-export 's5 a s v b e))

;;;###autoload
(defun org-pandoc-export-to-s5-and-open (&optional a s v b e)
  "Export to s5 and open."
  (interactive) (org-pandoc-export 's5 a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-s5 (&optional a s v b e)
  "Export as s5."
  (interactive) (org-pandoc-export 's5 a s v b e t))

(defcustom org-pandoc-options-for-slideous nil
  "Pandoc options for slideous."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-slideous-hook nil
  "Hook called after processing slideous."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-slideous (&optional a s v b e)
  "Export to slideous."
  (interactive) (org-pandoc-export 'slideous a s v b e))

;;;###autoload
(defun org-pandoc-export-to-slideous-and-open (&optional a s v b e)
  "Export to slideous and open."
  (interactive) (org-pandoc-export 'slideous a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-slideous (&optional a s v b e)
  "Export as slideous."
  (interactive) (org-pandoc-export 'slideous a s v b e t))

(defcustom org-pandoc-options-for-slidy nil
  "Pandoc options for slidy."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-slidy-hook nil
  "Hook called after processing slidy."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-slidy (&optional a s v b e)
  "Export to slidy."
  (interactive) (org-pandoc-export 'slidy a s v b e))

;;;###autoload
(defun org-pandoc-export-to-slidy-and-open (&optional a s v b e)
  "Export to slidy and open."
  (interactive) (org-pandoc-export 'slidy a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-slidy (&optional a s v b e)
  "Export as slidy."
  (interactive) (org-pandoc-export 'slidy a s v b e t))

(defcustom org-pandoc-options-for-texinfo nil
  "Pandoc options for texinfo."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-texinfo-hook nil
  "Hook called after processing texinfo."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-texinfo (&optional a s v b e)
  "Export to texinfo."
  (interactive) (org-pandoc-export 'texinfo a s v b e))

;;;###autoload
(defun org-pandoc-export-to-texinfo-and-open (&optional a s v b e)
  "Export to texinfo and open."
  (interactive) (org-pandoc-export 'texinfo a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-texinfo (&optional a s v b e)
  "Export as texinfo."
  (interactive) (org-pandoc-export 'texinfo a s v b e t))

(defcustom org-pandoc-options-for-textile nil
  "Pandoc options for textile."
  :group 'org-export-pandoc
  :type 'list)

(defcustom org-pandoc-after-processing-textile-hook nil
  "Hook called after processing textile."
  :group 'org-export-pandoc
  :type 'list)

;;;###autoload
(defun org-pandoc-export-to-textile (&optional a s v b e)
  "Export to textile."
  (interactive) (org-pandoc-export 'textile a s v b e))

;;;###autoload
(defun org-pandoc-export-to-textile-and-open (&optional a s v b e)
  "Export to textile and open."
  (interactive) (org-pandoc-export 'textile a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-textile (&optional a s v b e)
  "Export as textile."
  (interactive) (org-pandoc-export 'textile a s v b e t))


;;; ox-pandoc main routine

(defvar org-pandoc-format nil)
(defvar org-pandoc-option-table nil)
(defvar org-pandoc-epub-metadata nil)

(defun org-pandoc-export (format a s v b e &optional buf-or-open)
  "General interface for Pandoc Export.
If BUF-OR-OPEN is nil, output to file.  0, then open the file.
t means output to buffer."
  (unless (equal major-mode 'org-mode)
    (error "You must run this command in org-mode."))
  (unless (executable-find org-pandoc-command)
    (error "Pandoc (version 1.12.4 or later) can not be found."))
  (setq org-pandoc-format format)
  (org-export-to-file 'pandoc (org-export-output-file-name
                               (concat (make-temp-name ".tmp") ".org") s)
    a s v b e (lambda (f) (org-pandoc-run-to-buffer-or-file f format s buf-or-open))))

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
  (org-pandoc-put-options
   (--mapcat (-when-let (val (plist-get info (cdr it)))
               (list (cons (car it) val)))
             '((epub-chapter-level . :epub-chapter-level)
               (epub-cover-image   . :epub-cover-image)
               (epub-embed-font    . :epub-embed-font)
               (epub-metadata      . :epub-metadata)
               (epub-stylesheet    . :epub-stylesheet)
               (bibliography       . :bibliography))))
  contents)

(defun org-pandoc-put-options (options)
  (dolist (option options)
    (let ((name (car option))
          (value (cdr option)))
      (if (equal "t" value) (setq value t))
      (if (equal "nil" value) (setq value nil))
      (unless (memq name org-pandoc-valid-options)
        (error "Org-Pandoc: Improper Option Name! %s" name))
      (if (memq name org-pandoc-file-options)
          (setq value (expand-file-name value)))
      (puthash name value org-pandoc-option-table))))

(defun org-pandoc-run-to-buffer-or-file
    (input-file format subtreep &optional buffer-or-open)
  (let ((output-buffer-or-file
         (if (equal t buffer-or-open) (get-buffer-create "*Pandoc Output*")
           (org-export-output-file-name
            (concat "." (symbol-name
                         (or (assoc-default format org-pandoc-extensions)
                             format)))
            subtreep)))
        (metadata-file (make-temp-file "org-pandoc" nil ".xml"))
        (local-hook-symbol (intern (format "org-pandoc-after-processing-%s-hook"
                                           format))))
    (when (bufferp output-buffer-or-file)
      (with-current-buffer output-buffer-or-file (erase-buffer)))
    (when (and (or (equal org-pandoc-format 'epub) (equal org-pandoc-format 'epub3))
               (null (gethash 'epub-metadata org-pandoc-option-table)))
      (puthash 'epub-metadata metadata-file org-pandoc-option-table)
      (with-temp-file metadata-file
        (insert org-pandoc-epub-metadata)))
    (org-pandoc-run input-file output-buffer-or-file format
                    org-pandoc-option-table)
    (delete-file input-file)
    (if (file-exists-p metadata-file) (delete-file metadata-file))
    (when (bufferp output-buffer-or-file)
      (pop-to-buffer output-buffer-or-file)
      (run-hooks local-hook-symbol)
      (set-auto-mode))
    (if (and (not (bufferp output-buffer-or-file))
             (boundp local-hook-symbol)
             (symbol-value local-hook-symbol))
        (with-temp-file output-buffer-or-file
          (insert-file-contents output-buffer-or-file)
          (run-hooks local-hook-symbol)))
    (if (equal 0 buffer-or-open) (org-open-file
                                  output-buffer-or-file))))

(defun org-pandoc-run (input-file buffer-or-file format &optional options)
  "Run pandoc command with INPUT-FILE (org), BUFFER-OR-FILE, FORMAT and OPTIONS.
If BUFFER-OR-FILE is buffer, then output to specified buffer.
OPTIONS is a hashtable."
  (let ((args
         `("-f" "org"
           "-t" ,(symbol-name
                  (or (assoc-default format org-pandoc-translate-output-format)
                      format))
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

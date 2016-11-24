;;; ox-pandoc.el --- org exporter for pandoc.        -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2015 KAWABATA, Taichi

;; Filename: ox-pandoc.el
;; Description: Another org exporter for Pandoc
;; Author: KAWABATA, Taichi <kawabata.taichi@gmail.com>
;; Created: 2014-07-20
;; Version: 1.160629
;; Package-Requires: ((org "8.2") (emacs "24") (dash "2.8") (ht "2.0"))
;; Keywords: tools
;; URL: https://github.com/kawabata/ox-pandoc

;;; Commentary:

;; This is another exporter for org-mode that translates Org-mode file
;; to various other formats via Pandoc.  You need org-mode version 8.2
;; or later, and Pandoc 1.13 or later, to use this package.  For
;; details, please refer https://github.com/kawabata/ox-pandoc.

;;; Code:

(require 'ox-org)
(require 'dash)
(require 'ht)

(defgroup org-pandoc nil
  "Options specific to Pandoc export back-end."
  :tag "Org Pandoc"
  :group 'org-export)

(defconst org-pandoc-valid-options
  '(ascii atx-headers base-header-level biblatex bibliography chapters
    citation-abbreviations columns csl css data-dir
    default-image-extension dump-args email-obfuscation
    epub-chapter-level epub-cover-image epub-embed-font epub-metadata
    epub-stylesheet filter gladtex highlight-style html-q-tags html5
    id-prefix ignore-args include-after-body include-before-body
    include-in-header incremental indented-code-classes jsmath
    latex-engine latex-engine-opt latexmathml listings mathjax mathml
    metadata mimetex natbib no-highlight no-tex-ligatures no-wrap
    normalize number-offset number-sections offline old-dashes output
    parse-raw preserve-tabs read reference-docx reference-links
    reference-odt section-divs self-contained slide-level smart
    standalone tab-stop table-of-contents template title-prefix to
    toc-depth variable verbose version webtex))

(defconst org-pandoc-colon-separated-options
  '(include-in-header include-before-body include-after-body css
    epub-embed-font bibliography filter))

(defconst org-pandoc-file-options
  '(template include-in-header include-before-body include-after-body
    reference-odt reference-docx epub-stylesheet epub-cover-image
    epub-metadata epub-embed-font bibliography csl
    citation-abbreviations data-dir))

(defconst org-pandoc-extensions
  '((asciidoc . txt) (beamer . tex) (beamer-pdf . pdf)
    (commonmark . md) (context . tex)
    (docbook . dbk) (docbook5 . dbk) (dokuwiki . doku)
    (dzslides . html) (epub3 . epub)
    (haddock . hs) (html5 . html) (latex . tex)
    (latex-pdf . pdf) (markdown . md) (markdown_github . md)
    (markdown_mmd . md) (markdown_strict . md) (native . hs)
    (opendocument . xml) (plain . txt) (revealjs . html) (s5 . html)
    (slideous . html) (slidy . html) (texinfo . texi)))

(defconst org-pandoc-translate-output-format
  '((beamer-pdf . beamer) (latex-pdf . latex)))

(defconst org-pandoc-option-type
  `(choice (const t) (const nil)
           (alist :key-type (choice ,@(--map `(const ,it) org-pandoc-valid-options))
                  :value-type (choice (const t) (const nil) string))))

(defcustom org-pandoc-options '((standalone . t) (mathjax . t) (parse-raw . t))
  "Pandoc options."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-command "pandoc"
  "Pandoc command."
  :group 'org-pandoc
  :type 'string)

(defcustom org-pandoc-menu-entry
  '(
    ;;(?a "to asciidoc." org-pandoc-export-to-asciidoc)
    (?a "to asciidoc and open." org-pandoc-export-to-asciidoc-and-open)
    (?A "as asciidoc." org-pandoc-export-as-asciidoc)
    ;;(?1 "to beamer." org-pandoc-export-to-beamer)
    (?1 "to beamer and open." org-pandoc-export-to-beamer-and-open)
    (?! "as beamer." org-pandoc-export-as-beamer)
    (?b "to beamer-pdf and open." org-pandoc-export-to-beamer-pdf-and-open)
    (?B "to beamer-pdf." org-pandoc-export-to-beamer-pdf)
    ;;(?q "to commonmark." org-pandoc-export-to-commonmark)
    (?q "to commonmark and open." org-pandoc-export-to-commonmark-and-open)
    (?Q "as commonmark." org-pandoc-export-as-commonmark)
    ;;(?c "to context." org-pandoc-export-to-context)
    (?c "to context and open." org-pandoc-export-to-context-and-open)
    (?C "as context." org-pandoc-export-as-context)
    ;;(?d "to docbook." org-pandoc-export-to-docbook)
    ;;(?d "to docbook and open." org-pandoc-export-to-docbook-and-open)
    ;;(?D "as docbook." org-pandoc-export-as-docbook)
    ;;(?d "to docbook." org-pandoc-export-to-docbook5)
    (?d "to docbook and open." org-pandoc-export-to-docbook5-and-open)
    (?D "as docbook." org-pandoc-export-as-docbook5)
    (?x "to docx and open." org-pandoc-export-to-docx-and-open)
    (?X "to docx." org-pandoc-export-to-docx)
    ;;(?u "to dokuwiki." org-pandoc-export-to-dokuwiki)
    (?u "to dokuwiki and open." org-pandoc-export-to-dokuwiki-and-open)
    (?U "as dokuwiki." org-pandoc-export-as-dokuwiki)
    ;;(?z "to dzslides." org-pandoc-export-to-dzslides)
    (?z "to dzslides and open." org-pandoc-export-to-dzslides-and-open)
    (?Z "as dzslides." org-pandoc-export-as-dzslides)
    (?3 "to epub and open." org-pandoc-export-to-epub-and-open)
    (?# "to epub." org-pandoc-export-to-epub)
    (?e "to epub3 and open." org-pandoc-export-to-epub3-and-open)
    (?E "to epub3." org-pandoc-export-to-epub3)
    ;;(?f "to fb2." org-pandoc-export-to-fb2)
    (?f "to fb2 and open." org-pandoc-export-to-fb2-and-open)
    (?F "as fb2." org-pandoc-export-as-fb2)
    ;;(?~ "to haddock." org-pandoc-export-to-haddock)
    (?~ "to haddock and open." org-pandoc-export-to-haddock-and-open)
    (?^ "as haddock." org-pandoc-export-as-haddock)
    ;;(?5 "to html." org-pandoc-export-to-html)
    (?5 "to html and open." org-pandoc-export-to-html-and-open)
    (?% "as html." org-pandoc-export-as-html)
    ;;(?h "to html5." org-pandoc-export-to-html5)
    (?h "to html5 and open." org-pandoc-export-to-html5-and-open)
    (?H "as html5." org-pandoc-export-as-html5)
    ;;(?i "to icml." org-pandoc-export-to-icml)
    (?i "to icml and open." org-pandoc-export-to-icml-and-open)
    (?I "as icml." org-pandoc-export-as-icml)
    ;;(?j "to json." org-pandoc-export-to-json)
    (?j "to json and open." org-pandoc-export-to-json-and-open)
    (?J "as json." org-pandoc-export-as-json)
    ;;(?= "to latex." org-pandoc-export-to-latex)
    (?= "to latex and open." org-pandoc-export-to-latex-and-open)
    (?- "as latex." org-pandoc-export-as-latex)
    (?l "to latex-pdf and open." org-pandoc-export-to-latex-pdf-and-open)
    (?L "to latex-pdf." org-pandoc-export-to-latex-pdf)
    ;;(?m "to man." org-pandoc-export-to-man)
    (?m "to man and open." org-pandoc-export-to-man-and-open)
    (?M "as man." org-pandoc-export-as-man)
    ;;(?k "to markdown." org-pandoc-export-to-markdown)
    (?k "to markdown and open." org-pandoc-export-to-markdown-and-open)
    (?K "as markdown." org-pandoc-export-as-markdown)
    ;;(?g "to markdown_github." org-pandoc-export-to-markdown_github)
    (?g "to markdown_github and open." org-pandoc-export-to-markdown_github-and-open)
    (?G "as markdown_github." org-pandoc-export-as-markdown_github)
    ;;(?4 "to markdown_mmd." org-pandoc-export-to-markdown_mmd)
    (?4 "to markdown_mmd and open." org-pandoc-export-to-markdown_mmd-and-open)
    (?$ "as markdown_mmd." org-pandoc-export-as-markdown_mmd)
    ;;(?6 "to markdown_phpextra." org-pandoc-export-to-markdown_phpextra)
    (?6 "to markdown_phpextra and open." org-pandoc-export-to-markdown_phpextra-and-open)
    (?& "as markdown_phpextra." org-pandoc-export-as-markdown_phpextra)
    ;;(?7 "to markdown_strict." org-pandoc-export-to-markdown_strict)
    (?7 "to markdown_strict and open." org-pandoc-export-to-markdown_strict-and-open)
    (?' "as markdown_strict." org-pandoc-export-as-markdown_strict)
    ;;(?w "to mediawiki." org-pandoc-export-to-mediawiki)
    (?w "to mediawiki and open." org-pandoc-export-to-mediawiki-and-open)
    (?W "as mediawiki." org-pandoc-export-as-mediawiki)
    ;;(?n "to native." org-pandoc-export-to-native)
    (?n "to native and open." org-pandoc-export-to-native-and-open)
    (?N "as native." org-pandoc-export-as-native)
    (?o "to odt and open." org-pandoc-export-to-odt-and-open)
    (?O "to odt." org-pandoc-export-to-odt)
    (?8 "to opendocument and open." org-pandoc-export-to-opendocument-and-open)
    (?( "to opendocument." org-pandoc-export-to-opendocument)
    ;;(?9 "to opml." org-pandoc-export-to-opml)
    (?9 "to opml and open." org-pandoc-export-to-opml-and-open)
    (?) "as opml." org-pandoc-export-as-opml)
    ;;(?0 "to org." org-pandoc-export-to-org)
    (?0 "to org and open." org-pandoc-export-to-org-and-open)
    (?  "as org." org-pandoc-export-as-org)
    ;;(?p "to plain." org-pandoc-export-to-plain)
    (?p "to plain and open." org-pandoc-export-to-plain-and-open)
    (?P "as plain." org-pandoc-export-as-plain)
    ;;(?v "to revealjs." org-pandoc-export-to-revealjs)
    (?v "to revealjs and open." org-pandoc-export-to-revealjs-and-open)
    (?V "as revealjs." org-pandoc-export-as-revealjs)
    ;;(?: "to rst." org-pandoc-export-to-rst)
    (?: "to rst and open." org-pandoc-export-to-rst-and-open)
    (?* "as rst." org-pandoc-export-as-rst)
    ;;(?r "to rtf." org-pandoc-export-to-rtf)
    (?r "to rtf and open." org-pandoc-export-to-rtf-and-open)
    (?R "as rtf." org-pandoc-export-as-rtf)
    ;;(?s "to s5." org-pandoc-export-to-s5)
    (?s "to s5 and open." org-pandoc-export-to-s5-and-open)
    (?S "as s5." org-pandoc-export-as-s5)
    ;;(?< "to slideous." org-pandoc-export-to-slideous)
    (?< "to slideous and open." org-pandoc-export-to-slideous-and-open)
    (?, "as slideous." org-pandoc-export-as-slideous)
    ;;(?y "to slidy." org-pandoc-export-to-slidy)
    (?y "to slidy and open." org-pandoc-export-to-slidy-and-open)
    (?Y "as slidy." org-pandoc-export-as-slidy)
    ;;(?t "to texinfo." org-pandoc-export-to-texinfo)
    (?t "to texinfo and open." org-pandoc-export-to-texinfo-and-open)
    (?T "as texinfo." org-pandoc-export-as-texinfo)
    ;;(?2 "to tei." org-pandoc-export-to-tei)
    (?2 "to tei and open." org-pandoc-export-to-tei-and-open)
    (?\" "as tei." org-pandoc-export-as-tei)
    ;;(?> "to textile." org-pandoc-export-to-textile)
    (?> "to textile and open." org-pandoc-export-to-textile-and-open)
    (?. "as textile." org-pandoc-export-as-textile)
    )
  "Pandoc menu-entry."
  :group 'org-pandoc
  :type 'list)

(org-export-define-derived-backend 'pandoc 'org
  :translate-alist '((latex-environment . org-pandoc-latex-environ)
                     (link      . org-pandoc-link)
                     (template  . org-pandoc-template)
                     (paragraph . org-pandoc-identity))
  ;; :export-block "PANDOC"
  :menu-entry
  `(?p "export via pandoc"
       ,org-pandoc-menu-entry)
  :options-alist
  '((:pandoc-options "PANDOC_OPTIONS" nil nil space)
    (:pandoc-metadata "PANDOC_METADATA" nil nil space)
    (:pandoc-variables "PANDOC_VARIABLES" nil nil space)
    (:epub-chapter-level "EPUB_CHAPTER_LEVEL" nil nil t)
    (:epub-cover-image "EPUB_COVER" nil nil t)
    (:epub-stylesheet "EPUB_STYLESHEET" nil nil t)
    (:epub-embed-font "EPUB_EMBED_FONT" nil nil newline)
    (:epub-meta "EPUB_META" nil nil newline)
    (:epub-css "EPUB_CSS" nil nil newline)
    (:epub-rights "EPUB_RIGHTS" nil nil newline)
    (:bibliography "BIBLIOGRAPHY")))

(defcustom org-pandoc-epub-rights
  (concat "Copyright " (format-time-string "%Y")
          (if user-full-name (concat " " user-full-name))
          (if user-mail-address (concat " <" user-mail-address ">")))
  "Pandoc option for EPUB copyrihgt statement."
  :group 'org-pandoc
  :type 'string)

(defcustom org-pandoc-markdown-extension ""
  "Pandoc Markdown Extension specification."
  :group 'org-pandoc
  :type 'string)

;;; each backend processor

(defcustom org-pandoc-options-for-asciidoc nil
  "Pandoc options for asciidoc."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-asciidoc-hook nil
  "Hook called after processing asciidoc."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-beamer-hook nil
  "Hook called after processing beamer."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

;;;###autoload
(defun org-pandoc-export-to-beamer-pdf (&optional a s v b e)
  "Export to beamer-pdf."
  (interactive) (org-pandoc-export 'beamer-pdf a s v b e))

;;;###autoload
(defun org-pandoc-export-to-beamer-pdf-and-open (&optional a s v b e)
  "Export to beamer-pdf and open."
  (interactive) (org-pandoc-export 'beamer-pdf a s v b e 0))

(defcustom org-pandoc-options-for-commonmark nil
  "Pandoc options for commonmark."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-commonmark-hook nil
  "Hook called after processing commonmark."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-commonmark (&optional a s v b e)
  "Export to commonmark."
  (interactive) (org-pandoc-export 'commonmark a s v b e))

;;;###autoload
(defun org-pandoc-export-to-commonmark-and-open (&optional a s v b e)
  "Export to commonmark and open."
  (interactive) (org-pandoc-export 'commonmark a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-commonmark (&optional a s v b e)
  "Export as commonmark."
  (interactive) (org-pandoc-export 'commonmark a s v b e t))

(defcustom org-pandoc-options-for-context nil
  "Pandoc options for context."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-context-hook nil
  "Hook called after processing context."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-docbook-hook nil
  "Hook called after processing docbook."
  :group 'org-pandoc
  :type 'hook)

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

(defcustom org-pandoc-options-for-docbook5 nil
  "Pandoc options for docbook5."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-docbook5-hook nil
  "Hook called after processing docbook5."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-docbook5 (&optional a s v b e)
  "Export to docbook5."
  (interactive) (org-pandoc-export 'docbook5 a s v b e))

;;;###autoload
(defun org-pandoc-export-to-docbook5-and-open (&optional a s v b e)
  "Export to docbook5 and open."
  (interactive) (org-pandoc-export 'docbook5 a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-docbook5 (&optional a s v b e)
  "Export as docbook5."
  (interactive) (org-pandoc-export 'docbook5 a s v b e t))

(defcustom org-pandoc-options-for-docx nil
  "Pandoc options for docx."
  :group 'org-pandoc
  :type org-pandoc-option-type)

;;;###autoload
(defun org-pandoc-export-to-docx (&optional a s v b e)
  "Export to docx."
  (interactive) (org-pandoc-export 'docx a s v b e))

;;;###autoload
(defun org-pandoc-export-to-docx-and-open (&optional a s v b e)
  "Export to docx and open."
  (interactive) (org-pandoc-export 'docx a s v b e 0))

(defcustom org-pandoc-options-for-dokuwiki nil
  "Pandoc options for dokuwiki."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-dokuwiki-hook nil
  "Hook called after processing dokuwiki."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-dokuwiki (&optional a s v b e)
  "Export to dokuwiki."
  (interactive) (org-pandoc-export 'dokuwiki a s v b e))

;;;###autoload
(defun org-pandoc-export-to-dokuwiki-and-open (&optional a s v b e)
  "Export to dokuwiki and open."
  (interactive) (org-pandoc-export 'dokuwiki a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-dokuwiki (&optional a s v b e)
  "Export as dokuwiki."
  (interactive) (org-pandoc-export 'dokuwiki a s v b e t))

(defcustom org-pandoc-options-for-dzslides nil
  "Pandoc options for dzslides."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-dzslides-hook nil
  "Hook called after processing dzslides."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-fb2-hook nil
  "Hook called after processing fb2."
  :group 'org-pandoc
  :type 'hook)

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

(defcustom org-pandoc-options-for-haddock nil
  "Pandoc options for haddock."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-haddock-hook nil
  "Hook called after processing haddock."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-haddock (&optional a s v b e)
  "Export to haddock."
  (interactive) (org-pandoc-export 'haddock a s v b e))

;;;###autoload
(defun org-pandoc-export-to-haddock-and-open (&optional a s v b e)
  "Export to haddock and open."
  (interactive) (org-pandoc-export 'haddock a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-haddock (&optional a s v b e)
  "Export as haddock."
  (interactive) (org-pandoc-export 'haddock a s v b e t))

(defcustom org-pandoc-options-for-html nil
  "Pandoc options for html."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-html-hook nil
  "Hook called after processing html."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-html5-hook nil
  "Hook called after processing html5."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-icml-hook nil
  "Hook called after processing icml."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-json-hook nil
  "Hook called after processing json."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-latex-hook nil
  "Hook called after processing latex."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-man-hook nil
  "Hook called after processing man."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-markdown-hook nil
  "Hook called after processing markdown."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-markdown_github-hook nil
  "Hook called after processing markdown_github."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-markdown_mmd-hook nil
  "Hook called after processing markdown_mmd."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-markdown_phpextra-hook nil
  "Hook called after processing markdown_phpextra."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-markdown_strict-hook nil
  "Hook called after processing markdown_strict."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-mediawiki-hook nil
  "Hook called after processing mediawiki."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-native-hook nil
  "Hook called after processing native."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-opml-hook nil
  "Hook called after processing opml."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-org-hook nil
  "Hook called after processing org."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-plain-hook nil
  "Hook called after processing plain."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-revealjs-hook nil
  "Hook called after processing revealjs."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-rst-hook nil
  "Hook called after processing rst."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-rtf-hook nil
  "Hook called after processing rtf."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-s5-hook nil
  "Hook called after processing s5."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-slideous-hook nil
  "Hook called after processing slideous."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-slidy-hook nil
  "Hook called after processing slidy."
  :group 'org-pandoc
  :type 'hook)

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
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-texinfo-hook nil
  "Hook called after processing texinfo."
  :group 'org-pandoc
  :type 'hook)

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

(defcustom org-pandoc-options-for-tei nil
  "Pandoc options for tei."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-tei-hook nil
  "Hook called after processing tei."
  :group 'org-pandoc
  :type 'hook)

;;;###autoload
(defun org-pandoc-export-to-tei (&optional a s v b e)
  "Export to tei."
  (interactive) (org-pandoc-export 'tei a s v b e))

;;;###autoload
(defun org-pandoc-export-to-tei-and-open (&optional a s v b e)
  "Export to tei and open."
  (interactive) (org-pandoc-export 'tei a s v b e 0))

;;;###autoload
(defun org-pandoc-export-as-tei (&optional a s v b e)
  "Export as tei."
  (interactive) (org-pandoc-export 'tei a s v b e t))

(defcustom org-pandoc-options-for-textile nil
  "Pandoc options for textile."
  :group 'org-pandoc
  :type org-pandoc-option-type)

(defcustom org-pandoc-after-processing-textile-hook nil
  "Hook called after processing textile."
  :group 'org-pandoc
  :type 'hook)

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
(defvar org-pandoc-epub-meta nil)
(defvar org-pandoc-epub-css nil)

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

(defun org-pandoc-latex-environ (blob contents _info)
  "Transcode a latex environment for export with pandoc.
Works around a bug in
pandoc (https://github.com/jgm/pandoc/issues/1764, present in at
least up-to and including pandoc 1.18) to surround an AMSMath
latex environment in BLOB with plain TeX equation block
delimiters, '$$ .. $$' in order for pandoc to properly recognise
the maths environment as a latex equation.  Also adds surrounding
line-breaks so that pandoc treats the math environment as its own
paragraph.  This avoids having text before or after the math
environment ending up on the same line as the equation.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
  (let ((raw-value (org-export-expand blob contents t)))
      ;; If we're exporting to a TeX-based format, there's no need for
      ;; this hack
      (if (member org-pandoc-format '(beamer beamer-pdf latex latex-pdf))
          raw-value
        ;; Otherwise, add '$$' elements before and after the block to
        ;; get pandoc to process it.
        (let* ((post-blank (org-element-property :post-blank blob))
               (case-fold-search t)
               (preface
                (replace-regexp-in-string
                 (rx (group-n 1 "\\begin{"
                              (or "align" "alignat" "eqnarray" "equation" "flalign" "gather" "multline")
                              (zero-or-one "*" ) "}"))
                 "\n$$\\1"
                 raw-value))
               (output
                (replace-regexp-in-string
                 (rx (group-n 1 "\\end{"
                              (or "align" "alignat" "eqnarray" "equation" "flalign" "gather" "multline")
                              (zero-or-one "*" ) "}"))
                 "\\1$$"
                 preface)))
          ;; If we've added the '$$' delimiters, then also set the
          ;; :post-blank property to add a blank line after this current
          ;; latex equation environment
          (unless (or (>= post-blank 1)
                      (string-equal raw-value output))
            (org-element-put-property blob :post-blank 1))
          output))))

(defun org-pandoc-link (link contents _info)
  "Transcode LINK object using the registered formatter for the
'pandoc backend.  If none exists, transcode using the registered
formatter for the 'org export backend.  CONTENTS is the
description of the link, as a string, or nil.  INFO is ignored."
  (or (org-export-custom-protocol-maybe link contents 'pandoc)
      (org-export-custom-protocol-maybe link contents 'org)
      (org-element-link-interpreter link contents)))

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
     (--map (let* ((_match (string-match "^\\([^:]+\\):\\(.+\\)$" it))
                   (name (intern (match-string 1 it)))
                   (value (match-string 2 it)))
              (cons name value))
            (split-string-and-unquote pandoc-options))))
  (setq org-pandoc-epub-css (plist-get info :epub-css))
  (setq org-pandoc-epub-meta
        (or (plist-get info :epub-meta)
            (concat
             (-when-let (epub-rights (or (plist-get info :epub-rights)
                                         org-pandoc-epub-rights))
               (concat "<dc:rights>" (url-insert-entities-in-string
                                      epub-rights) "</dc:rights>\n"))
             (-when-let (description (plist-get info :description))
               (concat "<dc:description>" description "</dc:description>\n"))
             (-when-let (keywords (plist-get info :keywords))
               (concat "<dc:subject>" keywords "</dc:subject>\n")))))
  (org-pandoc-put-options
   (--mapcat (-when-let (val (plist-get info (cdr it)))
               (list (cons (car it) (split-string-and-unquote val))))
             '((metadata . :pandoc-metadata)
               (variable . :pandoc-variables))))
  (org-pandoc-put-options
   (--mapcat (-when-let (val (plist-get info (cdr it)))
               (list (cons (car it) (split-string val "\n"))))
             '((epub-embed-font .    :epub-embed-font)
               (epub-chapter-level . :epub-chapter-level)
               (epub-cover-image   . :epub-cover-image)
               (epub-stylesheet    . :epub-stylesheet)
               (bibliography .       :bibliography))))
  ;; 'ox-pandoc' is derived from 'ox-org'. If 'ox-org' defines its own
  ;; template, then this template function (org-pandoc-template) calls
  ;; original ox-org template at the end.
  (let ((org-template
         (cdr (assoc 'template
                     (org-export-get-all-transcoders 'org)))))
    (if org-template
        (funcall org-template contents info)
    contents)))

(defun org-pandoc-identity (blob contents _info)
  "Transcode BLOB element or object back into Org syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored.
Like `org-org-identity', but also preserves #+ATTR_* tags in the
output."
  (org-export-expand blob contents t))

(defun org-pandoc-put-options (options)
  "Put alist OPTIONS to `org-pandoc-option-table'."
  (dolist (option options)
    (let* ((name (car option))
           (value (cdr option))
           (values
            (cond ((not (memq name org-pandoc-valid-options))
                   (error "Org-Pandoc: Improper Option Name! %s" name))
                  ((equal "t" value) t)
                  ((equal "nil" value) nil)
                  ((listp value) value)
                  ((memq name org-pandoc-colon-separated-options)
                   (split-string value ":"))
                  (t (list value)))))
      (if (memq name org-pandoc-file-options)
          (setq values
                (--map (if (file-exists-p it)
                           (if (= ?~ (string-to-char it)) (expand-file-name it) it)
                         (error "File (%s) can not be found" it)) values)))
      (puthash name values org-pandoc-option-table))))

(defun org-pandoc-run-to-buffer-or-file
    (input-file format subtreep &optional buffer-or-open)
  ;; buffer-or-open  (t: buffer) (nil: file) (0: file and open).
  (let ((output-file
         (unless (equal t buffer-or-open)
           (org-export-output-file-name
            (concat "." (symbol-name
                         (or (assoc-default format org-pandoc-extensions)
                             format)))
            subtreep)))
        (local-hook-symbol (intern (format "org-pandoc-after-processing-%s-hook"
                                           format)))
        css-temp-file meta-temp-file)
    (when (or (equal org-pandoc-format 'epub)
              (equal org-pandoc-format 'epub3))
      (when org-pandoc-epub-css
        (setq css-temp-file (make-temp-file "org-pandoc" nil ".css"))
        (puthash 'epub-stylesheet
                 (append (gethash 'epub-stylesheet org-pandoc-option-table)
                         (list css-temp-file))
                 org-pandoc-option-table)
        (with-temp-file css-temp-file
          (insert org-pandoc-epub-css)))
      (when org-pandoc-epub-meta
        (setq meta-temp-file (make-temp-file "org-pandoc" nil ".xml"))
        (org-pandoc-put-options `((epub-metadata ,meta-temp-file)))
        (with-temp-file meta-temp-file
          (insert org-pandoc-epub-meta))))
    (let ((process
           (org-pandoc-run input-file output-file format
                           'org-pandoc-sentinel org-pandoc-option-table)))
      (process-put process 'files (list input-file meta-temp-file css-temp-file))
      (process-put process 'output-file output-file)
      (process-put process 'local-hook-symbol local-hook-symbol)
      (process-put process 'buffer-or-open buffer-or-open))))

(defun org-pandoc-sentinel (process message)
  "PROCESS sentinel with MESSAGE."
  (case (process-status process)
    (run)
    (signal
     ;; Warning.  Temporary files not removed (for now.)
     (display-warning 'ox-pandoc (format "Signal Received. %s" message)))
    (exit
     (dolist (file (process-get process 'files))
       (if (and file (file-exists-p file)) (delete-file file)))
     (let ((exit-status (process-exit-status process))
           (buffer (process-buffer process))
           (output-file (process-get process 'output-file))
           (local-hook-symbol (process-get process 'local-hook-symbol))
           (buffer-or-open (process-get process 'buffer-or-open)))
       (if (/= exit-status 0)
           (message "Error occured. \n%s"
                    (with-current-buffer buffer (buffer-string)))
         (if output-file
             (progn
               (kill-buffer buffer)
               (message "Exported to %s." output-file)
               (if (and (boundp local-hook-symbol)
                        (symbol-value local-hook-symbol))
                   (with-temp-file output-file
                     (insert-file-contents output-file)
                     (run-hooks local-hook-symbol)))
               (if (equal 0 buffer-or-open)
                   (org-open-file output-file)))
           ;; output to buffer
           (pop-to-buffer buffer)
           (run-hooks local-hook-symbol)
           (set-auto-mode)))))))

(defun org-pandoc-run (input-file output-file format sentinel &optional options)
  "Run pandoc command with INPUT-FILE (org), OUTPUT-FILE, FORMAT and OPTIONS.
If BUFFER-OR-FILE is buffer, then output to specified buffer.
OPTIONS is a hashtable.  It runs asynchronously."
  (let* ((output-format
          (symbol-name
           (or (assoc-default format org-pandoc-translate-output-format)
               format)))
         (args
          `("-f" "org"
            "-t" ,(if (string-match output-format "^markdown")
                      (concat output-format org-pandoc-markdown-extension)
                    output-format)
            ,@(and output-file
                   (list "-o" (expand-file-name output-file)))
            ,@(-mapcat (lambda (key)
                         (-when-let (vals (gethash key options))
                           (if (equal vals t) (setq vals (list t)))
                           (--map (concat "--" (symbol-name key)
                                          (when (not (equal it t)) (format "=%s" it)))
                                  vals)))
                       (ht-keys options))
            ,(expand-file-name input-file))))
    (message "Running pandoc with args: %s" args)
    (let ((process
           (apply 'start-process
                  `("pandoc" ,(generate-new-buffer "*Pandoc*")
                    ,org-pandoc-command ,@args))))
      (set-process-sentinel process sentinel)
      process)))

(defun org-pandoc-startup-check ()
  (interactive)
  (if (not (executable-find org-pandoc-command))
      (display-warning 'ox-pandoc "Pandoc command is not installed.")
    (let ((version (with-temp-buffer
                    (call-process org-pandoc-command nil t nil "-v")
                    (buffer-string))))
      (if (not (string-match "^pandoc.*? \\([0-9]+\\)\\.\\([0-9]+\\)" version))
          (display-warning 'ox-pandoc "Pandoc version number can not be retrieved.")
        (let ((major (string-to-number (match-string 1 version)))
              (minor (string-to-number (match-string 2 version))))
          (unless (or (< 1 major)
                      (and (= 1 major)
                           (< 12 minor)))
            (display-warning 'ox-pandoc "This Pandoc may not support org-mode reader.")))))))

(org-pandoc-startup-check)

(provide 'ox-pandoc)

;;; ox-pandoc.el ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:

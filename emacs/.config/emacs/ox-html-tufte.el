;; -*- lexical-binding: t; fill-column: 101 -*-
;; ox-html-tufte.el --- HTML back-end for Tufte CSS 

;; Author: James Geddes <jgeddes@turing.ac.uk>

;;; Commentary:

;; This library implements an HMTL back-end for org-export which
;; produces HTML suitable for styling with (a derived version of)
;; tufte.css. The main difference with org-html is that footnotes
;; definitions are exported near to their references (and become
;; marginal notes in the final output)

;;; Code:

;;; Dependencies

(require 'ox)
(require 'ox-publish)
(require 'ox-html)


;; Derived from org-html-publish-to-html

;;;###autoload
(defun org-html-publish-to-html-tufte (plist filename pub-dir)
  "Publish an org file to HTML (for tufte.css).

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'html-tufte filename
		      (concat (when (> (length org-html-extension) 0) ".")
			      (or (plist-get plist :html-extension)
				  org-html-extension
				  "html"))
		      plist pub-dir))

;; Derived from org-html-footnote-reference
;; - html-format-footnote provides the required <label>...</label><input/>
;; - this function adds the footnote definition itself inside a <span>...</span>

;; TODO: Currently only works for inline footnotes! Block footnotes
;; are broken. This is because the parse tree of a block footnote
;; starts with a paragraph; and that is transcoded by org-export-data
;; using org-html-paragraph which wraps the footnote in a div. (It's
;; not clear to me whether paragraphs are allowed at all in marginal
;; notes in tufte.css

(defun org-html-tufte-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML and
include, nearby, the definition. CONTENTS is nil. INFO is a plist
holding contextual information.

Does not correctly deal with block footnotes."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       (plist-get info :html-footnote-separator)))
   (let* ((n (org-export-get-footnote-number footnote-reference info))
	  (fn (org-export-get-footnote-definition footnote-reference info))
	  (id (format "fnr.%d%s"
		      n
		      (if (org-export-footnote-first-reference-p
			   footnote-reference info)
			  ""
			".100"))))
     (concat
      (format (plist-get info :html-footnote-format) (format "fn-%d" n))
      ;; Add the body of the footnote here.
      (format "<span class=\"sidenote\">%s</span>" (org-export-data fn info))
      ))))

;; Define new backend derived from html
(org-export-define-derived-backend 'html-tufte 'html
  :translate-alist
  '((footnote-reference . org-html-tufte-footnote-reference)))

(provide 'ox-html-tufte)

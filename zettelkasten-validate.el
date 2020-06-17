;;; zettelkasten-validate -- Zettelkasten validation tools  -*- lexical-binding: t -*-

;; Copyright (C) 2020  Ashlynn Anderson

;; Author: Ashlynn Anderson <maintainer at pea.sh>
;; URL: https://github.com/lambdadog/zettelkasten-mode
;; Keywords: zettelkasten deft notetaking notes
;; Version: 0.1.0
;; Package-Requires ((emacs "25.1") (deft "0.8"))

;;; Commentary:
;;
;; Utilities to validate your zettelkasten

;;; Code:

(require 'zettelkasten-vars)
(require 'zettelkasten-util)
(require 'zettelkasten-deft)

(defun zettelkasten-validate-current-note ()
  "Validate the current note to ensure that there are no illegal
links in it."
  (interactive)
  (zettelkasten-deft--wrap
   (let ((tags (zettelkasten-util--get-tags)))
     (dolist (tag tags illegal)
       (unless (boundp 'illegal)
	 (setq illegal nil))
       (let* ((illegal-tags (zettelkasten-validate--get-illegal-tags tag))
	      (links-regexp (zettelkasten-validate--get-links-regexp illegal-tags)))
	 (unless (string= links-regexp "")
	   (save-excursion
	     (goto-char (point-min))
	     (while (re-search-forward links-regexp nil t)
	       (setq illegal t)
	       (message "Illegal link at line %d" (line-number-at-pos (match-beginning 0))))))))
     (not illegal))))

(defun zettelkasten-validate-zettelkasten ()
  "Validate the entire zettelkasten to ensure that there are no
illegal links contained in it. This function is run just to be
safe before publishing your zettelkasten."
  (interactive)
  (let ((restricted-tags (delete-dups (mapcar #'car zettelkasten-illegal-links))))
    (dolist (tag restricted-tags illegal)
      (unless (boundp 'illegal)
	(setq illegal nil))
      (unless (zettelkasten-validate-tag tag)
	(setq illegal t)))
    (not illegal)))

(defun zettelkasten-validate-tag (tag)
  "Validate a single tag in your zettelkasten. Useful mostly for
publishing, but is also used internally to validate your entire
zettelkasten with 'zettelkasten-validate-zettelkasten'."
  (interactive "sTag: ")
  (zettelkasten-deft--wrap
   (let* ((illegal-tags (zettelkasten-validate--get-illegal-tags tag))
	  (links-regexp (zettelkasten-validate--get-links-regexp illegal-tags)))
     (unless (string= links-regexp "")
       (let* ((tagged-notes (zettelkasten-util--filter-tags `(,tag)))
	      (matches (zettelkasten-util--filter-regexps `(,links-regexp) tagged-notes)))
	 (when (> (length matches) 0)
	   (message "\"%s\" tag contains illegal links" tag))
	 matches)))))

(defun zettelkasten-validate--get-links-regexp (tags)
  "Given a list of tags, find all documents tagged with them and
build a regexp from their note links."
  (let* ((notes-nested (mapcar (lambda (tag) (zettelkasten-util--filter-tags `(,tag))) tags))
	 (notes (apply #'append notes-nested))
	 (links (mapcar #'zettelkasten-util--note-to-link-format notes)))
    (regexp-opt links)))

(defun zettelkasten-validate--get-illegal-tags (tag)
  "Given a tag, get all illegal tags to link from
'zettelkasten-illegal-links'."
  (mapcar #'cdr (seq-filter (lambda (elt) (equal (car elt) tag))
			    zettelkasten-illegal-links)))

(provide 'zettelkasten-validate)

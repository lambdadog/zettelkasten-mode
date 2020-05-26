;;; zettelkasten-org -- Zettelkasten utilites  -*- lexical-binding: t -*-

;; Copyright (C) 2020  Ashlynn Anderson

;; Author: Ashlynn Anderson <maintainer at pea.sh>
;; URL: https://github.com/lambdadog/zettelkasten-mode
;; Keywords: zettelkasten deft notetaking notes
;; Version: 0.1.0
;; Package-Requires ((emacs "25.1") (deft "0.8"))

;;; Commentary:
;;
;; Utilities for the rest of the zettelkasten-mode package.

;;; Code:

(require 'zettelkasten-vars)
(require 'deft)

(defun zettelkasten-util--id-and-title (note)
  "Returns the ID and title of a note (must be passed by
filepath)."
  (let ((basename (file-name-base note)))
    (string-match "^\\(?1:[0-9\-]*\\) \\(?2:.*\\)$" basename)
    `(,(match-string-no-properties 1 basename) . ,(match-string-no-properties 2 basename))))

(defun zettelkasten-util--new-note (title)
  "Create a new zettelkasten note."
  (let* ((zettel-id (format-time-string zettelkasten-id-format))
	 (filename (concat zettel-id " " title)))
    (deft-new-file-named filename)
    (insert (concat "#+TITLE: " title "\n"
		    "#+TAGS: "))
    filename))

(defun zettelkasten-util--validate-zettelkasten ()
  "Validate the zettelkasten to ensure that no public notes
forward-link to private notes."
  (let ((private-id-regexp (regexp-opt
			    (mapcar #'zettelkasten-util--note-to-link-format
				    (zettelkasten-util--filter-tags '("private")))))
	(public-notes (zettelkasten-util--filter-tags '("public"))))
    ;; Handle the case where there's no private notes
    (unless (string= private-id-regexp "")
      (let ((matches (zettelkasten-util--filter-regexps `(,private-id-regexp)
							public-notes)))
	(if (> (length matches) 0)
	    (progn
	      (message "Zettelkasten has links from public to private notes")
	      matches)
	  matches)))))

(defconst zettelkasten-util--tag-regexp-format-string "#\\+TAGS:.*? \\(?:#?\\)%s[ \n]"
  "Format string for converting a tag to a regexp that matches
the tag. Can accept both non-prefixed and #-prefixed tags.")

(defun zettelkasten-util--tag-regexp (tag)
  "Takes a tag (without #) and returns a regexp matching it in a
zettelkasten note."
  (format zettelkasten-util--tag-regexp-format-string (regexp-quote tag)))

(defun zettelkasten-util--note-to-link-format (note)
  "Format note ID to a link string that can be searched for."
  (let ((id (car (zettelkasten-util--id-and-title note))))
    (concat "zettel:" id)))

(defun zettelkasten-util--file-from-id (id)
  (let ((files (zettelkasten-util--filter-strings `(,id) nil t)))
    (when (= (length files) 1)
      (car files))))

(defun zettelkasten-util--filter-regexps (regexps &optional files only-filenames)
  "Filter files with a list of regexps."
  (let ((deft-filter-regexp regexps)
	(deft-filter-only-filenames only-filenames)
	(deft-incremental-search nil))
    (deft-filter-files (if files files
			 deft-all-files))))

(defun zettelkasten-util--filter-strings (strings &optional files only-filenames)
  "Filter files with a list of strings."
  (zettelkasten-util--filter-regexps (mapcar #'regexp-quote strings) files only-filenames))

(defun zettelkasten-util--filter-tags (tags &optional files)
  "Filter files with a list of tags."
  (zettelkasten-util--filter-regexps (mapcar #'zettelkasten-util--tag-regexp tags) files))

(provide 'zettelkasten-util)

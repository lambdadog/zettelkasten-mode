;;; zettelkasten-deft -- Wrappers for deft  -*- lexical-binding: t -*-

;; Copyright (C) 2020  Ashlynn Anderson

;; Author: Ashlynn Anderson <maintainer at pea.sh>
;; URL: https://github.com/lambdadog/zettelkasten-mode
;; Keywords: zettelkasten deft notetaking notes
;; Version: 0.1.0
;; Package-Requires ((emacs "25.1") (deft "0.8"))

;;; Commentary:
;;
;; Deft is normally designed to be used very statefully, and by the
;; user. What we do here is co-opt its functionality, assuming that
;; the user doesn't currently have zettelkasten-directory set to the
;; same value as deft-directory.

;;; Code:

(require 'zettelkasten-vars)
(require 'deft)

(defvar zettelkasten-deft-all-files nil
  "All files currently in the deft directory, shadows
'deft-all-files'.")

(defvar zettelkasten-deft-hash-contents nil
  "Hash containing complete cached file contents, shadows
'deft-hash-contents'.")

(defvar zettelkasten-deft-hash-mtimes nil
  "Hash containing cached file modification times, shadows
'deft-hash-mtimes'.")

(defvar zettelkasten-deft-hash-titles nil
  "Hash containing cached file titles, shadows
'deft-hash-titles'.")

(defvar zettelkasten-deft-hash-summaries nil
  "Hash containing cached file summaries, shadows
'deft-hash-summaries'.")

;; I really feel like there ought to be a better way to do this than
;; copying data in and out, but I can't really figure out one.
(defmacro zettelkasten-deft--wrap (&rest body)
  ;; Ensure deft is initialized
  (let ((body `(let ((deft-extensions '("org"))
		     (deft-default-extension "org")
		     (deft-use-filename-as-title t)
		     (deft-auto-save-interval 0))
		 (unless (and (boundp 'deft-all-files)
			      deft-all-files)
		   (deft-cache-initialize)
		   (deft-cache-update-all))
		 ,@body)))
    (if (string= zettelkasten-directory deft-directory)
        body
      `(let ((deft-directory zettelkasten-directory)
	     (deft-all-files zettelkasten-deft-all-files)
	     (deft-hash-contents zettelkasten-deft-hash-contents)
	     (deft-hash-mtimes zettelkasten-deft-hash-mtimes)
	     (deft-hash-titles zettelkasten-deft-hash-titles)
	     (deft-hash-summaries zettelkasten-deft-hash-summaries))
	 (let ((result ,body))
	   (setq zettelkasten-deft-all-files deft-all-files
		 zettelkasten-deft-hash-contents deft-hash-contents
		 zettelkasten-deft-hash-mtimes deft-hash-mtimes
		 zettelkasten-deft-hash-titles deft-hash-titles
		 zettelkasten-deft-hash-summaries deft-hash-summaries)
	   result)))))

(provide 'zettelkasten-deft)

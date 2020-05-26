;;; zettelkasten --- Code for interacting with your zettelkasten  -*- lexical-binding: t -*-

;; Copyright (C) 2020  Ashlynn Anderson

;; Author: Ashlynn Anderson <maintainer at pea.sh>
;; URL: https://github.com/lambdadog/zettelkasten-mode
;; Keywords: zettelkasten deft notetaking notes
;; Version: 0.1.0
;; Package-Requires ((emacs "25.1") (deft "0.8"))

;;; Commentary
;;
;; This file contains code for interacting with your zettelkasten
;; independent of actually editing a note (which is what
;; zettelkasten-mode is for).

;;; Code:

(require 'zettelkasten-deft)
(require 'zettelkasten-util)

;;;###autoload
(defun zettelkasten-new-public-note (title)
  "Create a new public-tagged note in your zettelkasten."
  (interactive "sTitle: ")
  (let ((filename (zettelkasten-deft--wrap
		   (zettelkasten-util--new-note title))))
    (insert "#public ")
    (unless (called-interactively-p 'interactive)
      filename)))

;;;###autoload
(defun zettelkasten-new-private-note (title)
  "Create a new private-tagged note in your zettelkasten."
  (interactive "sTitle: ")
  (let ((filename (zettelkasten-deft--wrap
		   (zettelkasten-util--new-note title))))
    (insert "#private ")
    (unless (called-interactively-p 'interactive)
      filename)))

(provide 'zettelkasten)

;;; zettelkasten-mode --- A mode for writing zettelkasten in emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2020  Ashlynn Anderson

;; Author: Ashlynn Anderson <maintainer at pea.sh>
;; URL: https://github.com/lambdadog/zettelkasten-mode
;; Keywords: zettelkasten deft notetaking notes
;; Version: 0.1.0
;; Package-Requires ((emacs "25.1") (deft "0.8"))

;;; Commentary
;;
;; Zettelkasten note-taking mode for emacs. Uses deft, but takes
;; (hacky) steps to avoid actually taking over your deft config. If
;; you wish to use deft to manage your zettelkasten yourself, it will
;; likely improve performance. Just set your 'deft-directory' to the
;; same location as your 'zettelkasten-directory'.

;;; Code:

(require 'zettelkasten-vars)
(require 'zettelkasten-deft)
(require 'zettelkasten-util)
(require 'zettelkasten-org)
(require 'deft)

;;;###autoload
(define-minor-mode zettelkasten-mode
  "A minor mode for editing org files in your zettelkasten"
  :lighter " Zettel"
  (cond
   (zettelkasten-mode (zettelkasten-deft--wrap
		       ;; Set up zettelkasten links
		       (zettelkasten-org--set-up)
		       
		       (add-hook 'after-save-hook
				 #'zettelkasten-mode--update-deft-cache-hook)
		       (add-hook 'deft-open-file-hook
				 #'zettelkasten-mode--remove-after-save-hook)))
   (t (assoc-delete-all "zettel" org-link-parameters)
      (remove-hook 'after-save-hook
		   #'zettelkasten-mode--update-deft-cache-hook))))

(defun zettelkasten-mode--update-deft-cache-hook ()
  "Updates the deft search cache when you save a note in your
zettelkasten."
  (let ((deft-is-zettel (string= zettelkasten-directory
				 deft-directory)))
    (zettelkasten-deft--wrap
     (deft-cache-update-file buffer-file-name)
     (when deft-is-zettel
       (if (deft-buffer-visible-p)
	   (deft-refresh-filter)
	 (setq deft-pending-updates t))))))

(defun zettelkasten-mode--remove-after-save-hook ()
  "Removes the zettelkasten-mode after-save-hook if the file is
opened with deft, as deft adds its own after-save-hook to update
the cache."
  ;; I don't know when this would happen if they weren't the same, but
  ;; doesn't hurt to check...
  (when (string= zettelkasten-directory deft-directory)
    (remove-hook 'after-save-hook
		 #'zettelkasten-mode--update-deft-cache-hook)))

(add-hook 'org-mode-hook #'zettelkasten-mode--maybe-enable)
(defun zettelkasten-mode--maybe-enable ()
  "Enable zettelkasten-mode if in zettelkasten-directory"
  (when (string= default-directory zettelkasten-directory)
    (zettelkasten-mode 1)))

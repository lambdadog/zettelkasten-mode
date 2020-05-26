;;; zettelkasten-org -- Org helpers for zettelkasten  -*- lexical-binding: t -*-

;; Copyright (C) 2020  Ashlynn Anderson

;; Author: Ashlynn Anderson <maintainer at pea.sh>
;; URL: https://github.com/lambdadog/zettelkasten-mode
;; Keywords: zettelkasten deft notetaking notes
;; Version: 0.1.0
;; Package-Requires ((emacs "25.1") (deft "0.8"))

;;; Commentary:
;;
;; Adds ~zettel:~ links to org-mode.

;;; Code:

(require 'zettelkasten-deft)
(require 'zettelkasten-util)

(defun zettelkasten-org--set-up ()
  (make-local-variable 'org-link-parameters)
  (org-link-set-parameters "zettel"
			   :follow #'zettelkasten-org-follow-link
			   :store #'zettelkasten-org-store-link))

(defun zettelkasten-org-follow-link (id)
  "Follow a zettel: link in org-mode."
  (zettelkasten-deft--wrap
   (let ((file (zettelkasten-util--file-from-id id)))
     (if file
	 (deft-open-file file)
       (user-error "ID Error. Either no or multiple zettelkasten notes found with ID %s" id)))))

(defun zettelkasten-org-store-link ()
  "Store a link to a zettel note."
  (when zettelkasten-mode
    (let ((zettel-info (zettelkasten-util--id-and-title buffer-file-name)))
      (org-store-link-props
       :type "zettel"
       :link (concat "zettel:" (car zettel-info))
       :description (cdr zettel-info)))))

(provide 'zettelkasten-org)

;;; zettelkasten-vars -- Variables for zettelkasten-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2020  Ashlynn Anderson

;; Author: Ashlynn Anderson <maintainer at pea.sh>
;; URL: https://github.com/lambdadog/zettelkasten-mode
;; Keywords: zettelkasten deft notetaking notes
;; Version: 0.1.0
;; Package-Requires ((emacs "25.1") (deft "0.8"))

;;; Code:

(defgroup zettelkasten-mode nil
  "Emacs zettelkasten-mode group."
  :group 'local)

(defcustom zettelkasten-directory (expand-file-name "~/.zettel/")
  "Zettelkasten directory. Must have an ending slash for correct functionality."
  :type 'directory
  :safe 'stringp
  :group 'zettelkasten-mode)

(defcustom zettelkasten-id-format "%Y-%m-%d-%H%M%S"
  "Format string used for the ID portion of new file
names. Package functionality relies on this number being
unique. See 'format-time-string' for possible formatting."
  :type 'string
  :group 'zettelkasten-mode)

(provide 'zettelkasten-vars)

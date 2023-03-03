;;; init.el --- Initialisation file for Emacs
;; 

;;; Commentary:
;; Tangle the real init file (emacs-config.org) and then read that

;;; Code:

(require 'org)

(let ((custom-file-name (expand-file-name "custom.el" user-emacs-directory)))
  (setq custom-file custom-file-name)
  (load custom-file))

(org-babel-load-file
 (expand-file-name "emacs-config.org" user-emacs-directory))

;; (message "About to load init2.el")
;; (load
;;  (expand-file-name "init2.el" "~/.config/emacs"))

(provide 'init)

;;; init.el ends here

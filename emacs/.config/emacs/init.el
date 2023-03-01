;;; init.el --- Initialisation file for Emacs
;; 

;;; Commentary:
;; Tangle the real init file (init.org) and load everything else

;;; Code:

(require 'org)

(message "About to tangle init.org")
(org-babel-load-file
 (expand-file-name "emacs-config.org" "~/.config/emacs"))

;; (message "About to load init2.el")
;; (load
;;  (expand-file-name "init2.el" "~/.config/emacs"))

(provide 'init)

;;; init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))
 '(package-selected-packages '(color-theme-sanityinc-solarized use-package))
 '(warning-suppress-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Fira Code"))))
 '(font-lock-constant-face ((t (:foreground "medium sea green"))))
 '(fringe ((t (:foreground "#93a1a1"))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :weight bold))))
 '(tuareg-font-lock-governing-face ((t (:weight bold)))))


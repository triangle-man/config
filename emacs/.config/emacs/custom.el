
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-solarized-dark))
 '(custom-safe-themes
   '("48d34b6afe72407ca494387c8bea495bb2deee96bd88516f302db1f11e1810a1"
     "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328"
     "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4"
     default))
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-pyvenv
			 elpy-module-highlight-indentation
			 elpy-module-yasnippet elpy-module-django
			 elpy-module-sane-defaults))
 '(hledger-currency-string "£")
 '(hledger-top-income-account "revenue")
 '(ledger-binary-path "ledger")
 '(package-selected-packages
   '(adaptive-wrap auctex color-theme-sanityinc-solarized company
		   company-maxima csv-mode elpy engrave-faces ess
		   eval-in-repl flycheck flycheck-ocaml forge
		   geiser-guile gnuplot haskell-mode hasklig-mode
		   htmlize ledger-mode lua-mode magit maxima merlin
		   nix-mode opam-switch-mode opencl-c-mode org
		   org-present paredit paren-face poly poly-markdown
		   poly-org polymode presentation-mode quarto-mode
		   racket-mode request tuareg use-package utop
		   visual-fill-column))
 '(python-interpreter "python3")
 '(safe-local-variable-values '((TeX-command-extra-options . "-shell-escape")))
 '(tool-bar-mode nil)
 '(warning-suppress-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight medium :height 141 :width normal :foundry "nil" :family "Fira Code"))))
 '(font-lock-constant-face ((t (:foreground "medium sea green"))))
 '(fringe ((t (:background unspecified))))
 '(mode-line ((t (:box (:line-width (2 . 2) :style flat-button) :weight normal))))
 '(mode-line-inactive ((t (:box (:line-width (2 . 2) :style flat-button)))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :weight bold))))
 '(tuareg-font-lock-governing-face ((t (:weight bold)))))

;;in case session is installed locally: copy features like session.el to ~/.emacs.d/lisp 
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;this is to enable melpa package; after this do: list-packages and pray for it to correctly.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

(require 'auto-complete-config)
(ac-config-default)

(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(desktop-save-mode 1)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;;in case session is installed locally: copy features like session.el to ~/.emacs.d/lisp 
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(desktop-save-mode 1)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

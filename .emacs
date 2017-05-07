;;this is how a comments looks in LISP


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(prefer-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(require 'session)
;;(require 'edit-utils)


;;debug why emacs screws indentation
(setq c-debug-parse-state t)

(add-hook 'after-init-hook 'session-initialize)
(desktop-save-mode 1)

;;Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)


(setq ring-bell-function 'ignore)
(setq sentence-end-double-space nil)

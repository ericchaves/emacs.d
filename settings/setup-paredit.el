;; My keybindings for paredit

(require 'paredit)

(show-paren-mode 1)

(add-hook 'clojure-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'cider-repl-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))

(provide 'setup-paredit)
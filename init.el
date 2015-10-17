;;; init.el 

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; No splash screen please ...
(setq inhibit-startup-message t)

;; Set dependencies path 
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;; And add them to load-path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

; Set custom theme
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'zenburn t)

(setq auto-save-default nil)
(setq make-backup-files nil)
(setq backup-inhibited t)

;; Write backup files to own directory
;(setq backup-directory-alist 
;      `(("." . "~/.emacs.d/backups")))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Setup packages
(require 'setup-packages)

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(dockerfile-mode
     cider
     circe
     clojure-mode
     clojurescript-mode
     elisp-slime-nav
     helm
     helm-projectile
     highlight-escape-sequences
     json
     js2-refactor
     js2-mode
     magit
     markdown-mode
     nodejs-repl
     paredit
     projectile
     rainbow-delimiters
     rainbow-mode
     request
     smartparens
     smooth-scrolling
     undo-tree
     whitespace-cleanup-mode
;     yasnippet
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Lets start with a smattering of sanity
;(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Setup extensions
; (eval-after-load 'ido '(require 'setup-ido))
; (eval-after-load 'org '(require 'setup-org))
; (eval-after-load 'dired '(require 'setup-dired))
; (eval-after-load 'magit '(require 'setup-magit))
; (eval-after-load 'grep '(require 'setup-rgrep))

;(eval-after-load 'shell '(require 'setup-shell))

(require 'setup-projectile)
(require 'setup-helm)
(require 'setup-helm-projectile)
(require 'setup-paredit)
(require 'setup-rainbow-delimiters)
(require 'setup-clojars-search)

;(require 'setup-dockerfile)
;(require 'setup-hippie)
;;(require 'setup-yasnippet)
;;(require 'setup-perspective)
;;(require 'setup-ffip)
;;(require 'setup-html-mode)
; (require 'setup-markdown)
; (require 'setup-js2-mode)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
  
;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

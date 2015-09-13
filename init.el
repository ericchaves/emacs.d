;;; init.el 

;; Always ALWAYS use UTF-8

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
;;(dolist (p '(settings-dir site-lisp-dir)) (load-path p))
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

; Set custom theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn-emacs/")
(load-theme 'zenburn t)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Setup packages
(require 'setup-packages)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(dockerfile-mode
     elisp-slime-nav
     highlight-escape-sequences
   	 magit
     markdown-mode
     nodejs-repl
     paredit
     restclient
     simple-httpd
     smartparens
     smooth-scrolling
     undo-tree
     whitespace-cleanup-mode
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Setup extensions


;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
  
;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

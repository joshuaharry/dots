;;; package --- Summary: -*- lexical-binding: t -*-

;;; VANILLA CONFIGURATION
;;; ------------------------------------------------------

;; Always run Emacs as a full desktop. If you're profiling, you probably
;; want to turn this off, so we keep it at the top of the file.
(set-frame-parameter nil 'fullscreen 'fullboth)

;; Global settings
(setq mac-right-command-modifier 'control
      mac-command-modifier 'meta
      show-paren-delay 0
      ring-bell-function #'ignore
      insert-directory-program "/opt/homebrew/bin/gls"
      backup-directory-alist '(("." . "~/.config/emacs/backups"))
      dired-use-ls-dired 0)

;;; Simple, good enough modeline
(setq-default mode-line-format '(" %b | %l:%C "))

;; Global Programming Settings
(defun jlib/prog-mode-hook ()
  "My global settings for programming."
  (display-line-numbers-mode))

(add-hook 'prog-mode-hook #'jlib/prog-mode-hook)

;; Convenient editing major modes
(electric-pair-mode)
(show-paren-mode)

;; Improve dired support
(require 'dired-x)

(defun jlib/dired-mode-hook ()
  "Hook for entering dired mode."
  (define-key dired-mode-map (kbd "s") nil)
  (auto-revert-mode))

(add-hook 'dired-mode-hook #'jlib/dired-mode-hook)

(defadvice load-theme (before theme-dont-propagate activate)
  "Prevent themes from interfering with one another."
  (mapc #'disable-theme custom-enabled-themes))

;; Misc keybindings
(global-set-key (kbd "C-j") #'set-mark-command)
(global-set-key (kbd "C-z") #'undo)
(global-set-key (kbd "M-q") #'kill-emacs)

(defun google ()
  "Go to Google.com inside of Emacs."
  (interactive)
  (eww "https://google.com"))

;; Font configuration
(defvar *jlib/default-font-size* 200
  "Default font size to revert to on changes.")

(set-face-attribute
 'default nil
 :font "M+ 1m"
 :height *jlib/default-font-size*)

(defun jlib/reset-font-size ()
  "Reset my Emacs font size."
  (interactive)
  (set-face-attribute
   'default nil :height *jlib/default-font-size*))

(defun jlib/increase-font-size ()
  "Make the current font bigger."
  (interactive)
  (set-face-attribute
   'default nil
   :height (+ (face-attribute 'default :height) 20)))

(defun jlib/decrease-font-size ()
  "Make the current font smaller."
  (interactive)
  (set-face-attribute
   'default nil
   :height (- (face-attribute 'default :height) 20)))

(global-set-key (kbd "C-0") #'jlib/reset-font-size)
(global-set-key (kbd "C-=") #'jlib/increase-font-size)
(global-set-key (kbd "C--") #'jlib/decrease-font-size)

;;; Window Management
(defun jlib/push-border-left ()
  "Push the border of the current window I am editing to the left."
  (interactive)
  (when (> (length (window-list)) 1)
    (if (window-right (get-buffer-window))
        (enlarge-window-horizontally 5)
      (shrink-window-horizontally 5))))

(defun jlib/push-border-right ()
  "Push the border of the current window I am editing to the right."
  (interactive)
  (when (> (length (window-list)) 1)
    (if (window-left (get-buffer-window))
        (enlarge-window-horizontally 5)
      (shrink-window-horizontally 5))))

(defun jlib/push-border-down ()
  "Push the border of the current window I am editing downwards."
  (interactive)
  (when (> (length (window-list)) 1)
    (if (window-in-direction 'below)
        (enlarge-window 5)
      (shrink-window 5))))

(defun jlib/push-border-up ()
  "Push the border of the current window I am editing upwards."
  (interactive)
  (when (> (length (window-list)) 1)
    (if (window-in-direction 'above)
        (enlarge-window 5)
      (shrink-window 5))))

(defun jlib/other-window-backwards ()
  "Go to the previous window backwards."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-c l") #'jlib/push-border-left)
(global-set-key (kbd "C-c h") #'jlib/push-border-right)
(global-set-key (kbd "C-c j") #'jlib/push-border-down)
(global-set-key (kbd "C-c k") #'jlib/push-border-up)
(global-set-key (kbd "C-S-j") #'other-window)
(global-set-key (kbd "C-S-k") #'jlib/other-window-backwards)

;; Emacs LISP Editing Config
(defun jlib/indent-lisp ()
  "Indent the buffer in a LISP-y way for me."
  (interactive)
  (indent-region (point-min) (point-max))
  (message "Buffer indented successfully."))

(defun jlib/emacs-lisp-mode ()
  "My personal settings for editing Emacs LISP."
  (interactive)
  (define-key emacs-lisp-mode-map (kbd "C-c p") #'jlib/indent-lisp))

(add-hook 'emacs-lisp-mode-hook #'jlib/emacs-lisp-mode)

(defun jlib/lisp-interaction-mode ()
  "My personal settings for LISP interaction mode."
  (interactive)
  (define-key lisp-interaction-mode-map (kbd "C-j") #'set-mark-command)
  (define-key lisp-interaction-mode-map (kbd "C-c p") #'jlib/indent-lisp))

(add-hook 'emacs-lisp-mode-hook #'jlib/emacs-lisp-mode)
(add-hook 'lisp-interaction-mode-hook #'jlib/lisp-interaction-mode)

;; Web Development

;; Project Management
(put 'dired-find-alternate-file 'disabled nil)

(defun jlib/path-join (root &rest dirs)
  "Join ROOT with DIRS to construct a path string in an OS independent way."
  (let ((res root))
    (dolist (el dirs res)
      (setq res (expand-file-name el res)))))

(defvar *jlib/project-file*
  (jlib/path-join user-emacs-directory "current-project")
  "Path to a file that contains the project on which I was last working.")

(defun jlib/get-current-project ()
  "Get the path to the current project on which I am working."
  (let ((project
	 (string-trim
	  (condition-case nil
	      (with-temp-buffer
		(insert-file-contents *jlib/project-file*)
		(buffer-string))
	    (file-error (getenv "HOME"))))))
    (if (file-directory-p project) project (getenv "HOME"))))

(defun jlib/set-current-project ()
  "Save the current location to a convenient spot."
  (interactive)
  (write-region
   (replace-regexp-in-string "~" (getenv "HOME") default-directory)
   nil *jlib/project-file*))

(defun jlib/goto-current-project ()
  "Go to my last working project."
  (interactive)
  (dired (jlib/get-current-project)))

(global-set-key (kbd "C-c d s") #'jlib/set-current-project)
(global-set-key (kbd "C-c d p") #'jlib/goto-current-project)

(defun jlib/make-goto (keys path)
  "Create a keybinding where pressing KEYS takes you to PATH in dired."
  (global-set-key (kbd keys) #'(lambda () (interactive) (dired path))))

(defvar *jlib/keylists*
  '(("C-c d c" . "~/code")
    ("C-c g h" . "~")
    ("C-c d g" . "~/code/github")
    ("C-c d e" . "~/.config/emacs")
    ("C-c d q" . "~/code/scratch"))
  "A list of keybindings to file paths.")

(dolist (l *jlib/keylists*)
  (jlib/make-goto (car l) (cdr l)))

(setq initial-buffer-choice (jlib/get-current-project))

;; Terminal Management
(defun jlib/shell ()
  "Quickly open up an `ansi-term' using the fish shell."
  (interactive)
  (ansi-term (getenv "SHELL")))

(defun jlib/goto-shell ()
  "Switch to or open up a `ansi-term' buffer with my terminal."
  (interactive)
  (let ((jlib/terminal-buffer (get-buffer "*ansi-term*")))
    (if jlib/terminal-buffer (switch-to-buffer jlib/terminal-buffer)
      (jlib/shell))))

(defun jlib/terminal-right ()
  "Open a terminal to the right."
  (interactive)
  (split-window-right)
  (windmove-right)
  (jlib/goto-shell))

(defun jlib/terminal-left ()
  "Open a terminal to the left."
  (interactive)
  (split-window-right)
  (jlib/goto-shell))

(defun jlib/terminal-down ()
  "Open a terminal below."
  (interactive)
  (split-window-below)
  (windmove-down)
  (jlib/goto-shell))

(defun jlib/terminal-up ()
  "Open a terminal above."
  (interactive)
  (split-window-below)
  (jlib/goto-shell))

(global-set-key (kbd "C-c t t") #'jlib/goto-shell)
(global-set-key (kbd "C-c t l") #'jlib/terminal-right)
(global-set-key (kbd "C-c t h") #'jlib/terminal-left)
(global-set-key (kbd "C-c t j") #'jlib/terminal-down)
(global-set-key (kbd "C-c t k") #'jlib/terminal-up)

(defun term--update-term-menu ()
  "WORKAROUND - Prevent this function from throwing errors related to the menu bar, which I have disabled."
  nil)

;;; PACKAGES & THIRD PARTY SOFTWARE
;;; ------------------------------------------------------

;; Bootstrap our package manager: straight.el
(setq straight-check-for-modifications nil
      use-package-always-defer t
      straight-use-package-by-default t)

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Profiling

;; Work around a bug where esup tries to step into the byte-compiled
;; version of `cl-lib', and fails horribly.
(setq esup-depth 0)
(use-package esup)

;; Path Configuration
(use-package exec-path-from-shell)
(setq exec-path-from-shell-arguments nil)
(exec-path-from-shell-initialize)

;; Useful global modes
(use-package selectrum
  :demand t
  :config (selectrum-mode))

(use-package ctrlf
  :demand t
  :config (ctrlf-mode))

(use-package company
  :demand t
  :config
  (setq company-idle-delay nil)
  (global-set-key (kbd "M-n") #'company-complete)
  ;; Overwrite this warning; we don't need it.
  (defun company--warn-changed-binding () nil)
  (global-company-mode))

;; Linting
(use-package flycheck)

;; Git Integration
(use-package magit)

;; Fuzzy Finding
(use-package rg)

;; Snippets
(use-package yasnippet)
(yas-global-mode)

;; Theming
(use-package doom-themes)
(load-theme 'doom-ir-black t)
(set-face-foreground 'font-lock-comment-face "#999999")
(set-face-background 'company-tooltip-selection "#353535")

;; LISP Development
(use-package cider)
(use-package rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)

(defun jlib/clojure-mode-hook ()
  "Hook for editing Clojure code."
  (define-key clojure-mode-map (kbd "C-c p") #'jlib/indent-lisp)
  (rainbow-delimiters-mode))

(add-hook 'clojure-mode-hook #'jlib/clojure-mode-hook)

;; Documentation
(use-package olivetti)
(use-package markdown-mode)

(defun jlib/writing-mode-hook ()
  "Custom hook for writing plain documents."
  (interactive)
  (display-line-numbers-mode)
  (olivetti-mode))

(add-hook 'markdown-mode-hook #'jlib/writing-mode-hook)

;; Language Server Integration

;; Disable most of the features that come with LSP mode; I only use it for
;; linting, autocomplete, and automatic formatting most of the time.

(setq
 lsp-enable-symbol-highlighting nil
 lsp-enable-indentation nil
 lsp-ui-doc-enable nil
 lsp-ui-doc-show-with-cursor nil
 lsp-ui-doc-show-with-mouse nil
 lsp-ui-doc-show-with-mouse nil
 lsp-headerline-breadcrumb-enable nil
 lsp-ui-sideline-enable nil
 lsp-ui-sideline-show-code-actions nil
 lsp-ui-sideline-enable nil
 lsp-ui-sideline-show-hover nil
 lsp-modeline-code-actions-enable nil
 lsp-ui-sideline-enable nil
 lsp-ui-sideline-show-diagnostics nil 
 lsp-eldoc-enable-hover nil
 lsp-signature-auto-activate nil
 lsp-signature-render-documentation nil)

;; Sometimes, we would prefer to use `flycheck' to lint the buffer instead of
;; LSP mode. This can create problems because the LSP UI clashes with the
;; Flycheck UI. To make sure flycheck wins, one can run:
;;
;; (setq-local lsp-diagnostics-provider :none
;; 	       lsp-modeline-diagnostics-enable nil)
;;
;; In the major mode hook for a particular buffer. 
(use-package lsp-mode)

;; Web Mode Formatting
(use-package web-mode)
(use-package prettier-js :demand t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(defun jlib/web-mode-hook ()
  "Hook for entering and editing web mode files."
  (setq
   web-mode-auto-close-style 2
   web-mode-markup-indent-offset 2
   web-mode-enable-auto-quoting nil
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2)
  (define-key web-mode-map (kbd "C-c p") #'prettier-js)
  ;; Treat ' as a string. I spent *years* trying to figure out how to make
  ;; web-mode do this correctly, and I only found the solution after I tried
  ;; to write a major mode of my own. You live and you learn :)
  (modify-syntax-entry ?' "\"" web-mode-syntax-table))

(add-hook 'web-mode-hook #'jlib/web-mode-hook)

;; Ruby/Rails
(use-package inf-ruby)

(add-to-list
 'display-buffer-alist
 `("ruby"
   (display-buffer-at-bottom)
   (window-height . 0.25)))

(defun jlib/ruby-mode-hook ()
  "Hook for editing Ruby code."
  (interactive)
  (setq-local
   lsp-diagnostics-provider :auto
   lsp-modeline-diagnostics-enable t)
  (define-key ruby-mode-map (kbd "C-c C-c") #'ruby-send-buffer)
  (define-key ruby-mode-map (kbd "C-c p") #'lsp-format-buffer))

(add-hook 'ruby-mode-hook #'jlib/ruby-mode-hook)

;; YAML Files
(use-package yaml-mode)

;; Dotfile Management
(use-package homer
  :demand t
  :straight (homer :type git :host github :repo "joshuaharry/homer"))

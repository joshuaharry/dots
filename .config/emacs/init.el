;;; package --- Summary: -*- lexical-binding: t -*-

;;; VANILLA CONFIGURATION
;;; ------------------------------------------------------

;; Always run Emacs in fullscreen mode.
(set-frame-parameter nil 'fullscreen 'fullboth)

;; Global settings
(setq mac-right-command-modifier 'control
      mac-command-modifier 'meta
      show-paren-delay 0
      js-indent-level 2
      ring-bell-function #'ignore
      insert-directory-program "/opt/homebrew/bin/gls"
      backup-directory-alist '(("." . "~/.config/emacs/backups"))
      lock-file-name-transforms '((".*" "~/.config/emacs/lockfiles" t))
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

(defun duckduckgo ()
  "Go to duckduckgo.com inside of Emacs."
  (interactive)
  (eww "https://duckduckgo.com"))

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
(defvar *jlib/window-adjustment-amount* 10
  "Control how large window adjustments are.")

(defun jlib/push-border-left ()
  "Push the border of the current window I am editing to the left."
  (interactive)
  (when (> (length (window-list)) 1)
    (if (window-right (get-buffer-window))
        (enlarge-window-horizontally *jlib/window-adjustment-amount*)
      (shrink-window-horizontally *jlib/window-adjustment-amount*))))

(defun jlib/push-border-right ()
  "Push the border of the current window I am editing to the right."
  (interactive)
  (when (> (length (window-list)) 1)
    (if (window-left (get-buffer-window))
        (enlarge-window-horizontally *jlib/window-adjustment-amount*)
      (shrink-window-horizontally *jlib/window-adjustment-amount*))))

(defun jlib/push-border-down ()
  "Push the border of the current window I am editing downwards."
  (interactive)
  (when (> (length (window-list)) 1)
    (if (window-in-direction 'below)
        (enlarge-window *jlib/window-adjustment-amount*)
      (shrink-window *jlib/window-adjustment-amount*))))

(defun jlib/push-border-up ()
  "Push the border of the current window I am editing upwards."
  (interactive)
  (when (> (length (window-list)) 1)
    (if (window-in-direction 'above)
        (enlarge-window *jlib/window-adjustment-amount*)
      (shrink-window *jlib/window-adjustment-amount*))))

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

;; Buffer Management
(defun reset ()
  "Reset all of the buffers in Emacs."
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (dolist (cur (buffer-list))
      (kill-buffer cur))))

;; LaTeX Editing Config
(defun jlib/latex-mode ()
  "Settings for editing LaTeX code."
  (interactive)
  (define-key latex-mode-map (kbd "C-j") #'set-mark-command))

(add-hook 'latex-mode-hook #'jlib/latex-mode)

;; Emacs LISP Editing Config
(defun jlib/indent-lisp ()
  "Indent the buffer in a LISP-y way for me."
  (interactive)
  (indent-region (point-min) (point-max))
  (message "Buffer indented successfully."))

(defun jlib/lisp-interaction-mode ()
  "My personal settings for LISP interaction mode."
  (interactive)
  (define-key lisp-interaction-mode-map (kbd "C-j") #'set-mark-command))

(add-hook 'lisp-interaction-mode-hook #'jlib/lisp-interaction-mode)

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
(defvar *jlib/terminal-function* #'ansi-term
  "Function for summoning a terminal emulator.")

(defun jlib/shell ()
  "Quickly open up a terminal using the specified `*jlib/terminal-function'."
  (interactive)
  (funcall *jlib/terminal-function* (getenv "SHELL")))

(defun jlib/terminal-right ()
  "Open a terminal to the right."
  (interactive)
  (split-window-right)
  (windmove-right)
  (jlib/shell))

(defun jlib/terminal-left ()
  "Open a terminal to the left."
  (interactive)
  (split-window-right)
  (jlib/shell))

(defun jlib/terminal-down ()
  "Open a terminal below."
  (interactive)
  (split-window-below)
  (windmove-down)
  (jlib/shell))

(defun jlib/terminal-up ()
  "Open a terminal above."
  (interactive)
  (split-window-below)
  (jlib/shell))

(global-set-key (kbd "C-c t t") #'jlib/shell)
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
(use-package selectrum)
(selectrum-mode)

(use-package ctrlf)
(ctrlf-mode)

(use-package company
  :config
  (setq company-idle-delay nil)
  (global-set-key (kbd "M-n") #'company-complete)
  ;; Overwrite this warning; we don't need it.
  (defun company--warn-changed-binding () nil))

(global-company-mode)

;; Linting
(use-package flycheck)

;; Git Integration
(use-package magit)

;; Fuzzy Finding
(use-package rg)

;; Snippets
(use-package yasnippet)
(yas-global-mode)

;; Multiple Cursors
(use-package multiple-cursors)
(global-set-key (kbd "C-c s") #'mc/edit-lines)

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
(use-package lsp-mode
  :config (add-to-list 'lsp-language-id-configuration '(".*\\.erb$" . "html")))

;; LISP Development
(use-package rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)

;; Clojure
(use-package cider
  :config (setq cider-clojure-cli-aliases ":test:user"))
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

;; Common LISP
(use-package sly)

(defun jlib/sly-mode-hook ()
  (interactive)
  "Hook for editing code in SLY"
  (define-key sly-editing-mode-map (kbd "M-n") #'company-complete))

(add-hook 'sly-mode-hook #'jlib/sly-mode-hook)

;; YAML Files
(use-package yaml-mode)

;; JSON files
(use-package json-mode)

;; Docker
(use-package docker)
(use-package dockerfile-mode)

;; Better Terminal Emulation
(use-package vterm)
(setq *jlib/terminal-function* #'vterm)

;; Documentation
(use-package olivetti)
(use-package markdown-mode)

(defun jlib/writing-mode-hook ()
  "Custom hook for writing plain documents."
  (interactive)
  (display-line-numbers-mode)
  (olivetti-mode))

(add-hook 'markdown-mode-hook #'jlib/writing-mode-hook)

;; Golang
(use-package go-mode)
(add-hook 'go-mode-hook #'lsp)

;; Python
(add-hook 'python-mode-hook #'lsp)

;; Unset Python mode's default keybindings.
(with-eval-after-load 'python
  (setq python-mode-map (make-sparse-keymap)))

;; Terraform
(use-package terraform-mode)
(add-hook 'terraform-mode-hook #'lsp)

;; C Code
(defun jlib/c-mode-hook ()
  (interactive)
  "Hook for when I'm editing C code."
  (flycheck-mode))

(add-hook 'c-mode-hook #'jlib/c-mode-hook)
(add-hook 'c++-mode-hook #'jlib/c-mode-hook)

(defun jlib/rust-mode-hook ()
  "Hook for editing rust files."
  (flycheck-mode)
  (lsp))

(add-hook 'rust-mode-hook #'lsp)

;; Ruby/Rails
(use-package inf-ruby
  :config
  (define-key inf-ruby-mode-map (kbd "M-n") #'company-complete))

(add-to-list
 'display-buffer-alist
 `("ruby"
   (display-buffer-at-bottom)
   (window-height . 0.25)))

(add-to-list
 'display-buffer-alist
 `("rails"
   (display-buffer-at-bottom)
   (window-height . 0.25)))

(defun jlib/ruby-mode-hook ()
  "Hook for editing Ruby code."
  (interactive)
  (define-key ruby-mode-map (kbd "C-c C-c") #'ruby-send-buffer)
  (lsp))

(add-hook 'ruby-mode-hook #'jlib/ruby-mode-hook)

;; Web Mode Formatting
(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(setq web-mode-enable-current-element-highlight t)

(defun jlib/web-mode-hook ()
  "Hook for entering and editing web mode files."
  ;; Treat ' as a string. I spent *years* trying to figure out how to make
  ;; web-mode do this correctly, and I only found the solution after I tried
  ;; to write a major mode of my own. You live and you learn :)
  (modify-syntax-entry ?' "\"" web-mode-syntax-table)
  (modify-syntax-entry ?` "\"" web-mode-syntax-table)
  (setq
   web-mode-auto-close-style 2
   web-mode-markup-indent-offset 2
   web-mode-enable-auto-quoting nil
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2)
  (lsp))

(add-hook 'web-mode-hook #'jlib/web-mode-hook)

;; Ocaml
(use-package tuareg)
(use-package merlin)
(add-hook 'tuareg-mode-hook #'merlin-mode)

;; Racket
(use-package racket-mode)

(defun jlib/racket-mode-hook ()
  "Hook for starting Racket mode."
  (racket-start-back-end)
  (setq racket-show-functions '(racket-show-echo-area))
  (racket-xp-mode))

(add-hook 'racket-mode-hook #'jlib/racket-mode-hook)

;; Dotfile Management
(use-package homer
  :demand t
  :straight (homer :type git :host github :repo "joshuaharry/homer"))

;; Direnv Integration
(use-package direnv)

;; Code Formatting
(use-package efmt
  :demand t
  :straight (efmt :type git :host github :repo "joshuaharry/efmt"))

(setq *efmt-format-alist*
      `((web-mode ("prettier" "-w" "<TARGET>"))
	(yaml-mode ("prettier" "-w" "<TARGET>"))
	(json-mode ("prettier" "-w" "<TARGET>"))
	(markdown-mode ("prettier" "-w" "<TARGET>"))
	(c-mode ("clang-format" "-i" "<TARGET>"))
	(c++-mode ("clang-format" "-i" "<TARGET>"))
	(python-mode ("black" "<TARGET>"))
	(latex-mode ("/opt/homebrew/Cellar/latexindent/3.18/bin/latexindent" "-w" "<TARGET>"))
	("erb" ("htmlbeautifier" "<TARGET>"))
	("ml" ("ocamlformat" "--enable-outside-detected-project" "<TARGET>" "-i"))
	(ruby-mode ,#'lsp-format-buffer)
	("go" ,#'gofmt)
	("rkt" ,#'jlib/indent-lisp)
	("rs" ("rustfmt" "<TARGET>"))
	("lisp" ,#'jlib/indent-lisp)
	("el" ,#'jlib/indent-lisp)
	("edn" ,#'jlib/indent-lisp)
	("clj" ,#'jlib/indent-lisp)))

(global-set-key (kbd "C-c p") #'efmt)

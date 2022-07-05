;; -*- lexical-binding: t -*-

;; These operations take *surpising* amount of time, which is why we're doing
;; them here instead of init.el
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(fringe-mode 0)

(setq
 ;; Prevent Custom from modifying our init files.
 custom-file (expand-file-name
	      (format "custom-%d-%d.el" (emacs-pid) (random)) temporary-file-directory)
 
 ;; Make garbage collection happen less frequently. 
 gc-cons-threshold (* 100 1024 1024)
 ;; Prevent package.el from loading on startup
 package-enable-at-startup nil
 ;; As per https://github.com/emacs-lsp/lsp-mode#performance
 read-process-output-max (* 1024 1024))

;; Load a dark theme from the beginning.
(load-theme 'modus-vivendi)

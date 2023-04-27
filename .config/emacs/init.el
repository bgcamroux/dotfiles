;;  -*- lexical-binding: t; -*-
(setq inhibit-startup-message t   ; turn off the startup message screen
      visible-bell t)             ; enable the visual bell

(scroll-bar-mode -1)              ; disable scrollbar
(tool-bar-mode   -1)              ; disable toolbar
(tooltip-mode    -1)              ; disable tooltips
(menu-bar-mode   -1)              ; disable menu bar
(set-fringe-mode 10)              ; gives us some breathing room

(set-face-attribute 'default nil :font "JetBrainsMonoNL Nerd Font Mono" :height 100)

;; Install straight.el on whatever system this config is used on. It will not attempt to
;; install it twice, so no problems there.
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
      (eval-print-last-sexp))
    (load bootstrap-file nil 'nomessage)))
;; To install a package, include an expression like this:
;; (straight-use-package '<pkgname>)
;; straight.el will then install the package if it hasn't been installed already.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load up Prot's excellent Modus- themes, and set them to toggle between operandi-tinted
;; and vivendi-tinted. Assign the toggle function to <f5> on the keyboard.
(require-theme 'modus-themes)
(setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
(load-theme 'modus-vivendi-tinted 1) 
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

;; Initialize package sources
(require 'package)

;; Setup the package archives to use MELPA, ORG, and ELPA
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-delimiters latex-extra latex-table-wizard mu2tex org-bullets org-d20 org-superstar modus-themes auctex orca)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

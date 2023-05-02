;;  -*- lexical-binding: t; -*-
(setq inhibit-startup-message t      ; turn off the startup message screen
      visible-bell t)                ; enable the visual bell

(scroll-bar-mode -1)                 ; disable scrollbar
(tool-bar-mode   -1)                 ; disable toolbar
(tooltip-mode    -1)                 ; disable tooltips
(menu-bar-mode   -1)                 ; disable menu bar
(set-fringe-mode 10)                 ; gives us some breathing room

(set-face-attribute 'default nil :font "JetBrainsMonoNL Nerd Font Mono" :height 100)

;; Install elpaca package manager
(defvar elpaca-installer-version 0.3)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode)                 ; enable :elpaca use-package keyword
  (setq elpaca-use-package-by-default t))   ; assume :elpaca t unless otherwise specified

; block until current queue is processed - this will allow use of use-package right away
(elpaca-wait)

(use-package modus-themes)
(use-package i3wm-config-mode)
(use-package rainbow-delimiters)
(use-package org-superstar)
(use-package orca)
(use-package org-d20)
(use-package latex-extra)
(use-package latex-table-wizard)
(use-package mu2tex)
(use-package auctex)

; block until queue is processed - allows use of these packages
(elpaca-wait)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load up Prot's excellent Modus- themes, and set them to toggle between operandi-tinted
;; and vivendi-tinted. Assign the toggle function to <f5> on the keyboard.
(require-theme 'modus-themes)
(setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
(load-theme 'modus-vivendi-tinted t)
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)


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

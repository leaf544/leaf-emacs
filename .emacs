;; PREQS

(require 'cl)
(require 'org) ;; Org mode goodies
(require 'package) ;; Package, MELPA

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

 ;; GLOBALS VARIABLES

(defvar WRITING nil)
(defvar MODE-LINE t)
(defvar LINES t)
(defvar START-SCREEN t)
(defvar INCREASE 3)
(defvar MOTIVATION-PICS-PATH "F:/motivation")
(defvar MARKED nil)

;; DEFUN

(defun compile-cpp-gpp ()
  "Compiles cpp programs via the GCC compiler"
  (interactive)
  (let ((compile-mode (read-string "Compile Mode: "))
        (links)
        (compile-command))
    (cond
     ((string= compile-mode "0")
      (setq compile-command (concat "g++ " (buffer-file-name) " -o main_exec.exe && start main_exec.exe"))
      ;;(print OUTPUT-BODY)
      (shell-command compile-command))
     ((string= compile-mode "1")
      (setq links (read-string "Links: "))
      (setq compile-command (concat "g++ " (buffer-file-name) " " links " -o main_exec.exe && start main_exec.exe"))
      (shell-command compile-command)))))

(defun explorer-here ()
  "Opens up the explorer in your current directory"
  (interactive)
  (shell-command "explorer ."))

(defun except-close ()
  "Kills all buffers except the one you're working in, useful for organizing" 
  (interactive)
  (let ((work-buffer (buffer-name))
        marked-buffer)
    (next-buffer)
    (print work-buffer)
    (while (not (string= (buffer-name) work-buffer))
      (setq marked-buffer (get-buffer (buffer-name)))
      (print marked-buffer)
      (kill-buffer marked-buffer)
      (next-buffer))))

(defun updated-revert ()
  "For dired"
  (interactive)
  (revert-buffer t t))

(defun motivate-me ()
  (interactive)
  (let ((temp t))
    (dolist (file (reverse (directory-files "F:/motivation")))
      (when (not (string-equal (substring file 0 1) "."))
        (find-file (concat MOTIVATION-PICS-PATH "/" file))))))

(defun mark-open ()
  (interactive)
  (setq MARKED t)

  )

;; DEFUN END

;; STARTUP

(when t
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq c-set-style "k&r")
  (setq c-basic-offset 4)

  (electric-pair-mode t) ;; Automatic pair closing
  (setq ring-bell-function 'ignore) ;; Turn off windows sounds
  (set-fringe-mode 0)

  (tool-bar-mode -1) ;; Disable tool bar
  (menu-bar-mode -1) ;; Disable menu bar
  (toggle-scroll-bar -1) ;; Disable scroll bar

  (unless MODE-LINE
    (setq-default mode-line-format nil))

  (when LINES
    (global-display-line-numbers-mode))

  (kill-buffer "*scratch*")
  (kill-buffer "*Messages*")

  (toggle-frame-maximized) ;; Maximize emacs on startup

  ;; Kill irrelevant buffers
  (setq inhibit-startup-message t) 
  (setq initial-scratch-message nil)
  (setq make-backup-files nil)

  (global-auto-revert-mode t) ;; Automatic buffer revert 

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 120 :width normal :size 23)))))

  ;; EVENT BINDERS AND AWAKENERS

  (global-set-key (kbd "M-s") 'compile-cpp-gpp)
  (global-set-key (kbd "C-;") 'explorer-here)
  (global-set-key [(meta up)] 'except-close)
  (global-set-key (kbd "C-h C-j") 'updated-revert)
  (global-set-key (kbd "C-=") 'delete-other-windows)
  (global-set-key [(meta down)] 'motivate-me)

  (when START-SCREEN
    (setq org-startup-folded t)
    (use-package org-bullets
      :ensure t
      :init
      (add-hook 'org-mode-hook (lambda ()
                                 (org-bullets-mode 1))))
    (find-file "~/main.org")
    (find-file "~/start.org")
    (text-scale-increase INCREASE)
    (setq org-return-follows-link t)
    (transient-mark-mode t)
    (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
    (setq org-hide-emphasis-markers t)
    ;; Probably temporary
    (split-window-right)
    (switch-to-buffer-other-window "main.org")
    (text-scale-increase INCREASE))
  (load-theme 'wheatgrass))



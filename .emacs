;; PREQS
(require 'cl-lib) ;; CL
(require 'f) ;; For file related functions
(require 'org) ;; Org mode goodies
(require 'package) ;; Package, MELPA

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

 ;; GLOBALS VARIABLES
(defvar MODE-LINE t)
(defvar LINES nil)
(defvar THEME 'migrainity)
(defvar MARKED nil)

;; FUNCTIONS BEGIN
(defun leaf/compile-cpp-gpp ()
  "Compiles cpp programs via the GCC compiler"
  (interactive)
  (let ((compile-mode (read-string "Compile Mode: "))
        (links)
        (compile-command))
    (cond
     ((string= compile-mode "0")
      (setq compile-command (concat "g++ " (buffer-file-name) " -o main_exec.exe && start main_exec.exe"))
      (async-shell-command compile-command))
     ((string= compile-mode "1")
      (setq links (read-string "Links: "))
      (setq compile-command (concat "g++ " (buffer-file-name) " " links " -o main_exec.exe && start main_exec.exe"))
      (async-shell-command compile-command)))))

(defun leaf/explorer-here ()
  "Opens up the explorer in your current directory"
  (interactive)
  (shell-command "explorer ."))

(defun leaf/except-close ()
  "Kills all buffers except the one you're working in, useful for organizing" 
  (interactive)
  (let ((work-buffer (buffer-name))
        marked-buffer)
    (next-buffer)
    (while (not (string= (buffer-name) work-buffer))
      (setq marked-buffer (get-buffer (buffer-name)))
      (kill-buffer marked-buffer)
      (next-buffer)))
  (message "%s" "Killed all buffers")
  (kill-buffer "*Messages*"))

(defun leaf/updated-revert ()
  "For dired"
  (interactive)
  (revert-buffer t t))

(defun leaf/motivate-me ()
  "Helps me turn into a sigma"
  (interactive)
  (let ((temp t))
    (dolist (file (reverse (directory-files "F:/motivation")))
      (when (not (string-equal (substring file 0 1) "."))
        (find-file (concat "F:/motivation" "/" file))))))

(defun leaf/mark-open ()
  "Mark open"
  (interactive)
  (setq MARKED t)
  (shell-command (concat "echo " (buffer-file-name) " > " "f:/emacs/last.txt " (number-to-string text-scale-mode-amount) " "(number-to-string (line-number-at-pos)))))

(defun leaf/org-init ()
  "Org customizations"
  (set-face-foreground 'org-todo "#A9A8E8")
  (use-package org-bullets :ensure t :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (setq org-hide-emphasis-markers t)
  (setq org-return-follows-link t)
  (transient-mark-mode t)
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (add-hook 'org-mode-hook 'leaf/sensible-zoom))

(defun leaf/sensible-zoom ()
  "Zoom into files according to common sense"
  (let ((nline (line-number-at-pos (point-max))))
    (when (> (/ nline 61) 1)
      (dotimes (i (/ nline 61))
        (text-scale-increase 0.4)))
    (unless (> (/ nline 61) 1)
      (text-scale-increase 2))))

;; FUNCTIONS END

;;STARTUP
(when t
  
  (if THEME
      (load-theme THEME t))
  
  (leaf/org-init)

  ;; Free area
  (set-face-attribute 'info-header-xref nil :foreground "#00000")
  (set-face-attribute 'info-node nil :family "Jetbrains Mono NL")
  (set-face-attribute 'info-title-1 nil :family "Jetbrains Mono NL" :height 1.1)

  (setq exercise-mode-syntax-highlights
        '(("#INT\\|#CHAR\\|#BOOL" . font-lock-type-face)
          ("%" . font-lock-string-face)))
  
  (define-derived-mode exercise-mode fundamental-mode "Exercise-Mode"
    "Lose that APT, dawg."
    (setq font-lock-defaults '(exercise-mode-syntax-highlights)))

  (add-to-list 'auto-mode-alist '("\\.newage\\'" . exercise-mode))
  
  ;; Mark open business
  (when (> (string-width (f-read-text "~/last.txt")) 2)
    (setq read-text (f-read-text "~/last.txt"))
    (setq values (split-string read-text))
    (find-file (car values))
    (text-scale-increase (string-to-number (nth 1 values)))
    (goto-line (string-to-number (nth 2 values)))    
    (setq MARKED t)
    (shell-command (concat "break > " "f:/emacs/last.txt")))

  (unless MARKED
    (find-file "~/main.org"))
  
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

  (toggle-frame-maximized) ;; Maximize emacs on startup
  
  (unless MODE-LINE
    (setq-default mode-line-format nil))
  
  (when LINES
    (global-display-line-numbers-mode))
  
  ;; Kill irrelevant buffers
  (setq inhibit-startup-message t) 
  (setq initial-scratch-message nil)
  ;;(setq make-backup-files nil)

  (kill-buffer "*scratch*")
  (setq-default message-log-max nil)
  (kill-buffer "*Messages*")
  
  (global-auto-revert-mode t) ;; Automatic buffer revert 
  
  ;; EVENT BINDERS AND AWAKENERS
  (global-set-key (kbd "M-s") 'leaf/compile-cpp-gpp)
  (global-set-key (kbd "C-;") 'leaf/explorer-here)
  (global-set-key [(meta up)] 'leaf/except-close)
  (global-set-key (kbd "C-h C-j") 'leaf/updated-revert)
  (global-set-key (kbd "C-=") 'delete-other-windows)
  (global-set-key [(meta down)] 'leaf/motivate-me)
  (global-set-key (kbd "C-c C-'") 'leaf/mark-open)
  (global-set-key (kbd "M-[") 'shell)
  (global-set-key (kbd "M-]") 'restart-emacs)
  
  ;; ALIASES
  (defalias 'ec 'leaf/except-close)
  (defalias 're 'restart-emacs)
  (defalias 'mf 'leaf/mark-open)

  ;; HOOKS TO CUSTOM FUNCTIONS
  (add-hook 'emacs-lisp-mode-hook 'leaf/sensible-zoom)
  (add-hook 'c-mode-common-hook 'leaf/sensible-zoom)
  (add-hook 'text-mode-hook 'leaf/sensible-zoom)
  (add-hook 'bookmark-bmenu-mode-hook 'leaf/sensible-zoom))



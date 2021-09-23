;; PREQS

;; Increment: incf (PLACE, &optional VALUE) // requires 'CL
;; strtok equivalent: (split-string STRING)

(require 'cl-lib)
(require 'f)
(require 'org) ;; Org mode goodies
(require 'package) ;; Package, MELPA
(require 'elcord) ;; Discord

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

 ;; GLOBALS VARIABLES

(defvar WRITING t)
(defvar MODE-LINE t)
(defvar LINES nil)
(defvar START-SCREEN nil)
(defvar INCREASE 1)
(defvar THEME nil)
(defvar DECORATE t)
(defvar MOTIVATION-PICS-PATH "F:/motivation")
(defvar MARKED nil)

;; DEFUN

(defun leaf/compile-cpp-gpp ()
  "Compiles cpp programs via the GCC compiler"
  (interactive)
  (let ((compile-mode (read-string "Compile Mode: "))
        (links)
        (compile-command))
    (cond
     ((string= compile-mode "0")
      (setq compile-command (concat "g++ " (buffer-file-name) " -o main_exec.exe && start main_exec.exe"))
      (shell-command compile-command))
     ((string= compile-mode "1")
      (setq links (read-string "Links: "))
      (setq compile-command (concat "g++ " (buffer-file-name) " " links " -o main_exec.exe && start main_exec.exe"))
      (shell-command compile-command)))))

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
        (find-file (concat MOTIVATION-PICS-PATH "/" file))))))

(defun leaf/mark-open ()
  "Mark open"
  (interactive)
  (setq MARKED t)
  (shell-command (concat "echo " (buffer-file-name) " > " "f:/emacs/last.txt " (number-to-string text-scale-mode-amount) " "(number-to-string (line-number-at-pos)))))

(defun leaf/org-init ()
  (setq org-hide-emphasis-markers t)
  (transient-mark-mode t)
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file))

(defun leaf/start-screen ()
  "Start Screen"
  (interactive)
  (when START-SCREEN
    (when (not MARKED)
      (setq org-startup-folded t)
      ;;(find-file "~/apt.org")
      (find-file "~/main.org")
      (find-file "~/start.org")
      (text-scale-increase INCREASE)
      (setq org-return-follows-link t)
      (transient-mark-mode t)
      (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
      (setq org-hide-emphasis-markers t)
      (split-window-right)
      (switch-to-buffer-other-window "main.org")
      (text-scale-increase INCREASE)
      (switch-to-buffer-other-window "start.org")
      (text-scale-increase (+ INCREASE 1))
      (setq INCREASE 0))))

(defun leaf/init-decorate ()
  "Theme and decorations"

  (set-face-attribute 'default nil :family "Consolas" :background "#F5F4E8")
  (set-face-attribute 'mode-line nil :box nil :font "Jetbrains Mono NL" :background "#C9786D" :height 120)
  (set-face-attribute 'mode-line-inactive nil :box nil :background "#5D8A85" :foreground "#00000")
  (set-face-attribute 'minibuffer-prompt nil :weight 'bold :foreground "#710C3F")
  (set-face-attribute 'isearch-fail nil :background "#C7839C")
  (set-face-attribute 'isearch nil :background "#710C3F")
  (set-face-attribute 'font-lock-preprocessor-face nil :foreground "#F63858")
  (set-face-attribute 'font-lock-builtin-face nil :foreground "#F96161")
  (set-face-attribute 'lazy-highlight nil :background "#0017D6")
  
  ;; Default
  ;; (set-face-attribute 'default nil :background "#F5F4E4" :family "Droid Sans Mono" :foreground "#000000" :height 120)
  ;; (set-face-attribute 'region nil :background "#A9A8E8")
  ;; (set-face-foreground 'line-number "#710C3F")
  
  ;; Syntax
  (set-face-attribute 'font-lock-type-face nil :foreground "#703770")
  (set-face-attribute 'font-lock-constant-face nil :foreground "#35455D")
  (set-face-attribute 'font-lock-keyword-face nil :foreground "#703770")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "#000000")
  (set-face-attribute 'font-lock-function-name-face nil :foreground "#000000")
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "#504a4c")
  (set-face-attribute 'font-lock-comment-face nil :foreground "#636061")
  (set-face-attribute 'font-lock-string-face nil :foreground "#703770"))
  
  ;; Mode Line customizations
  ;; (set-face-attribute 'mode-line nil :box nil :font "Jetbrains Mono NL" :background "#42D601" :height 115)
  ;; (set-face-attribute 'mode-line-inactive nil :box nil :background "#01217F" :foreground "#FFFFFF")
  ;; (set-face-attribute 'minibuffer-prompt nil :weight 'bold :foreground "#710C3F")
  ;; (set-face-attribute 'isearch-fail nil :background "#C7839C")
  ;; (set-face-attribute 'isearch nil :background "#710C3F")
  ;; (set-face-attribute 'font-lock-preprocessor-face nil :foreground "#F63858")
  ;; (set-face-attribute 'font-lock-builtin-face nil :foreground "#9A4AB7")
  ;; (set-face-attribute 'lazy-highlight nil :background "#0017D6"))

(defun leaf/custom-decorate ()
  "Theme and decorations"
  
  (set-face-attribute 'default nil :family "Consolas" :background "#12151a" :foreground "#dfe4eb" :weight 'normal :height 115)
  (set-face-attribute 'mode-line nil :box nil :font "Jetbrains Mono NL" :background "#A8CCBA" :height 120)
  (set-face-attribute 'mode-line-inactive nil :box nil :background "#A9A8E8" :foreground "#000000")
  (set-face-attribute 'minibuffer-prompt nil :weight 'bold :foreground "#A8CCBA")
  (set-face-attribute 'isearch-fail nil :background "#FF3A1D")
  (set-face-attribute 'isearch nil :background "#710C3F")
  (set-face-attribute 'font-lock-preprocessor-face nil :foreground "#A8CCBA")
  (set-face-attribute 'font-lock-builtin-face nil :foreground "#A9A8E8")
  (set-face-attribute 'lazy-highlight nil :background "#0017D6")
  
  ;; Default
  ;; (set-face-attribute 'default nil :background "#F5F4E4" :family "Droid Sans Mono" :foreground "#000000" :height 120)
  (set-face-attribute 'region nil :background "#153335")
  ;; (set-face-foreground 'line-number "#710C3F")
  
  ;; Syntax
  (set-face-attribute 'font-lock-type-face nil :foreground "#b6dce3" :weight 'bold)
  (set-face-attribute 'font-lock-constant-face nil :foreground "#A8CCBA")
  (set-face-attribute 'font-lock-keyword-face nil :foreground "#b6dce3")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "#59b0c0")
  (set-face-attribute 'font-lock-function-name-face nil :foreground "#59b0c0")
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "#504a4c")
  (set-face-attribute 'font-lock-comment-face nil :foreground "#636061")
  (set-face-attribute 'font-lock-string-face nil :foreground "#6A8D96"))
  
  ;; Mode Line customizations
  ;; (set-face-attribute 'mode-line nil :box nil :font "Jetbrains Mono NL" :background "#42D601" :height 115)
  ;; (set-face-attribute 'mode-line-inactive nil :box nil :background "#01217F" :foreground "#FFFFFF")
  ;; (set-face-attribute 'minibuffer-prompt nil :weight 'bold :foreground "#710C3F")
  ;; (set-face-attribute 'isearch-fail nil :background "#C7839C")
  ;; (set-face-attribute 'isearch nil :background "#710C3F")
  ;; (set-face-attribute 'font-lock-preprocessor-face nil :foreground "#F63858")
  ;; (set-face-attribute 'font-lock-builtin-face nil :foreground "#9A4AB7")
  ;; (set-face-attribute 'lazy-highlight nil :background "#0017D6"))


(defun sensible-zoom ()
  "Zoom into files according to common sense"
  (dotimes (i (/ (line-number-at-pos (point-max)) 61))
    (text-scale-increase 0.4)))

;; DEFUN END

;; STARTUP

(when t
  
  (leaf/org-init)
  
  (if DECORATE
      (leaf/custom-decorate))
  
  ;;(elcord-mode)
  
  (use-package org-bullets
    :ensure t
    :init
    (add-hook 'org-mode-hook (lambda ()
                               (org-bullets-mode 1))))
  
  (find-file "~/")
  
  ;; Mark open business
  (when (> (string-width (f-read-text "~/last.txt")) 2)
    (setq read-text (f-read-text "~/last.txt"))
    (setq values (split-string read-text))
    (find-file (car values))
    (text-scale-increase (string-to-number (nth 1 values)))
    (goto-line (string-to-number (nth 2 values)))    
    (setq MARKED t)
    (shell-command (concat "break > " "f:/emacs/last.txt")))
  
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

  ;; Base customizations
  ;; (set-face-attribute 'default nil :family "Droid Sans Mono" :background "#F5F4E8")
  ;; (set-face-attribute 'mode-line nil :box nil :font "Jetbrains Mono NL" :background "#C9786D" :height 115)
  ;; (set-face-attribute 'mode-line-inactive nil :box nil :background "#5D8A85" :foreground "#00000")
  ;; (set-face-attribute 'minibuffer-prompt nil :weight 'bold :foreground "#710C3F")
  ;; (set-face-attribute 'isearch-fail nil :background "#C7839C")
  ;; (set-face-attribute 'isearch nil :background "#710C3F")
  ;; (set-face-attribute 'font-lock-preprocessor-face nil :foreground "#F63858")
  ;; (set-face-attribute 'font-lock-builtin-face nil :foreground "#9A4AB7")
  ;; (set-face-attribute 'lazy-highlight nil :background "#0017D6")
  
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
  
  ;; EVENT BINDERS AND AWAKENERS
  
  (global-set-key (kbd "M-s") 'leaf/compile-cpp-gpp)
  (global-set-key (kbd "C-;") 'leaf/explorer-here)
  (global-set-key [(meta up)] 'leaf/except-close)
  (global-set-key (kbd "C-h C-j") 'leaf/updated-revert)
  (global-set-key (kbd "C-=") 'delete-other-windows)
  (global-set-key [(meta down)] 'leaf/motivate-me)
  (global-set-key (kbd "C-c C-'") 'leaf/mark-open)
  (global-set-key (kbd "M-p") 'leaf/start-screen)
  (global-set-key (kbd "M-[") 'shell)
  (global-set-key (kbd "M-]") 'restart-emacs)

  (defalias 'ec 'leaf/except-close)
  
  ;; HOOKS TO CUSTOM FUNCTIONS

  (add-hook 'c-mode-common-hook 'sensible-zoom)
  
  (if THEME
      (load-theme THEME t))
  
  ;; Weird ass shit man

  ;; (when (not MARKED)
  ;;   (when (string-equal (buffer-name) "*scratch*")
  ;;   (setq lol (string-equal (read-string "Open start screen? ") "y"))
  ;;   (when lol
  ;;     (setq START-SCREEN t))))

  ;; Org
    (leaf/start-screen))

;; Lisp

;; PREQS

(require 'f)
(require 'org) ;; Org mode goodies
(require 'package) ;; Package, MELPA

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

 ;; GLOBALS VARIABLES

(defvar WRITING nil)
(defvar MODE-LINE t)
(defvar LINES nil)
(defvar START-SCREEN nil)
(defvar INCREASE 1)
(defvar THEME 'wheatgrass)
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
  (shell-command (concat "echo " (buffer-file-name) " > " "f:/emacs/last.txt " (number-to-string text-scale-mode-amount))))

(defun leaf/start-screen ()
  (interactive)
  (when START-SCREEN
    (when (not MARKED)
      (setq org-startup-folded t)
      (find-file "~/apt.org")
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
      (split-window-below)
      (switch-to-buffer-other-window "apt.org")
      (text-scale-increase (+ INCREASE 1))
      (switch-to-buffer-other-window "start.org")
      (setq INCREASE 0))))
  
;; DEFUN END

;; STARTUP

(when t

  (require 'elcord)
  (elcord-mode)


  (use-package org-bullets
    :ensure t
    :init
    (add-hook 'org-mode-hook (lambda ()
                               (org-bullets-mode 1))))
  
  (when (> (string-width (f-read-text "~/last.txt")) 2)
    (setq read-text (f-read-text "~/last.txt"))
    (setq str (substring read-text 0 (- (string-width read-text) 3)))
    (message "%s" str)
    (find-file str)
    (setq increase-amount (string-to-number (substring read-text (- (string-width read-text) 1) (string-width read-text))))
    ;;(setq increase-amount (- increase-amount 2))
    (message "%d" increase-amount)
    (text-scale-increase increase-amount)
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

  (set-face-attribute 'mode-line nil :font "Jetbrains Mono NL")
  
  (unless MODE-LINE
    (setq-default mode-line-format nil))

  (when LINES
    (global-display-line-numbers-mode))
  
  (kill-buffer "*scratch*")
  ;;(kill-buffer "*Messages*")

  (toggle-frame-maximized) ;; Maximize emacs on startup

  ;; Kill irrelevant buffers
  (setq inhibit-startup-message t) 
  (setq initial-scratch-message nil)
  (setq make-backup-files nil)
  
  (global-auto-revert-mode t) ;; Automatic buffer revert 

  (custom-set-faces
   '(default ((t (:family "Droid Sans Mono" :foundry "outline" :slant normal :weight normal :height 120 :width normal :size b23)))))

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
  (global-set-key (kbd "M-]") 'switch-to-buffer-other-frame)

  (load-theme THEME)
  
  ;; Weird ass shit man

  (when (not MARKED)
    (setq lol (string-equal (read-string "Open start screen? ") "y"))
    (when lol
      (setq START-SCREEN t)))
  
  (leaf/start-screen))

;; Lisp
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#21252B" "#E06C75" "#98C379" "#E5C07B" "#61AFEF" "#C678DD" "#56B6C2" "#ABB2BF"])
 '(custom-safe-themes
   '("5b7c31eb904d50c470ce264318f41b3bbc85545e4359e6b7d48ee88a892b1915" "1fbd63256477789327fe429bd318fb90a8a42e5f2756dd1a94805fc810ae1b62" "18cd5a0173772cdaee5522b79c444acbc85f9a06055ec54bb91491173bc90aaa" default))
 '(package-selected-packages
   '(plan9-theme zeno-theme zenburn-theme ws-butler winum which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package undo-tree ubuntu-theme toxi-theme toc-org terminal-here suscolors-theme spaceline solarized-theme smex smart-mode-line-powerline-theme restart-emacs request rainbow-delimiters popwin persp-mode pdf-tools pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text magit macrostep lorem-ipsum linum-relative link-hint ivy-hydra indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-make gruvbox-theme google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav elcord dumb-jump diminish define-word dakrone-light-theme counsel-projectile company column-enforce-mode clean-aindent-mode chocolate-theme calfw bliss-theme birds-of-paradise-plus-theme berrys-theme autumn-light-theme auto-highlight-symbol auto-compile atom-dark-theme async arc-dark-theme aggressive-indent adjust-parens adaptive-wrap ace-window ace-link abyss-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Droid Sans Mono" :foundry "outline" :slant normal :weight normal :height 120 :width normal :size b23)))))



(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(package-initialize)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

; packages
(use-package evil)
(use-package labburn-theme)

; configuration
(evil-mode 1)

(define-key evil-normal-state-map (kbd "H")   'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "S")   'evil-end-of-line)

(global-set-key (kbd "C-n")  'evil-window-up)
(global-set-key (kbd "C-t")  'evil-window-down)
(global-set-key (kbd "C-h")  'evil-window-left)
(global-set-key (kbd "C-s")  'evil-window-right)
(global-set-key (kbd "C-\\") 'split-window-horizontally)
(global-set-key (kbd "C-\-") 'split-window-vertically)

(evil-define-key 'visual evil-surround-mode-map "s" 'evil-substitute)
(evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)

(global-set-key (kbd "C-a")  'align-regexp)

(setq visible-bell 1)
(setq inhibit-startup-screen t)

(set-face-attribute 'default t :font "Hasklig-16")

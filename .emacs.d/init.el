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
(use-package evil-surround)
(use-package labburn-theme)
(use-package company
  :bind (("<tab>" . company-complete)))

; evil configuration
(evil-mode 1)
(global-evil-surround-mode 1)
(global-company-mode t)

(define-key evil-normal-state-map (kbd "H")   'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "S")   'evil-end-of-line)

(global-set-key (kbd "C-n")  'evil-window-up)
(global-set-key (kbd "C-t")  'evil-window-down)
(global-set-key (kbd "C-h")  'evil-window-left)
(global-set-key (kbd "C-s")  'evil-window-right)
(global-set-key (kbd "C-\\") 'split-window-horizontally)
(global-set-key (kbd "C-\-") 'split-window-vertically)

(define-key evil-visual-state-map "s" 'evil-substitute)
(define-key evil-visual-state-map "S" 'evil-surround-region)
(define-key evil-visual-state-map "h" 'evil-backward-char)
(define-key evil-visual-state-map "t" 'evil-next-line)
(define-key evil-visual-state-map "n" 'evil-previous-line)
(define-key evil-visual-state-map "s" 'evil-forward-char)

(define-key evil-normal-state-map (kbd "h") 'backward-char)
(define-key evil-normal-state-map (kbd "t") 'evil-next-line)
(define-key evil-normal-state-map (kbd "n") 'previous-line)
(define-key evil-normal-state-map (kbd "s") 'forward-char)
(define-key evil-normal-state-map (kbd "<return>") 'evil-insert-newline-below)
(define-key evil-normal-state-map (kbd "S-<return>") 'evil-insert-newline-above)
; (define-key evil-normal-state-map "k" 'kill-line)
; (define-key evil-normal-state-map
;  "K" #'(lambda () (interactive)
          ; "kill from point to the beginning of the line"
          ; (kill-line 0)))
; (define-key evil-normal-state-map "t" 'join-line)
(define-key evil-normal-state-map
  "T" #'(lambda () (interactive)
          "join this line at the end of the line below"
          (join-line 1)))

; configuration

(global-set-key (kbd "C-a")  'align-regexp)

(setq visible-bell 1)
(setq inhibit-startup-screen t)

(set-face-attribute 'default t :font "Hasklig-16")

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

(setq package-enable-at-startup nil)
(package-initialize)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; packages
(use-package evil)
(use-package evil-surround)
(use-package company)
(use-package helm)
(use-package helm-ls-git)
(use-package which-key)
(use-package editorconfig)
(use-package rainbow-delimiters)
(use-package lsp-mode)
(use-package company-lsp)

;; language specific
(use-package tuareg) ;; OCaml
(use-package merlin) ;; OCaml
(use-package rust-mode)
(use-package racket-mode)
(use-package haskell-mode)
(use-package glsl-mode)
(use-package clang-format)
(use-package lsp-clangd
  :hook
  ((c-mode . lsp-clangd-c-enable)
   (c++-mode . lsp-clangd-c++-enable)
   (objc-mode . lsp-clangd-objc-enable)))
(use-package nix-mode)


;; for fun
					; (use-package elcord)

;; themes
(use-package base16-theme)
(load-theme 'base16-mocha t)

;; Set the cursor color based on the evil state
(defvar my/base16-colors base16-mocha-colors)
(setq evil-emacs-state-cursor   `(,(plist-get my/base16-colors :base0D) box)
      evil-insert-state-cursor  `(,(plist-get my/base16-colors :base0D) bar)
      evil-motion-state-cursor  `(,(plist-get my/base16-colors :base0E) box)
      evil-normal-state-cursor  `(,(plist-get my/base16-colors :base0B) box)
      evil-replace-state-cursor `(,(plist-get my/base16-colors :base08) bar)
      evil-visual-state-cursor  `(,(plist-get my/base16-colors :base09) box))

;; smooth scroll
(use-package smooth-scroll
  :config
  (smooth-scroll-mode 1)
  (setq smooth-scroll/vscroll-step-size 2)
  )

;; load external configs
(add-to-list 'load-path "~/.emacs.d/extras")
					; (require 'iosevka)

;; enable all things
(evil-mode 1)
(global-evil-surround-mode 1)
(global-company-mode t)
(helm-mode 1)
(which-key-mode)
(editorconfig-mode 1)
					; (elcord-mode)

;; add LSP to company autocomplete
(push 'company-lsp company-backends)

;; hook up lsp to rust
(add-hook 'rust-mode-hook #'lsp)

;; hook up rainbow delimiters
(add-hook 'emacs-lisp-mode-hook	'rainbow-delimiters-mode)
(add-hook 'racket-mode-hook	'rainbow-delimiters-mode)

;; setup haskell
(setq haskell-stylish-on-save t)

;; setup c
(setq c-default-style "linux"
      c-basic-offset 8)

;; setup c++
(setq clang-format-style-option "llvm")

(defun my-c++-mode-hook ()
  (setq c-basic-offset 8)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; setup glsl
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;;
;; all the keybinds
;;

(global-set-key (kbd "C-SPC")	'helm-M-x)
(global-set-key (kbd "C-a")	'align-regexp)

(define-key evil-normal-state-map (kbd "C-/") 'helm-imenu)

(define-key evil-insert-state-map (kbd "<backtab>") 'company-complete-common-or-cycle)

(define-key evil-motion-state-map (kbd "l") 'evil-find-char-to)
(define-key evil-motion-state-map (kbd "L") 'evil-find-char-to-backward)
(define-key evil-motion-state-map (kbd "k") 'evil-search-next)
(define-key evil-motion-state-map (kbd "K") 'evil-search-previous)

(define-key evil-visual-state-map (kbd "L")	'evil-surround-region)
(define-key evil-visual-state-map (kbd "H")	'evil-first-non-blank)
(define-key evil-visual-state-map (kbd "S")	'evil-end-of-line)

(define-key evil-normal-state-map (kbd "H")	'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "S")	'evil-end-of-line)

(define-key evil-motion-state-map (kbd "C-f") 'helm-ls-git-ls)

(define-key evil-insert-state-map (kbd "C-i")	'evil-normal-state)
(define-key evil-insert-state-map (kbd "<tab>")	'tab-to-tab-stop)

(define-key evil-motion-state-map (kbd "C-e")
  (lambda () (interactive) (evil-scroll-down 0)))
(define-key evil-motion-state-map (kbd "C-u")
  (lambda () (interactive) (evil-scroll-up 0)))
(define-key evil-normal-state-map (kbd "C-p")	'helm-mini)

(global-set-key (kbd "C-S-N")			'evil-window-up)
(global-set-key (kbd "C-S-T")			'evil-window-down)
(global-set-key (kbd "C-S-H")			'evil-window-left)
(global-set-key (kbd "C-S-S")			'evil-window-right)

(define-key evil-normal-state-map (kbd "C-|")	'split-window-horizontally)
(define-key evil-normal-state-map (kbd "C-\-")	'split-window-vertically)

(define-key evil-visual-state-map "h"		'evil-backward-char)
(define-key evil-visual-state-map "t"		'evil-next-line)
(define-key evil-visual-state-map "n"		'evil-previous-line)
(define-key evil-visual-state-map "s"		'evil-forward-char)

(define-key evil-normal-state-map (kbd "h")	'backward-char)
(define-key evil-normal-state-map (kbd "t")	'evil-next-line)
(define-key evil-normal-state-map (kbd "n")	'previous-line)
(define-key evil-normal-state-map (kbd "s")	'forward-char)

(define-key evil-normal-state-map (kbd "<return>")
  (lambda () (interactive)
    (evil-open-below 1)
    (evil-force-normal-state)
    (evil-previous-line 1)))

(define-key evil-normal-state-map (kbd "S-<return>")
  (lambda () (interactive)
    (evil-open-above 1)
    (evil-force-normal-state)
    (evil-next-line 1)))

(define-key evil-normal-state-map
  "T" #'(lambda () (interactive)
	  "join this line at the end of the line below"
	  (join-line 1)))

;; editor settings follow
(setq visible-bell 1)
(setq inhibit-startup-screen t)
(setq jit-lock-defer-time 0)
(setq fast-but-imprecise-scrolling t)

(setq scroll-conservatively 10)
(setq scroll-margin 7)

(add-to-list 'after-make-frame-functions
	     (lambda (frame)
	       (select-frame frame)

	       (push
		'("lambda"  . ?λ) prettify-symbols-alist)

	       (global-prettify-symbols-mode +1)
	       (setq prettify-symbols-unprettify-at-point t)
	       ;; thematic configs
	       (set-frame-font "iosevka-14" nil t)
	       (tool-bar-mode -1)))

;; LLVM Style Guide from llvm/utils/emacs/emacs.el

;; ease of access
(defun linux-style ()
  (interactive)
  (c-set-style "linux"))

(defun llvm-style ()
  (interactive)
  (c-set-style "llvm.org"))

(defun llvm-lineup-statement (langelem)
  (let ((in-assign (c-lineup-assignments langelem)))
    (if (not in-assign)
	'++
      (aset in-assign 0
	    (+ (aref in-assign 0)
	       (* 2 c-basic-offset)))
      in-assign)))

;; Add a cc-mode style for editing LLVM C and C++ code
(c-add-style "llvm.org"
	     '("gnu"
	       (fill-column . 80)
	       (c++-indent-level . 2)
	       (c-basic-offset . 2)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((arglist-intro . ++)
				   (innamespace . 0)
				   (member-init-intro . ++)
				   (statement-cont . llvm-lineup-statement)))))

;; Files with "llvm" in their names will automatically be set to the
;; llvm.org coding style.
(add-hook 'c-mode-common-hook
	  (function
	   (lambda nil
	     (if (string-match "llvm" buffer-file-name)
		 (progn
		   (c-set-style "llvm.org"))))))

;; setup general
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(setq tab-width 2)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(scroll-bar-mode -1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("85e6bb2425cbfeed2f2b367246ad11a62fb0f6d525c157038a0d0eaaabc1bfee" "8543b328ed10bc7c16a8a35c523699befac0de00753824d7e90148bca583f986" "9ce93c9eb0f26697eadc71afb99214fa2c85396f6148ddabf5a8319e667513ae" "fec45178b55ad0258c5f68f61c9c8fd1a47d73b08fb7a51c15558d42c376083d" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "100eeb65d336e3d8f419c0f09170f9fd30f688849c5e60a801a1e6addd8216cb" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "fee4e306d9070a55dce4d8e9d92d28bd9efe92625d2ba9d4d654fc9cd8113b7f" "50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" "ad16a1bf1fd86bfbedae4b32c269b19f8d20d416bd52a87cd50e355bf13c2f23" "93268bf5365f22c685550a3cbb8c687a1211e827edc76ce7be3c4bd764054bad" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "840db7f67ce92c39deb38f38fbc5a990b8f89b0f47b77b96d98e4bf400ee590a" "a62f0662e6aa7b05d0b4493a8e245ab31492765561b08192df61c9d1c7e1ddee" "04790c9929eacf32d508b84d34e80ad2ee233f13f17767190531b8b350b9ef22" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "341b2570a9bbfc1817074e3fad96a7eff06a75d8e2362c76a2c348d0e0877f31" "819d24b9aba8fcb446aecfb59f87d1817a6d3eb07de7fdec67743ef32194438b" "f5f3a6fb685fe5e1587bafd07db3bf25a0655f3ddc579ed9d331b6b19827ea46" "542e6fee85eea8e47243a5647358c344111aa9c04510394720a3108803c8ddd1" "ffac21ab88a0f4603969a24b96993bd73a13fe0989db7ed76d94c305891fad64" "45a8b89e995faa5c69aa79920acff5d7cb14978fbf140cdd53621b09d782edcf" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "fc7fd2530b82a722ceb5b211f9e732d15ad41d5306c011253a0ba43aaf93dccc" "3e34e9bf818cf6301fcabae2005bba8e61b1caba97d95509c8da78cff5f2ec8e" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "27.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
	    (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Bootstrap config
(eval-when-compile
  (require 'use-package))

(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el

; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH


;; Allow users to provide an optional "init-preload-local.el"
(require 'init-preload-local nil t)
(require 'emacs-std)

(use-package session
:ensure t
:hook
(after-init-hook . session-initialize))

(require 'desktop)
(desktop-save-mode)

(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame nil)
(add-hook 'after-make-frame-functions (lambda (frame) (select-frame frame) (maximize-frame)) nil)

(require 'init-themes)

(require 'init-org-mode)

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
  ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-@" . 'counsel-mark-ring)
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(use-package amx
  :ensure t
  :init (amx-mode))

;; 切换窗口
(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))

(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

(use-package hydra
  :ensure t)

(use-package use-package-hydra
  :ensure t
  :after hydra)

;; (use-package undo-tree
;;   :ensure t
;;   :init
;;   (global-undo-tree-mode)
;;   :custom
;;   (undo-tree-auto-save-history nil))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  :after hydra
  :bind ("C-x C-h u" . hydra-undo-tree/body)
  :hydra (hydra-undo-tree (:hint nil)
  "
  _p_: undo  _n_: redo _s_: save _l_: load   "
  ("p"   undo-tree-undo)
  ("n"   undo-tree-redo)
  ("s"   undo-tree-save-history)
  ("l"   undo-tree-load-history)
  ("u"   undo-tree-visualize "visualize" :color blue)
  ("q"   nil "quit" :color blue)))

;; 平滑滚动
(use-package good-scroll
  :ensure t
  :if window-system
  :init (good-scroll-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode))

(global-set-key (kbd "C-j") nil)
(use-package avy
  :ensure t
  :bind
  (("C-j C-j" . avy-goto-char-timer)))

 (use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "LaoBu's Emacs")
  ;; (setq dashboard-projects-backend 'projectile)
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)
			  (projects . 10)))
  (dashboard-setup-startup-hook))

;; 高亮
(use-package highlight-symbol
  :ensure t
  :init
  (highlight-symbol-mode)
  :bind ("<f3>" . highlight-symbol))

;; 多重括号用不同颜色表示
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 自动补全
(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1) ; 只需敲 1 个字母就开始进行自动补全
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence))) ; 根据选择的频率进行排序，不喜欢可以去掉

(global-set-key (kbd "M-/") 'hippie-expand)


;; (use-package yasnippet
;;   :ensure t
;;   :hook
;;   (prog-mode . yas-minor-mode)
;;   :config
;;   (yas-reload-all)
;;   ;; add company-yasnippet to company-backends
;;   (defun company-mode/backend-with-yas (backend)
;;     (if (and (listp backend) (member 'company-yasnippet backend))
;; 	backend
;;       (append (if (consp backend) backend (list backend))
;; 	      '(:with company-yasnippet))))
;;   (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
;;   ;; unbind <TAB> completion
;;   (define-key yas-minor-mode-map [(tab)]        nil)
;;   (define-key yas-minor-mode-map (kbd "TAB")    nil)
;;   (define-key yas-minor-mode-map (kbd "<tab>")  nil)
;;   :bind
;;   (:map yas-minor-mode-map ("S-<tab>" . yas-expand)))

;; (use-package yasnippet-snippets
;;   :ensure t
;;   :after yasnippet)

;; (use-package company-c-headers
;;   :ensure t
;;   :config (add-to-list 'company-backends 'company-c-headers))

;; 代码分析
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
	lsp-file-watch-threshold 500
	lsp-auto-configure t
	lsp-auto-guess-root t
	lsp-idle-delay 0.500
	lsp-session-file "~/.emacs/.cache/lsp-sessions")
  :hook
  (lsp-mode . lsp-enable-which-key-integration) ; which-key integration
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-completion-provider :none) ;; 阻止lsp 重新设置 company-backend 而覆盖我们 yasnippet 的设置
  (setq lsp-headerline-breadcrumb-enable t)
  :bind
  ("C-c l s" . lsp-ivy-workspace-symbol)) ;; 可快速搜索工作区内的符号（类名、函数名、变量名等）


(use-package lsp-ui
  :after (lsp-mode)
  :commands (lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
	;; M-.
	([remap xref-find-references] . lsp-ui-peek-find-references)
	;; M-?
	([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	("C-c u" . lsp-ui-imenu))
  :hook (lsp-mode . lsp-ui-mode)
  :init
  ;; https://github.com/emacs-lsp/lsp-mode/blob/master/docs/tutorials/how-to-turn-off.md
  (setq lsp-enable-symbol-highlighting t
	lsp-ui-doc-enable t
	lsp-lens-enable t))

;; (use-package lsp-ui
;;   :ensure t
;;   :config
;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;;   (setq lsp-ui-doc-position 'top))

(use-package lsp-ivy
  :ensure t
  :after (lsp-mode))

;;
(use-package flycheck
  :ensure t
  :config
  (setq truncate-lines nil)
  :hook
  (c-mode . flycheck-mode)
  (c++-mode . flycheck-mode))

(use-package dap-mode
  :ensure t
  :after hydra lsp-mode
  :commands dap-debug
  :custom
  (dap-auto-configure-mode t)
  :config
  (dap-ui-mode 1)
  :hydra
  (hydra-dap-mode
   (:color pink :hint nil :foreign-keys run)
   "
^Stepping^          ^Switch^                 ^Breakpoints^         ^Debug^                     ^Eval
^^^^^^^^----------------------------------------------------------------------------------------------------------------
_n_: Next           _ss_: Session            _bb_: Toggle          _dd_: Debug                 _ee_: Eval
_i_: Step in        _st_: Thread             _bd_: Delete          _dr_: Debug recent          _er_: Eval region
_o_: Step out       _sf_: Stack frame        _ba_: Add             _dl_: Debug last            _es_: Eval thing at point
_c_: Continue       _su_: Up stack frame     _bc_: Set condition   _de_: Edit debug template   _ea_: Add expression.
_r_: Restart frame  _sd_: Down stack frame   _bh_: Set hit count   _ds_: Debug restart
_Q_: Disconnect     _sl_: List locals        _bl_: Set log message
		  _sb_: List breakpoints
		  _sS_: List sessions
"
   ("n" dap-next)
   ("i" dap-step-in)
   ("o" dap-step-out)
   ("c" dap-continue)
   ("r" dap-restart-frame)
   ("ss" dap-switch-session)
   ("st" dap-switch-thread)
   ("sf" dap-switch-stack-frame)
   ("su" dap-up-stack-frame)
   ("sd" dap-down-stack-frame)
   ("sl" dap-ui-locals)
   ("sb" dap-ui-breakpoints)
   ("sS" dap-ui-sessions)
   ("bb" dap-breakpoint-toggle)
   ("ba" dap-breakpoint-add)
   ("bd" dap-breakpoint-delete)
   ("bc" dap-breakpoint-condition)
   ("bh" dap-breakpoint-hit-condition)
   ("bl" dap-breakpoint-log-message)
   ("dd" dap-debug)
   ("dr" dap-debug-recent)
   ("ds" dap-debug-restart)
   ("dl" dap-debug-last)
   ("de" dap-debug-edit-template)
   ("ee" dap-eval)
   ("ea" dap-ui-expressions-add)
   ("er" dap-eval-region)
   ("es" dap-eval-thing-at-point)
   ("q" nil "quit" :color blue)
   ("Q" dap-disconnect :color red)))

(use-package helm
  :ensure t
  :config (helm-mode 1)
  :bind
  (
   ("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-mini)
   ("C-x r b" . helm-filtered-bookmarks)
   ) ;; end of bind
  :config (helm-autoresize-mode t)
  :config (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
  :config (use-package helm-ag
	    :ensure t
	    ;;:config (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
	    :config (setq helm-ag-base-command "rg --vimgrep --no-heading")
	    :config (setq helm-ag-insert-at-point 'symbol)
	    :config (setq helm-ag-success-exit-status '(0 2))
	    ;;:config (setq helm-ag-command-option "--all-text")
	    ) ;; end of config
  :config (use-package helm-ls-git
	    :ensure t
	    :bind
	    (
	     ("C-x C-d" . helm-browser-project)
	     ("C-x r p" . helm-projects-history)
	     )
	    )
  :config (use-package helm-xref
	    :ensure t
	    )
)

(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))

;; (use-package helm-projectile
;;   :if (functionp 'helm)
;;   :config
;;   (helm-projectile-on))


(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :init (counsel-projectile-mode))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-tag-follow-mode)
  :bind
  (:map global-map
	("M-0"       . treemacs-select-window)
	("C-x t 1"   . treemacs-delete-other-windows)
	("C-x t t"   . treemacs)
	("C-x t B"   . treemacs-bookmark)
	;; ("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
	("/" . treemacs-advanced-helpful-hydra)))


(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp))


(use-package org-download
  :ensure t
  :config
  (require 'org-download)
  ;; add support to dired
  (add-hook 'dired-mode-hook 'org-download-enable))


;; 主题
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-monokai-octagon t)
  (doom-themes-treemacs-config))

(use-package smart-mode-line
  :ensure t
  :init
  ; (setq sml/no-confirm-load-theme t)  ; avoid asking when startup
  (sml/setup)
  :config
  (setq rm-blacklist
    (format "^ \\(%s\\)$"
      (mapconcat #'identity
	'("Projectile.*" "company.*" "Google"
	  "Undo-Tree" "counsel" "ivy" "yas" "WK")
	"\\|"))))

;; language
(use-package erlang
  :ensure t)

(use-package ivy-erlang-complete
  :ensure t)

(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :config
  ;; for debug
  (require 'dap-python))

(use-package lsp-pyright
  :ensure t
  :config
  :hook
  (python-mode . (lambda ()
		  (require 'lsp-pyright)
		  (lsp-deferred))))

(use-package pyvenv
  :ensure t
  :config
  ;; (setenv "WORKON_HOME" (expand-file-name "D:\\miniconda3"))
  (pyvenv-mode t))

;; c&c++
(use-package c++-mode
  :functions			; suppress warnings
  c-toggle-hungry-state
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (c++-mode . c-toggle-hungry-state))

;; (use-package eglot
;;   :ensure t
;;   :config
;;   (add-to-list 'eglot-server-programs '((c-mode c++-mode) "clangd"))
;;   (add-hook 'c-mode-hook 'eglot-ensure)
;;   (add-hook 'c++-mode-hook 'eglot-ensure))

(use-package clang-format+
  :ensure t
  :init
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  :config
  (add-hook 'c-mode-common-hook #'clang-format+-mode)
  ;; (add-hook 'cpp-mode-common-hook
  ;;         (function (lambda ()
  ;;                     (add-hook 'write-contents-functions
  ;;                               (lambda() (progn (clang-format-buffer) nil))))))
  :bind(("C-M-\\" . clang-format)
  ))

;; font
(set-frame-font "Fira Code Retina-14")
(global-set-key (kbd "C-j h") 'ff-find-other-file)

;; (when (fboundp 'set-charset-priority)
;;   (set-charset-priority 'unicode))
;; (prefer-coding-system 'utf-8-unix)
;; (setq locale-coding-system 'utf-8-unix)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here

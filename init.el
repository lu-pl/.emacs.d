;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;;;; use package installation
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "./init.el"))
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; general settings

;; output generated code to separate file
(setq custom-file (expand-file-name "./.generated.el"))

;; load theme
(load-theme 'wombat t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; global keybindings
(define-key global-map (kbd "M-c") 'completion-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; use-package 
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  ;; behold, the chad setting!
  (defalias 'evil-insert-state 'evil-emacs-state)
  (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
  (setq evil-emacs-state-cursor '(bar . 1)))

(use-package linum-relative
  :ensure t
  :init
  (display-line-numbers-mode)
  (linum-relative-global-mode)
  (setq linum-relative-current-symbol ""))

(use-package helm
  :ensure t
  :config
  (setf helm-move-to-line-cycle-in-source nil)
  :bind
  (:map global-map
	(("M-x" . 'helm-M-x)
	 ("C-x C-f" . 'helm-find-files)
	 ("C-x b" . 'helm-mini)
	 ("M-s M-f" . 'helm-find)
	 ("M-j" . 'helm-next-line)
	 ("M-k" . 'helm-previous-line)
	 ("M-d" . 'helm-next-page)
	 ("M-u" . 'helm-previous-page)
	 ;; visible mark bindings (helm selection)
	 ("M-C-j" . 'helm-toggle-visible-mark-forward)
	 ("M-C-k" . 'helm-toggle-visible-mark-backward)
	 ("M-C-q" . 'helm-toggle-all-marks)
	 ;; mark ring
	 ("M-m" . 'helm-mark-ring)
	 ;; kill ring)
	 ("M-p" . 'helm-show-kill-ring))))

(use-package flycheck
  :ensure t
  :hook
  (prog-mode-hook . flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; completion
(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-dabbrev-downcase 0
	company-idle-delay 0
	company-minimum-prefix-length 3
	company-selection-wrap-around t
	company-dabbrev-downcase nil))

(use-package company-quickhelp
  :ensure t
  :hook
  (python-mode . company-quickhelp-mode)
  (nxml-mode . company-quickhelp-mode)
  (lisp-mode . company-quickhelp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; parenthesis <3

(use-package smartparens
  :ensure t
  :hook
  (prog-mode . smartparens-global-mode)
  ((lisp-mode
    lisp-interaction-mode
    emacs-lisp-mode) . (lambda ()
    (progn (sp-pair "'" nil :actions :rem)
	   (sp-pair "`" nil :actions :rem)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; python

(use-package pyvenv
  :ensure t
  :config
  (setq pyvenv-default-virtual-env-name (expand-file-name "~/environments/")))

(use-package poetry
  :ensure t)

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq elpy-shell-echo-input nil)
  ;; start shell in current working directory
  (setq elpy-shell-starting-directory 'current-directory)
  ;; switch to *Python Doc* window on elpy-doc -> q to jump back to previous window
  (advice-add 'elpy-doc :after #'(lambda () (select-window (get-buffer-window "*Python Doc*"))))
  
  :hook
  (elpy-mode . (lambda () (highlight-indentation-mode -1)))
  (elpy-mode . (lambda () (flymake-mode -1)))
  ;; https://github.com/jorgenschaefer/elpy/issues/349#issuecomment-54599147
  (elpy-mode . (lambda ()
		 (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))
  :bind
  ("C-c e" . elpy-execute)
  ("C-M-l" . elpy-nav-indent-shift-right)
  ("C-M-h" . elpy-nav-indent-shift-left)
  ("C-M-k" . elpy-nav-move-line-or-region-up)
  ("C-M-j" . elpy-nav-move-line-or-region-down)
  ("C-k" . elpy-nav-backward-block)
  ("C-j" . elpy-nav-forward-block))


(use-package py-isort
  :ensure t
  :hook
  (before-save . py-isort-before-save))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; lsp

;; requires: pip install 'python-lsp-server[all]'
(use-package lsp-mode
  :ensure t
  :hook
  (python-mode . lsp-deferred)
  :config
  (setq lsp-pylsp-configuration-sources ["pylsp-mypy"])
  (lsp-register-custom-settings
   '(("pylsp.plugins.flake8.enabled" t t)
     ("pylsp.plugins.autopep8.enabled" t t)
     ("pylsp.plugins.black.enabled" t t)
     ("pylsp-plugins.isort.enabled" t t)
     ("pylsp.plugins.rope-autoimport.enabled" t t))))

(use-package lsp-ui
  :ensure t
  :config (setq lsp-ui-sideline-enable t
		lsp-ui-sideline-show-hover t
		lsp-ui-sideline-delay 0.3
		lsp-ui-sideline-ignore-duplicate t
		;; lsp-ui-sideline-diagnostics-max-lines 79
		lsp-ui-sideline-diagnostic-max-lines 5
		lsp-ui-sideline-show-code-actions t
		
		lsp-ui-doc-enable t
		lsp-ui-doc-delay 0.3
		lsp-ui-doc-position 'right
		lsp-ui-doc-alignment 'frame
		lsp-ui-doc-header nil
		lsp-ui-doc-include-signature t
		lsp-ui-doc-use-childframe t))

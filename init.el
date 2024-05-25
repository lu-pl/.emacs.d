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

;;;; general settings
;; output generated code to separate file
(setq custom-file (expand-file-name "./.generated.el"))

;; global keybindings
(define-key global-map (kbd "M-c") 'completion-at-point)


;;;; use-package 
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  ;; behold, the chad setting!
  (defalias 'evil-insert-state 'evil-emacs-state)
  (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
  (setq evil-emacs-state-cursor '(bar . 1)))

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
  
  ("M-k" . elpy-nav-backward-block)
  ("M-j" . elpy-nav-forward-block))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

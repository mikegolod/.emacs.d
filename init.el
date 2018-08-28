;; Init package system
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(push "~/.emacs.d/vendor" load-path)

;; Tell custom where to store it's stuff
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))
(eval-when-compile (require 'use-package))
(require 'bind-key)

;;;;;;;;;;;;;;;;;
;; Core editor ;;
;;;;;;;;;;;;;;;;;

;; Time locale
(setq-default system-time-locale "C")

;; Tabs
(setq-default indent-tab-mode nil)
(setq-default tab-width 8)
(setq tab-always-indent 'complete)

;; Swap meta & super for darwin
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

;;;;;;;;;;;;;;;;;
;; Programming ;;
;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

(use-package git-gutter :ensure t
  :config
  (add-hook 'prog-mode-hook 'git-gutter-mode)
  (setq git-gutter:added-sign "++")
  (setq git-gutter:modified-sign "==")
  (setq git-gutter:deleted-sign "--")
  (setq git-gutter:visual-line t))

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(use-package powerline :ensure t :config (powerline-default-theme))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (add-to-list 'magit-repository-directories '("~" . 3)))

(use-package editorconfig
  :ensure t
  :config (editorconfig-mode t))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode t))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
  	 ("C-x C-f" . helm-find-files)
  	 ("C-x f" . helm-recentf)
  	 ("C-x b" . helm-buffers-list)
	 ("C-c i" . helm-imenu))
  :config
  (require 'helm-config)
  (setq helm-recentf-fuzzy-match t))

(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package flycheck :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-flow :ensure t
  :config
  (flycheck-add-mode 'javascript-flow 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

(use-package flycheck-color-mode-line :ensure t
  :config
    (setq flycheck-highlighting-mode 'symbols)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package prettier-js
  :config
  (setq prettier-js-args '("--trailing-comma" "es5"
			   "--single-quote" "true"
			   "--print-width" "120"
			   )))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode)
  :init
  ;;(add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook 'flycheck-mode)
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(use-package less-css-mode :ensure t)

(use-package npm-mode :ensure t
  :mode ("package\\.json"))

(use-package centered-window :ensure t
  :bind ("C-c c" . centered-window-mode))

(if (display-graphic-p)
    (use-package monokai-theme :ensure t :config (load-theme 'monokai t)))

(use-package php-mode :ensure t)

(use-package phpcbf :ensure t
  :init
  (add-hook 'php-mode-hook 'phpcbf-enable-on-save)
  :config
  (setq phpcbf-standard "PSR2")
)

(use-package go-mode :ensure t
  :config
  (add-hook 'go-mode-hook (lambda () (add-hook (make-local-variable 'before-save-hook) 'gofmt-before-save)))
)

(use-package company-go :ensure t
  :config
  (add-hook 'go-mode-hook (lambda () (add-to-list 'company-backends 'company-go)))
  )

(use-package go-eldoc :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package restclient :ensure t)

(use-package yaml-mode :ensure t)

(use-package markdown-mode :ensure t)

(use-package markdown-mode+ :ensure t)

(use-package cider :ensure t)

(use-package typescript-mode :ensure t)

;;;;;;;;;;;;
;; Docker ;;
;;;;;;;;;;;;

(use-package docker :ensure t)

(use-package dockerfile-mode :ensure t)

(use-package docker-compose-mode :ensure t)

(use-package apache-mode :ensure t)

;;;;;;;;;;;;;;;;;;
;; Company mode ;;
;;;;;;;;;;;;;;;;;;

(use-package company :ensure t
  :bind ("M-/" . 'company-complete)
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;;;;;;;;;;;;;;;;
;; Dired mode ;;
;;;;;;;;;;;;;;;;

(setq dired-listing-switches "-alh")

(use-package dired+)

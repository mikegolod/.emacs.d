;; Init package system
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(push "~/.emacs.d/vendor" load-path)

;; Install use-package
(package-install 'use-package t)

;; Core editor
;; Tabs
(setq-default indent-tab-mode nil)
(setq-default tab-width 8)
(setq tab-always-indent 'complete)

;; Packages 
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
  (projectile-global-mode t))

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

(use-package prettier-js
  :config
  (setq prettier-js-args '("--trailing-comma" "es5"
			   "--single-quote" "true"
			   "--max-width" "120"
			   )))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode)
  :config (add-hook 'js2-mode-hook 'prettier-js-mode))

(use-package less-css-mode :ensure t)

;; Tell custom where to store it's stuff
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

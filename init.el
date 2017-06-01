;; Init package system
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(push "~/.emacs.d/vendor" load-path)

;; Install use-package
(package-install 'use-package t)

;;;;;;;;;;;;;;;;;
;; Core editor ;;
;;;;;;;;;;;;;;;;;

;; Tabs
(setq-default indent-tab-mode nil)
(setq-default tab-width 8)
(setq tab-always-indent 'complete)

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (add-to-list 'magit-repository-directories '("~" . 3)))

(use-package git-gutter :ensure t
  :config
  (git-gutter:linum-setup))

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
  :init (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

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
			   "--max-width" "120"
			   )))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode)
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook 'flycheck-mode)
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(use-package less-css-mode :ensure t)

(use-package npm-mode :ensure t
  :mode ("package\\.json"))

(use-package centered-window-mode :ensure t
  :bind ("C-c c" . centered-window-mode))

(use-package monokai-theme :ensure t :config (load-theme 'monokai t))

;;;;;;;;;;;;;;;;;
;; Programming ;;
;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'git-gutter-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

;; Tell custom where to store it's stuff
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

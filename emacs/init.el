;;Disable Start Menu
(setq inhibit-startup-screen t)

;;Menu Bar
(menu-bar-mode -1)

;;Scroll Bar
(scroll-bar-mode -1)

;;Toolbar
(tool-bar-mode -1)

;;Fringe Mode
(set-fringe-mode 10)

;;Disable Bell
(setq ring-bell-function 'ignore)

;;Backup Files
(setq
    backup-by-copying t
    backup-directory-alist
    '(("." . "~/.config/emacs/Garbage/"))
    delete-old-versions t
    kept-new-versions 6
    kept-old-versions 2
    version-control t)

;;Auto Save Files
(setq kill-buffer-delete-auto-save-files t)
;;(setq auto-save-file-name-transforms
;;     '((".*" . "~/.config/emacs/Garbage/")))
;;Lock Files
(setq create-lockfiles nil)
;;(setq lock-file-name-transforms
;;  '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))

;;Font
(set-face-attribute 'default nil :font "Iosevka NF" :height 160)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(column-number-mode)
(global-display-line-numbers-mode t)
;;Disable Line Numbers For Some Modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;Package Initialization
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package diminish)

;;Packages
(use-package gruvbox-theme
  :init
  (load-theme 'gruvbox-dark-medium t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.2))

(use-package all-the-icons)

(use-package evil
  :diminish evil-mode
  :init
  (setq evil-want-keybinding nil)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-want-C-u-delete t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-w-in-emacs-state t)
  (setq evil-insert-state-cursor 'evil-normal-state-cursor)
  (setq evil-visual-state-cursor 'evil-normal-state-cursor)
  :config
  (evil-mode 1)
  (evil-set-leader nil (kbd "C-SPC"))
  (evil-set-leader nil (kbd "SPC") t))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package mood-line
  :config
  (mood-line-mode)
  :custom
  (setq mood-line-glyph-alist mood-line-glyphs-unicode)
  (setq mood-line-format mood-line-format-default-extended))

(use-package helm
  :diminish helm-mode
  :init
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  :config
  (require 'helm-autoloads)
  (helm-mode 1))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.2)
  (which-key-mode))

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)
(use-package company
  :bind
      (:map company-active-map
            ("<tab>" . company-complete-selection))
  :hook ('after-init-hook . company-mode)
  :custom
  (setq company-minimum-prefix-length 1))

(use-package company-box
  :hook (company-mode . company-box-mode))


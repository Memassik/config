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

;; (column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
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
  (load-theme 'gruvbox-dark-hard t))

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
  (evil-set-leader nil (kbd "C-SPC"))
  (evil-set-leader nil (kbd "SPC") t)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package mood-line
  :config
  (mood-line-mode)
  :custom
  (setq mood-line-glyph-alist mood-line-glyphs-unicode)
  (setq mood-line-format mood-line-format-default-extended))

;; Enable vertico
(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 10) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(substring orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package eglot
  :config
  (progn
    (customize-set-variable 'eglot-autoshutdown t)
    (customize-set-variable 'eglot-extend-to-xref t)
    (with-eval-after-load 'eglot
        (setq completion-category-defaults nil)
        (add-to-list 'eglot-server-programs
            '((c-mode c++-mode)
                 . ("clangd"
                       "-j=8"
                       "--malloc-trim"
                       "--log=error"
                       "--background-index"
                       "--clang-tidy"
                       "--cross-file-rename"
                       "--completion-style=detailed"
                       "--pch-storage=memory"
                       "--header-insertion=never"
                       "--header-insertion-decorators=0"
		       "--function-arg-placeholders"))))

    (add-hook 'c-mode-hook #'eglot-ensure)
    (add-hook 'c++-mode-hook #'eglot-ensure)
    (add-hook 'rustic-mode-hook #'eglot-ensure)))

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package company
  :config
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(setq-default c-basic-offset 4)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(orderless vertico mood-line flycheck evil-commentary evil-surround evil-collection evil all-the-icons which-key rainbow-delimiters gruvbox-theme diminish)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

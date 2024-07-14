(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))
(setq use-package-always-ensure t)

;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:
;;(use-package general :ensure (:wait t) :demand t)

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
  :demand t
  :init
  (setq evil-respect-visual-line-mode t) ;; respect visual lines

  (setq evil-search-module 'isearch) ;; use emacs' built-in search functionality.

  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (setq evil-want-C-d-scroll t) ;; allow scroll down with 'C-d'

  (setq evil-want-integration t) ;; necessary for evil collection
  (setq evil-want-keybinding nil)

  (setq evil-split-window-below t) ;; split windows created below
  (setq evil-vsplit-window-right t) ;; vertically split windows created to the right

  (setq evil-want-C-i-jump nil) ;; hopefully this will fix weird tab behaviour

  (setq evil-undo-system 'undo-redo) ;; undo via 'u', and redo the undone change via 'C-r'; only available in emacs 28+.
  :config
  (evil-mode t) ;; globally enable evil mode
  ;; set the initial state for some kinds of buffers.
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; buffers in which I want to immediately start typing should be in 'insert' state by default.
  (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'magit-diff-mode 'insert))

(use-package evil-collection ;; evilifies a bunch of things
  :after evil
  :init
  (setq evil-collection-outline-bind-tab-p t) ;; '<TAB>' cycles visibility in 'outline-minor-mode'
  ;; If I want to incrementally enable evil-collection mode-by-mode, I can do something like the following:
  ;; (setq evil-collection-mode-list nil) ;; I don't like surprises
  ;; (add-to-list 'evil-collection-mode-list 'magit) ;; evilify magit
  ;; (add-to-list 'evil-collection-mode-list '(pdf pdf-view)) ;; evilify pdf-view
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode)) ;; globally enable evil-commentary

(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-evil-setup)
  ;; integrate general with evil

  ;; set up 'SPC' as the global leader key
  (general-create-definer patrl/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  ;; set up ',' as the local leader key
  (general-create-definer patrl/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "," ;; set local leader
    :global-prefix "M-,") ;; access local leader in insert mode

  ;;(general-define-key
  ;; :states 'insert
  ;; "C-g" 'evil-normal-state) ;; don't stretch for ESC

  ;; unbind some annoying default bindings
  ;;(general-unbind
  ;;  "C-x C-r"   ;; unbind find file read only
  ;;  "C-x C-z"   ;; unbind suspend frame
  ;;  "C-x C-d"   ;; unbind list directory
  ;;  "<mouse-2>") ;; pasting with mouse wheel click


  ;;(patrl/leader-keys
  ;;  "SPC" '(execute-extended-command :wk "execute command") ;; an alternative to 'M-x'
  ;;  "TAB" '(:keymap tab-prefix-map :wk "tab")) ;; remap tab bindings

  ;;(patrl/leader-keys
  ;;"w" '(:keymap evil-window-map :wk "window")) ;; window bindings

  ;;(patrl/leader-keys
  ;;  "c" '(:ignore t :wk "code"))

  ;;;; help
  ;;;; namespace mostly used by 'helpful'
  ;;(patrl/leader-keys
  ;;  "h" '(:ignore t :wk "help"))

  ;;;; file
  ;;(patrl/leader-keys
  ;;  "f" '(:ignore t :wk "file")
  ;;  "ff" '(find-file :wk "find file") ;; gets overridden by consult
  ;;  "fs" '(save-buffer :wk "save file"))

  ;;;; buffer
  ;;;; see 'bufler' and 'popper'
  ;;(patrl/leader-keys
  ;;  "b" '(:ignore t :wk "buffer")
  ;;  "bb" '(switch-to-buffer :wk "switch buffer") ;; gets overridden by consult
  ;;  "bk" '(kill-this-buffer :wk "kill this buffer")
  ;;  "br" '(revert-buffer :wk "reload buffer"))

  ;;;; bookmark
  ;;(patrl/leader-keys
  ;;  "B" '(:ignore t :wk "bookmark")
  ;;  "Bs" '(bookmark-set :wk "set bookmark")
  ;;  "Bj" '(bookmark-jump :wk "jump to bookmark"))

  ;;;; universal argument
  ;;(patrl/leader-keys
  ;;  "u" '(universal-argument :wk "universal prefix"))

  ;;;; notes
  ;;;; see 'citar' and 'org-roam'
  ;;(patrl/leader-keys
  ;;  "n" '(:ignore t :wk "notes")
  ;;  ;; see org-roam and citar sections
  ;;  "na" '(org-todo-list :wk "agenda todos")) ;; agenda

  ;;;; code
  ;;;; see 'flymake'
  ;;(patrl/leader-keys
  ;;  "c" '(:ignore t :wk "code"))

  ;;;; open
  ;;(patrl/leader-keys
  ;;  "o" '(:ignore t :wk "open")
  ;;  "os" '(speedbar t :wk "speedbar")
  ;;  "op" '(elpaca-log t :wk "elpaca"))


  ;;;; search
  ;;;; see 'consult'
  ;;(patrl/leader-keys
  ;;  "s" '(:ignore t :wk "search"))

  ;;;; templating
  ;;;; see 'tempel'
  ;;(patrl/leader-keys
  ;;  "t" '(:ignore t :wk "template"))
  )

(use-package mood-line
  :demand t
  :config
  (mood-line-mode)
  (setq mood-line-format mood-line-format-default-extended))

(use-package all-the-icons
  :demand t)

;; prettify dired with icons
(use-package all-the-icons-dired
  :demand t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :demand t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

(use-package hl-todo
  :demand t
  :init
  (global-hl-todo-mode))

(use-package gruvbox-theme
  :demand t
  :init
  (load-theme 'gruvbox-dark-soft t))

(use-package magit 
  :demand t
  )

(use-package tree-sitter-langs
  :demand t
  :init
  (require 'tree-sitter-langs))

(use-package tree-sitter
  :demand t
  :init
  (require 'tree-sitter)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.
(use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist '(("." . "~/.config/emacs/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

(setq package-enable-at-startup nil)
;; minimal UI
(menu-bar-mode -1) ;; disables menubar
(tool-bar-mode -1) ;; disables toolbar
(scroll-bar-mode -1) ;; disables scrollbar
(pixel-scroll-precision-mode 1) ;; enable smooth scrolling

(setq inhibit-splash-screen t ;; no thanks
        use-file-dialog nil ;; don't use system file dialog
        tab-bar-new-button-show nil ;; don't show new tab button
        tab-bar-close-button-show nil ;; don't show tab close button
        tab-line-close-button-show nil) ;; don't show tab close buttons

(set-face-attribute 'default nil :font "JetBrainsMono NF" :height 240)
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)
(defun post-text-scale-callback ()
  ;; fix line number text size
  (let ((new-size (floor (* (face-attribute 'default :height)
                            (expt text-scale-mode-step text-scale-mode-amount)))))
    (set-face-attribute 'line-number nil :height new-size)
    (setq nlinum-format "%d")
    (set-face-attribute 'line-number-current-line nil :height new-size)))
(add-hook 'text-scale-mode-hook 'post-text-scale-callback)

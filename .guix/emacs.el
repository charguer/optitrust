(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(setq auto-mode-alist 
      (append '(("\\.ml[ily]?$" . tuareg-mode)
	        ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

(require 'merlin)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(setq merlin-use-auto-complete-mode 'easy)

(defun set-indent (n)
  (setq indent-tabs-mode nil))
(add-hook 'prog-mode-hook (lambda () (set-indent 2)))
(add-hook 'text-mode-hook (lambda () (set-indent 2)))

(setq-default fill-column 80)
(setq display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
(setq-default display-fill-column-indicator-column 80)

;; Do not require a second space to mark the end of a sentence.
(setq sentence-end-double-space nil)

(require 'org-tempo) ;; easy templates
(require 'org-ref) ;; bibliography support

;; Do not confirm code block evaluation.
(setq org-confirm-babel-evaluate nil)

;; Preserve indentation on export and tangle.
(setq org-src-preserve-indentation t)
(setq org-src-tab-acts-natively t)

;; Allow syntax highlighting.
(setq org-src-fontify-natively t)

;; Disable auto-scroll.
(setq auto-window-vscroll nil)

;; Load loanguages for code block evaluation.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (shell . t)
   (dot . t)))

;; Disable inline image display.
(setq org-startup-with-inline-images nil)

;; Do not hide brackets around links in Org.
(setq org-descriptive-links nil)

(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq inhibit-startup-screen t)

(setq column-number-mode t)

(load-theme 'spacemacs-light t nil)

(set-face-attribute 'default nil :height 98)

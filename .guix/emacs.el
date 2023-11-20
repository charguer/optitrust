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

(setq display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
(setq-default display-fill-column-indicator-column 80)

(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq inhibit-startup-screen t)

(load-theme 'spacemacs-light t nil)

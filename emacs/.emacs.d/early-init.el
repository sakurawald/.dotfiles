
;; Optimize gc on startup and later.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 8 1000 1000)
                  gc-cons-percentage 0.1)))

;; Increase the external IO chunk size.
(setq read-process-output-max (* 8 1024 1024))

;; Hide un-used views.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tab-bar-mode)

;; Maximize the initial frame on startup: https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Set the coding-system.
(set-language-environment "UTF-8")
(setq default-input-method nil)

;; Ignore .Xdefaults and .Xresources files.
(setq inhibit-x-resources t)

;; Don't try to resize the frame to contains enough characters each line.
(setq frame-inhibit-implied-resize t)

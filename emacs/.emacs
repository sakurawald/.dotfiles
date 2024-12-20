
;;;; -- links --
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/index.html
;; https://neovim.io/
;; https://www.gnu.org/software/emacs/

;;;; -- humor --
;; See more in: https://www.gnu.org/fun/
;; While any text editor can save your files, only Emacs can save your soul.
;; While Vim is an extensible editor, the Emacs is an extended editor.
;; While Vim is a text editor, the Emacs has a text editor.
;; A nice Vim macro a day, keeps the VS Code away.


;; NOTE The name conversion use CRUD: create, read, update, delete.
;; NOTE reference https://github.com/rexim/dotfiles
;; TIP Basically, you need a good text-editor and a good compiler to work on a project. And a keymap works like macros to run scripts.

;; TIP The default 'prefix-keymap': https://www.gnu.org/software/emacs/manual/html_node/emacs/Prefix-Keymaps.html

;; TIP For translation service, use 'en' and 'dict' to trigger 'search engine' via 'vimium' in in browser.

;; NOTE It's also okay to steal some ideas from others' dotfiles.
;; TIP Use 'Super+{alpha}' to switch to useful programs (wmctrl -a "gnu emacs" || emacs): terminal, emacs, browser.
;; TIP Use 'Super+{num}' to switch to a window in KDE.
;; TIP Use 'Ctrl+{num}' to switch to a tab in web browser.
;; TIP Use 'customize' command to list the options provided by a package, and export them into '.emacs' later.
;; TIP To browse the firefox, use 'vimium' extension.

;; TODO customize the mode line

;;;; -- package manager --
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; NOTE To use the `stable repo', un-comment next line.
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents t)

;;;; -- vim emulator --
;; NOTE The vim golf makes the text-editing interesting. https://www.vimgolf.com/
;; TIP Use `C-z` to toggle between `vi-mode` and `emacs-mode`.
;; TIP Use 'vim macro' and 'vim repeat command' to do batch structure editing.
;; TIP Use `vi-visual-block-mode' for `multiple-cursors'.
(use-package evil
  :ensure t
  :init
  ;; integrate with: evil-collection
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)

  ;; option
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-u-delete t)
  (setq evil-undo-system 'undo-redo)

  :config
  (evil-mode 1))


(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-want-unimpaired-p nil)
  
  ;; NOTE evil-collection will setup a mode naemd `t` for `sldb` and `slime-inspector`.
  (evil-collection-setup-debugger-keys t)
  (evil-collection-want-find-usages-bindings t)
  (evil-collection-setup-minibuffer t)

  (evil-collection-term-sync-state-and-mode-p t)

  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-outline-bind-tab-p t)

  :config
  (evil-collection-init))

(use-package evil-escape
  :ensure t
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.1))

(use-package evil-surround
  :ensure t
  :config
  ;; TIP To 'add' a 'surrounding', use 'S' in 'vi-visual-state' or use 'ys<textobject><surrounding>' in 'vi-normal-state' (The 'ys' is a new vi-operator.).
  ;; TIP to 'change' a 'surrounding', use 'cs<old-textobject><new-textobject>'.
  ;; TIP to 'delete' a 'surrounding', use 'ds<text-object>'.
  (global-evil-surround-mode 1))

;;;; -- version control --
(use-package magit
  :ensure t)

;;;; -- social --
(use-package elcord
  :ensure t)
(require 'elcord)
(elcord-mode)

;;;; -- pair edit --
(use-package smartparens
  :ensure t
  ;; NOTE Only enable 'smartparens-mode' in these mode.
  :hook (prog-mode text-mode markdown-mode)
  :config
  ;; load default config
  (require 'smartparens-config))

;;;; -- language: lisp --
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "ros dynamic-space-size=4GiB run")
  ;; (setq slime-contribs '(slime-asdf
  ;; 			 slime-fancy
  ;; 			 slime-banner
  ;; 			 slime-xref-browser))
  ;; NOTE Use 'slime-setup' instead of `(setq slime-contribs '(slime-fancy))`.
  (slime-setup '(
		 slime-asdf
		 slime-quicklisp
		 slime-fancy
		 slime-banner
		 slime-xref-browser
		 ;; slime-highlight-edits (this only works for compile)
		 slime-company)))

;; Setup load-path and autoloads
;;(add-to-list 'load-path "~/dir/to/cloned/slime")
;;(require 'slime-autoloads)

(setq slime-startup-animation nil)


;;;; -- complete --
;; NOTE The 'company' extension has better integration than 'auto-complete'.
;; TIP Use 'Tab' or 'C-p' in insert-mode to trigger completion window
;; TIP Use 'C-n' and 'C-p' to select 'complete entry' in 'vi-insert-mode'.
;; TIP If you need the same number of key-stroke, why use fuzzy?
;; TIP Use 'key-conversion' to translate 'C-m' to 'RET'.

;; TODO polish the config of company
(use-package company
  :ensure t
  :config
  ;; FIXME not work
  (setq company-dabbrev-minimum-length 2)
  )

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode))

(use-package slime-company
  :ensure t
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))


;;;; -- auto read-only --
(use-package hardhat
  :ensure t
  :config
  (global-hardhat-mode 1)
  (push ".*/.roswell/src/.*" hardhat-fullpath-protected-regexps))

;;;; -- language: markdown --
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;;;; -- org --
;; TIP Use 'S-{arrow}' to control the 'priority' and 'status'.
;; TIP Use 'M-{arrow}' to control 'order' and 'level'.
;; TIP Use 'C-Ret' to insert a 'contextual-heading'.

;; TODO Use org agenda

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-agenda-files '("~/Workspace/github/note/TODO.org"))

(evil-define-key '(normal) 'global (kbd "SPC o t") 'org-show-todo-tree)
(evil-define-key '(normal) 'global (kbd "SPC u a") 'org-agenda)

;;;; -- chat --
;; NOTE It's recommemded to host an open-source chat-model locally.
;; TIP The possibility of chat includes: text generate, text complete, text improve, text expand, text shorten, text translate.
(use-package ellama
  :ensure t

  :config 
  (evil-define-key '(normal visual) 'global (kbd "SPC c") 'ellama-transient-main-menu)

  (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message)
  )


;; (use-package org-modern
;;   :ensure t
;;   :config
;;   (with-eval-after-load 'org (global-org-modern-mode))
;;   )

;;;; -- base16 theme -- 
;; TIP Use 'list-colors-display' to see all known-colors.
(use-package base16-theme
  :ensure t
  :config
  ;; NOTE See spec in https://github.com/chriskempson/base16/blob/main/styling.md
  ;; NOTE Instead of defining a new theme, we modify the existing one for convinence.
  (setq base16-sakura-theme-colors
	'(
	  :base00 "#000000" ;; default background: window
	  :base01 "#1C1C1C" ;; status bar, line numbers and folding marks. 
	  :base02 "#383838" ;; selection
	  :base03 "#545454" ;; comment
	  :base04 "#A2A2A2" ;; dark-theme foreground: doc-string
	  :base05 "#FFFFFF" ;; default foreground: text
	  :base06 "#DEDEDE" ;; light-theme foreground (not often used)
	  :base07 "#FCFCFC" ;; light-theme background (not often used)
	  :base08 "#FC5454" ;; cursor, symbol flags, sldb-condition
	  :base09 "#FFA500" ;; self-evaluating object, tab name.
	  :base0A "#FFFF00" ;; type, class
	  :base0B "#00FF00" ;; string
	  :base0C "#00FFFF" ;; keyword symbol
	  :base0D "#5454fc" ;; function name
	  :base0E "#FF00FF" ;; operator name
	  :base0F "#008000" ;; deprecated (opening/closing embedded language tags, e.g. '<?php ?>')
	  ))

  (load-theme 'base16-sakura t))



;;;; -- mode line --
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))


;;;; -- jump anywhere --
;; TIP You don't need to use `smooth scroll', just use `avy' or `grep'.
(use-package avy
  :ensure t
  :config 
  ;; TIP Use "M-j" to jump to a word, even in 'vi-visual-state' and 'vi-insert-state'.
  (global-set-key (kbd "M-z") 'avy-goto-word-0)
  (set-face-attribute 'avy-lead-face nil
		      :foreground "white"
		      :background "#e52b50")
  (set-face-attribute 'avy-lead-face-0 nil
		      :foreground "white"
		      :background "#4f5769")
  (set-face-attribute 'avy-lead-face-1 nil
		      :foreground "white"
		      :background "#4f5769")
  (set-face-attribute 'avy-lead-face-2 nil
		      :foreground "white"
		      :background "#4f5769"))

;;;; -- dimmer --
(use-package dimmer
  :ensure t
  :config 
  (dimmer-mode t))

;;;; -- mini-buffer --
;; TIP The 'which-key' extension is useless, just use 'mini-buffer' to search a command.
;; NOTE Choose 'helm' over other packages for its muture and active.
(use-package helm
  :init 
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode 1)
  )

;;;; -- auto indent --
(use-package aggressive-indent
  :ensure t
  :config
  ;; NOTE Only enable this mode for these modes.
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

;;;; -- key-cast --
(use-package keycast
  :ensure t
  :config
  ;; NOTE Display the key-cast in 'window-header'.
  (keycast-mode-line-mode))


;;;; -- todo --
(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-keyword-faces
	'(("TODO"   . "#FFFF00")
          ("FIXME"  . "#FF0000")
          ("NOTE"  . "#0000FF")
          ("TIP"  . "#00FF00")))
  (global-hl-todo-mode)
  )

(use-package magit-todos
  :ensure t
  :after (hl-todo magit)
  :config (magit-todos-mode 1))

;;;; -- language: latex --
;; NOTE For latex language, use the built-in 'reftex' package.

;;;; -- snippet --
;; TIP Good to have a 'template' system to avoid stupid codes in some stupid languages. (I am not saying about Java).
(use-package yasnippet
  :ensure t
  :config
  ;; TIP Use `M-e` in `vi-insert-mode' to expand the key into snippet.
  (define-key yas-minor-mode-map (kbd "M-e") yas-maybe-expand)
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :ensure t)

;;;; -- checker --
;; NOTE The correctness of 'flycheck' extension is much better than 'flymake'.
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))


;;;; -- appearance --
;; NOTE Use a mono-spaced-font like 'source code pro' or 'hack'. (Font is set by KDE)
;; TIP Use 'base16 theme', it's simple and beautiful.
;; TIP Don't use the transparent frame, it's ugly.
(setq inhibit-startup-screen t)
(setq initial-major-mode 'lisp-mode)

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar nil)


(toggle-frame-maximized)

(global-display-line-numbers-mode)

(blink-cursor-mode 0)

(global-hl-line-mode t)
(set-face-background 'hl-line "#000066")

;; -- mode line --
(setq column-number-mode t)


;;;; -- auto save --
;; FIXME auto save seems not work
(setq auto-save-interval 20)
(setq auto-save-timeout 5)
(setq auto-save-no-message nil)

;;;; -- formatter --
;; TIP Use `formatter` to format buffer automatically, instead of `<<` and `>>`.

;;;; -- fold --
;; TIP You don't need a 'index-menu' pop-up if you have 'code fold' function.
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
;; TIP Use 'zc' (fold-close) and 'zo' (fold-open).
;; TIP Use 'zm' (fold-more) and 'zr' (fold-reduce).
;; TIP Use 'zR' (fold-remove).
;; TIP Use 'zi' or 'za' (fold-invert).
(evil-define-key '(normal) 'global (kbd "z i") 'evil-toggle-fold)


;;;; -- better HL --
;; TIP use `zz` to center current line.
;; TIP use `M` to center current window.
;; TIP The `J` is for `join following lines to current-line`, and `K` for manual.
(evil-define-key '(normal visual) 'global "H" 'evil-beginning-of-line)
(evil-define-key '(normal visual) 'global "L" 'evil-end-of-line)

;;;; -- better escape --
;; TIP Use 'key-conversion': "C-[" = "Escape", "C-i" = "Tab" and "C-m" = "Return".
;; TIP The order to escape: jk > C-g > C-[ > Escape

;;;; -- location --
;; TIP Use 'C-i' and 'C-o' to navigate the jumplist, use double ' to jump-previous.
;; TIP Use `'.` to jump to `last changed location`.
;; TIP Use 'gi' to jump to `last changed location' and enter 'vi-insert-state' mode.


;;;; -- text object --
;; TIP Index the 'text object' via 'tree-sitter'.
;; TIP It's okay to use the 'paragraph text-object'.

;; TODO not work. (integrate it with [[ and ]])
(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

(use-package evil-textobj-tree-sitter
  :ensure t
  :config
  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

  ;; You can also bind multiple items and we will match the first one we can find
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
  )



;;;; -- goto --
;; gj / gk -> logical line
;; ge / GE -> backward word end / backward broad word end
;; GJ -> join line
;; Gu / GU -> downcase / upcase
;; gi -> goto 'last-changed-location' and enter 'vi-insert-state' mode.
;; TIP Use 'gz' to goto 'emacs-lisp' repl provided by 'ielm', if you didn't start a 'slime' instance.
(evil-define-key '(normal) 'global (kbd "SPC g a") 'helm-M-x)
(evil-define-key '(normal) 'global (kbd "SPC a") 'helm-M-x)


;; TIP Use `g d` to find definition, use `g r` to find references.
;; TIP Use 'C-j' and 'C-k' to show the details in 'definition result' window.
(evil-define-key '(normal) 'global (kbd "g s") 'slime-edit-definition)

(evil-define-key '(normal) 'global (kbd "g l") 'imenu)
(evil-define-key '(normal) 'global (kbd "g m") 'evil-jump-item)

;; TIP Use 'C-j' and 'C-k' to show the details in 'grep result' window.
(evil-define-key '(normal) 'global (kbd "g t") 'projectile-grep)
(evil-define-key '(normal) 'global (kbd "g T") 'hl-todo-occur)


;;;; -- buffer --
;; NOTE buffer < window < tab < frame
(evil-define-key '(normal) 'global (kbd "SPC b l") 'list-buffers)
(evil-define-key '(normal) 'global (kbd "SPC b b") 'switch-to-buffer)
(evil-define-key '(normal) 'global (kbd "SPC b B") 'switch-to-buffer-other-tab)

(evil-define-key '(normal) 'global (kbd "SPC b n") 'switch-to-next-buffer)
(evil-define-key '(normal) 'global (kbd "SPC b p") 'switch-to-prev-buffer)
;; TIP After the `buffer delete`, emacs will switch to `previous buffer`.
(evil-define-key '(normal) 'global (kbd "SPC b d") 'kill-buffer)

(evil-define-key '(normal) 'global (kbd "SPC b s") 'scratch-buffer)

;;;; -- window --
(evil-define-key '(normal) 'global (kbd "SPC s h") 'split-window-horizontally)
(evil-define-key '(normal) 'global (kbd "SPC s v") 'split-window-vertically)

;; NOTE The 'window-swap-states' can 'transpose' current window and next window.
(evil-define-key '(normal) 'global (kbd "SPC w t") 'window-swap-states)
(evil-define-key '(normal) 'global (kbd "SPC w h") 'windmove-swap-states-left)
(evil-define-key '(normal) 'global (kbd "SPC w j") 'windmove-swap-states-down)
(evil-define-key '(normal) 'global (kbd "SPC w k") 'windmove-swap-states-up)
(evil-define-key '(normal) 'global (kbd "SPC w l") 'windmove-swap-states-right)


(evil-define-key '(normal) 'global (kbd "SPC w o") 'other-window)
(evil-define-key '(normal) 'global (kbd "SPC w n") 'next-window)
(evil-define-key '(normal) 'global (kbd "SPC w p") 'previous-window)

(evil-define-key '(normal) 'global (kbd "SPC w m") 'maximize-window)
(evil-define-key '(normal) 'global (kbd "SPC w M") 'minimize-window)
(evil-define-key '(normal) 'global (kbd "SPC w b") 'balance-windows)


(evil-define-key '(normal) 'global (kbd "C-h") 'evil-window-left)
(evil-define-key '(normal) 'global (kbd "C-j") 'evil-window-down)
(evil-define-key '(normal) 'global (kbd "C-k") 'evil-window-up)
(evil-define-key '(normal) 'global (kbd "C-l") 'evil-window-right)

(evil-define-key '(normal) 'global (kbd "SPC w d") 'delete-window)
(evil-define-key '(normal) 'global (kbd "SPC w D") 'delete-other-windows)

;; TIP Undo the window that deleted accidently.
(winner-mode)
(evil-define-key '(normal) 'global (kbd "SPC w u") 'winner-undo)
(evil-define-key '(normal) 'global (kbd "SPC w U") 'winner-redo)

;;;; -- tab --
;; NOTE Besides the `tab-bar', there is a `tab-line' for each `tab'.
;; TIP Use `C-Tab` and `C-S-Tab` to cycle tabs.
;; TIP The `tab-switch` will switch to the named tab or create it.
;; NOTE Switch to a tab by its name (which reflects its buffer file name), not by its index.
(toggle-tab-bar-mode-from-frame)
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button-show nil)
(setq tab-bar-tab-hints t)

(evil-define-key '(normal) 'global (kbd "SPC t s") 'tab-switch)
(evil-define-key '(normal) 'global (kbd "SPC t t") 'tab-switch)

(evil-define-key '(normal) 'global (kbd "SPC t c") 'tab-bar-new-tab)
(evil-define-key '(normal) 'global (kbd "SPC t l") 'tab-list)
(evil-define-key '(normal) 'global (kbd "SPC t r") 'tab-bar-switch-to-recent-bar)

(defun tab-select-1 () (interactive) (tab-select 1))
(defun tab-select-2 () (interactive) (tab-select 2))
(defun tab-select-3 () (interactive) (tab-select 3))
(defun tab-select-4 () (interactive) (tab-select 4))
(defun tab-select-5 () (interactive) (tab-select 5))
(defun tab-select-6 () (interactive) (tab-select 6))
(defun tab-select-7 () (interactive) (tab-select 7))
(defun tab-select-8 () (interactive) (tab-select 8))
(defun tab-select-9 () (interactive) (tab-select 9))
(defun tab-select-0 () (interactive) (tab-select 0))
(evil-global-set-key 'normal (kbd "SPC 1") 'tab-select-1)
(evil-global-set-key 'normal (kbd "SPC 2") 'tab-select-2)
(evil-global-set-key 'normal (kbd "SPC 3") 'tab-select-3)
(evil-global-set-key 'normal (kbd "SPC 4") 'tab-select-4)
(evil-global-set-key 'normal (kbd "SPC 5") 'tab-select-5)
(evil-global-set-key 'normal (kbd "SPC 6") 'tab-select-6)
(evil-global-set-key 'normal (kbd "SPC 7") 'tab-select-7)
(evil-global-set-key 'normal (kbd "SPC 8") 'tab-select-8)
(evil-global-set-key 'normal (kbd "SPC 9") 'tab-select-9)
(evil-global-set-key 'normal (kbd "SPC 0") 'tab-select-0)

(evil-define-key '(normal) 'global (kbd "SPC t n") 'tab-next)
(evil-define-key '(normal) 'global (kbd "SPC t p") 'tab-previous)

(evil-define-key '(normal) 'global (kbd "SPC t N") 'tab-rename)

(evil-define-key '(normal) 'global (kbd "SPC t d") 'tab-close)
(evil-define-key '(normal) 'global (kbd "SPC t o") 'tab-close-other)
(evil-define-key '(normal) 'global (kbd "SPC t u") 'tab-bar-undo-close-tab)

;;;; -- frame --
(evil-define-key '(normal) 'global (kbd "SPC z z") 'toggle-frame-fullscreen)

(evil-define-key '(normal) 'global (kbd "Z R") 'restart-emacs)

;;;; -- session --
;;(desktop-save-mode 1)

;;;; -- file --
;; NOTE Use 'projectile' as a project interface layer, to 'discovery' and 'indexing' projects.
(use-package projectile
  :ensure t
  :config
  ;; Require a explicit project marker to use projectile commands.
  (setq projectile-require-project-root t)
  (projectile-mode +1)
  )

;; NOTE For better integration, use 'treemacs' as the file explorer.
;; TIP Press '?' in the 'treemacs window' for 'normal-help' and 'C-?' for 'advanced-help'.
;; TIP To navigate and operate, use 'hjkl' and 'C-{j/k}'.
;; TIP Use 'M-m' to mark multiple files.
;; NOTE The 'treemacs' treat 'projects' as a 'workspace'.
(use-package treemacs
  :ensure t
  :defer t
  :config
  ;;(treemacs-start-on-boot)
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          nil
          treemacs-indentation                     2
          treemacs-indentation-string              " "
	  ;; FIXME not work
	  treemacs-indent-guide-style              'line
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    (treemacs-resize-icons 22)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    ;; git
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  )

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)


(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;;(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
;;  :after (treemacs)
;;  :ensure t
;;  :config (treemacs-set-scope-type 'Tabs))


;; TIP Should not decice the content of file based on 'file extension name' and 'file icon (provided by file explorer)'

(evil-define-key '(normal) 'global (kbd "SPC f t") 'treemacs)
(evil-define-key '(normal) 'global (kbd "SPC f r") 'recentf)

(evil-define-key '(normal) 'global (kbd "SPC f s") 'save-buffer)

(evil-define-key '(normal) 'global (kbd "SPC f c") 'dired-create-empty-file)
(evil-define-key '(normal) 'global (kbd "SPC f C") 'dired-create-directory)

;;;; -- project --
(evil-define-key '(normal) 'global (kbd "SPC p s") 'projectile-switch-project)
(evil-define-key '(normal) 'global (kbd "SPC p b") 'projectile-switch-to-buffer)

(evil-define-key '(normal) 'global (kbd "SPC p h") 'projectile-dired)
(evil-define-key '(normal) 'global (kbd "SPC p f") 'projectile-find-file)
(evil-define-key '(normal) 'global (kbd "SPC p F") 'projectile-find-file-other-window)
(evil-define-key '(normal) 'global (kbd "SPC p d") 'projectile-find-dir)
(evil-define-key '(normal) 'global (kbd "SPC p r") 'projectile-recentf)

(evil-define-key '(normal) 'global (kbd "SPC p g") 'projectile-grep)
(evil-define-key '(normal) 'global (kbd "SPC p G") 'projectile-replace)

(evil-define-key '(normal) 'global (kbd "SPC p !") 'projectile-run-shell-command-in-root)
(evil-define-key '(normal) 'global (kbd "SPC p &") 'projectile-run-async-shell-command-in-root)

(evil-define-key '(normal) 'global (kbd "SPC p k") 'projectile-compile-project)
(evil-define-key '(normal) 'global (kbd "SPC p R") 'projectile-run-project)
(evil-define-key '(normal) 'global (kbd "SPC p P") 'projectile-package-project)
(evil-define-key '(normal) 'global (kbd "SPC p I") 'projectile-install-project)
(evil-define-key '(normal) 'global (kbd "SPC p T") 'projectile-test-project)

(evil-define-key '(normal) 'global (kbd "SPC p C") 'projectile-add-known-project)
(evil-define-key '(normal) 'global (kbd "SPC p n") 'projectile-remember-projects-under)
(evil-define-key '(normal) 'global (kbd "SPC p N") 'projectile-forget-project)

(evil-define-key '(normal) 'global (kbd "SPC p v") 'projectile-vc)

;;;; -- compile --
(evil-define-key '(normal) 'global (kbd "SPC k k") 'compile)

;;;; -- version --
(evil-define-key '(normal) 'global (kbd "SPC v") 'magit)


;;;; -- repl --
;; M-p -> previous input
;; C-<up>/<down>
;; TIP Use 'C-u' (back to indentation) and 'C-w' (word) to delete backward in insert/ex/search vi-state.
;; TIP Use 'M-p' and 'M-n' to navigate the `repl input history'.
;; TIP Use 'M-Ret' to 'close parens and return'.
;; TIP In `repl window`, you can mosue-click a `representation` to open `context-menu`.
;; TIP Use 'Tab' in repl to list all packages. (via 'slime-fuzzy-complete-symbol')

(defun slime-restart-inferior-lisp* ()
  (interactive)
  (slime-repl)
  (slime-restart-inferior-lisp))

(evil-define-key '(normal) 'global (kbd "SPC r R") 'slime)
(evil-define-key '(normal) 'global (kbd "SPC r r") 'slime-restart-inferior-lisp*)
(evil-define-key '(normal) 'global (kbd "SPC r l") 'slime-list-connections)
(evil-define-key '(normal) 'global (kbd "SPC r t") 'slime-list-threads)

(evil-define-key '(normal) 'global (kbd "SPC r c") 'slime-repl-clear-buffer)

(evil-define-key '(normal) 'global (kbd "M-c") 'slime-repl-clear-buffer)

;;;; -- evaluate --
;; TIP Use `M-n` and `M-p` to see compiler notes.

;; NOTE Only the 'slime-compile-...' commands will add compiler notes. (The 'slime-eval-...' will not.)
(evil-define-key '(normal) 'global (kbd "SPC e l") 'slime-list-compiler-notes)

(evil-define-key '(normal) 'global (kbd "SPC e w") 'slime-repl)
(evil-define-key '(normal) 'global (kbd "SPC e c") 'slime-handle-repl-shortcut)

(evil-define-key '(normal) 'global (kbd "SPC e s") 'slime-interactive-eval)
(evil-define-key '(normal) 'global (kbd "SPC e S") 'slime-load-system)
(evil-define-key '(normal) 'global (kbd "SPC e q") 'slime-repl-quicklisp-quickload)

;; TIP Don't use `slime-repl-region`, use `eval-defun` to treat the `defun-like-form` as minimal unit.
(evil-define-key '(normal) 'global (kbd "SPC e d") 'slime-eval-defun)
(evil-define-key '(normal) 'global (kbd "SPC e D") 'slime-disassemble-symbol)


;; TIP Use repl to 'resend' the last form to repl. (Seems only work in repl window.)
(evil-define-key '(normal) 'global (kbd "SPC e r") 'slime-repl-resend)
(evil-define-key '(normal) 'global (kbd "SPC e b") 'slime-eval-buffer)

(evil-define-key '(normal) 'global (kbd "SPC e I") 'slime-interrupt)
(evil-define-key '(normal) 'global (kbd "SPC e p") 'slime-sync-package-and-default-directory)

(evil-define-key '(normal) 'global (kbd "SPC e t") 'slime-toggle-trace-fdefinition)
(evil-define-key '(normal) 'global (kbd "SPC e T") 'slime-trace-dialog)

;; NOTE The 'profile' should be done by automatical scripts.
(evil-define-key '(normal) 'global (kbd "SPC e P") 'slime-profile-report)



;;;; -- inspect --
;; v -> verbose

;; h -> history entries
;; enter -> fetch
;; > -> fetch all
;; l -> prev entry (left)
;; n -> next entry
;; q -> quit

;; M-Ret -> copy down to repl
;; r/g -> re-inspect

;; p -> pprint
;; d -> describe
;; e -> eval
;; . -> show-source

;; TIP if there is no symbol under cursor, then the command will ask for a form to inspect.
(evil-define-key '(normal) 'global (kbd "SPC i i") 'slime-inspect)
(evil-define-key '(normal) 'global (kbd "SPC i I") 'slime-inspect-presentation-at-point)

(evil-define-key '(normal) 'global (kbd "SPC i e") 'slime-inspect-eval)

;;(evil-define-key '(normal) 'slime-inspector-mode-map (kbd "SPC v") 'slime-inspector-toggle-verbose)
;;(evil-define-key '(normal) 'slime-inspector-mode-map (kbd "SPC h") 'slime-inspector-history)
;;(evil-define-key '(normal) 'slime-inspector-mode-map (kbd "SPC Ret") 'slime-inspector-fetch)
;;(evil-define-key '(normal) 'slime-inspector-mode-map (kbd "SPC >") 'slime-inspector-fetch-all)
;;(evil-define-key '(normal) 'slime-inspector-mode-map (kbd "SPC l") 'slime-inspector-pop)
;;(evil-define-key '(normal) 'slime-inspector-mode-map (kbd "SPC n") 'slime-inspector-next)
;;(evil-define-key '(normal) 'slime-inspector-mode-map (kbd "SPC q") 'slime-inspector-quit)
;;
;;;;(evil-define-key '(normal) 'slime-inspector-mode-map (kbd "SPC d") 'slime-inspector-describe)
;;(evil-define-key '(normal) 'slime-inspector-mode-map (kbd "SPC g") 'slime-inspector-reinspect)
;;
;;(evil-define-key '(normal) 'slime-inspector-mode-map (kbd "SPC e") 'slime-inspector-eval)
;;(evil-define-key '(normal) 'slime-inspector-mode-map (kbd "SPC p") 'slime-inspector-pprint)
;;
;;(evil-define-key '(normal) 'slime-inspector-mode-map (kbd "SPC .") 'slime-inspector-show-source)

;;;; -- sldb --
;; n -> down
;; p -> up
;; M-n -> details down
;; M-p -> details up

;; c -> continue
;; a -> abort
;; r -> restart
;; 0..9 -> invoke restart by number
;; I -> invoke restart by name

;; v -> frame source
;; d -> eval in frame
;; e -> inspect in frame

;; s -> step
;; x -> next
;; o -> out
;; b -> break on return
;; C -> inspect condition
;; M-Ret -> copy down to repl

;; TIP Use `M-n` and `M-p` to nagivate the `backtracd` with `source form`.


;;;; -- describe --
;; NOTE the commands start with `describe-` is for `emacs lisp inferor`, and start with `slime-` is for `common lisp`.
(evil-define-key '(normal) 'global (kbd "SPC d d") 'slime-apropos-all)
;; NOTE The `slime-apropos` only list `external symbols`.
(evil-define-key '(normal) 'global (kbd "SPC d a") 'slime-apropos)
(evil-define-key '(normal) 'global (kbd "SPC d p") 'slime-apropos-package)

;; TIP Use `gs' to goto the definition of a symbol. 
(evil-define-key '(normal) 'global (kbd "SPC d s") 'slime-describe-symbol)
(evil-define-key '(normal) 'global (kbd "SPC d f") 'slime-describe-function)
;; NOTE Use `slime-browse-classes' to show the 'children' of the 'target class'.
(evil-define-key '(normal) 'global (kbd "SPC d c") 'slime-browse-classes)

;; TIP The command will ask for string if not string at point.
(evil-define-key '(normal) 'global (kbd "SPC d h") 'slime-documentation-lookup)
(evil-define-key '(normal) 'global (kbd "SPC d H") 'slime-documentation)

;;;; -- help --
;; TIP The `info` is the 'TOP-LEVEL' of manual about emacs and its packages.
(evil-define-key '(normal) 'global (kbd "SPC h h") 'info)
;; TIP Use `info-display-manual` to see `emacs package manual'.
(evil-define-key '(normal) 'global (kbd "SPC h H") 'info-display-manual)

(evil-define-key '(normal) 'global (kbd "SPC h b") 'describe-bindings)
(evil-define-key '(normal) 'global (kbd "SPC h k") 'describe-key)
(evil-define-key '(normal) 'global (kbd "SPC h c") 'describe-command)
(evil-define-key '(normal) 'global (kbd "SPC h f") 'describe-function)
(evil-define-key '(normal) 'global (kbd "SPC h m") 'describe-mode)
(evil-define-key '(normal) 'global (kbd "SPC h M") 'man)

(evil-define-key '(normal) 'global (kbd "SPC h p") 'describe-package)
(evil-define-key '(normal) 'global (kbd "SPC h P") 'finder-by-keyword)

(evil-define-key '(normal) 'global (kbd "SPC h s") 'apropos)

(evil-define-key '(normal) 'global (kbd "SPC h d") 'apropos-documentation)
(evil-define-key '(normal) 'global (kbd "SPC h D") 'shortdoc)

(evil-define-key '(normal) 'global (kbd "SPC h o") 'apropos-user-option)

(evil-define-key '(normal) 'global (kbd "SPC h l") 'apropos-library)


;;;; -- comment --
(evil-define-key '(normal visual) 'global (kbd "g c") 'comment-line)
(evil-define-key '(normal visual) 'global (kbd "g C") 'comment-box)

;;;; -- paredit (sexp) --
;; TIP Use `)` to move over the list.
;; TIP Use `g m` to move to the matching item.
(evil-define-key '(normal visual) 'global (kbd "SPC s w") 'sp-wrap-round)
(evil-define-key '(normal visual) 'global (kbd "SPC s W") 'sp-splice-sexp)

(evil-define-key '(normal) 'global (kbd "SPC s m") 'sp-mark-sexp)
(evil-define-key '(normal) 'global (kbd "SPC s k") 'sp-kill-sexp)

(evil-define-key '(normal) 'global (kbd "SPC s s") 'sp-forward-slurp-sexp)
(evil-define-key '(normal) 'global (kbd "SPC s S") 'sp-backward-slurp-sexp)
(evil-define-key '(normal) 'global (kbd "SPC s b") 'sp-forward-barf-sexp)
(evil-define-key '(normal) 'global (kbd "SPC s B") 'sp-backward-barf-sexp)

(evil-define-key '(normal) 'global (kbd "SPC s t") 'sp-transpose-sexp)
(evil-define-key '(normal) 'global (kbd "SPC s r") 'sp-raise-sexp)

;;;; -- bookmark --

;;;; -- browser --
(setq browse-url-browser-function 'eww-browse-url)

;;;; -- dict --
(evil-define-key '(normal) 'global (kbd "SPC u d") 'dictionary-search)


;;;; -- dashboard --
;; An idiot admires complexity, a genius admires simplicity.
;;                                                            ― Terry Davis
;;(setq initial-scratch-message (choice))

;;;; -- after init --
(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  "Eval after Emacs init."
  (slime))







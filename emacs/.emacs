
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

;; TODO mini buffer extensions: smex, ido, helm -> (video introseems not work

;; TODO integrate projectile

;; TODO org mode

;; TODO a LLM ai completion


;; NOTE It's also okay to steal some ideas from others' dotfiles.
;; TIP Use 'Super+{alpha}' to switch to useful programs (wmctrl -a "gnu emacs" || emacs): terminal, emacs, browser.
;; TIP Use 'Super+{num}' to switch to a window in KDE.
;; TIP Use 'Ctrl+{num}' to switch to a tab in web browser.
;; TIP Use 'customize' command to list the options provided by a package, and export them into '.emacs' later.
;; TIP To browse the firefox, use 'vimium' extension.

;;;; extension: package
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
		 slime-fancy
		 slime-banner
		 slime-xref-browser
		 slime-company)))

;; Setup load-path and autoloads
;;(add-to-list 'load-path "~/dir/to/cloned/slime")
;;(require 'slime-autoloads)


;;;; -- complete --
;; NOTE The 'company' extension has better integration than 'auto-complete'.
;; TIP Use tab or C-p in insert-mode to trigger completion window
;; TIP Use 'C-n' and 'C-p' to select 'complete entry' in 'vi-insert-mode'.
;; TIP If you need the same number of key-stroke, why use fuzzy?
;; TIP Use 'key-conversion' to translate 'C-m' to 'RET'.

;; TODO polish the config of company
(use-package company
  :ensure t
  :config
  ;; FIXME seems not work
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


;;;; -- base16 theme -- 
(use-package base16-theme
  :ensure t
  :config
  ;; FIXME the customize may save loaded theme
  (load-theme 'base16-windows-highcontrast t))


;;;; -- mode line --
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))


;;;; -- jump anywhere --
;; TIP You don't need to use `smooth scroll', just use `avy' or `grep'.
(use-package avy
  :ensure t
  :config 
  ;; TIP Use "SPC J" to jump to a word.
  (evil-define-key '(normal) 'global (kbd "SPC j") 'avy-goto-word-0)
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
;; TIP The `which-key' extension is useless, just use `vertico` to search a command.
(use-package vertico
  :ensure t
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 50) ;; Show more candidates
  (vertico-resize nil) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle nil) ;; Enable cycling for `vertico-next/previous'
  :init 
  (vertico-mode))

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
(toggle-tab-bar-mode-from-frame)
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
;; TIP The `J` is for `join lines`, and `K` for manual.
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

;; FIXME not work
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
;; TIP Use 'gz' to goto 'emacs-lisp repl'.
(evil-define-key '(normal) 'global (kbd "SPC g a") 'execute-extended-command)
(evil-define-key '(normal) 'global (kbd "SPC a") 'execute-extended-command)


;; TIP Use `g d` to find definition, use `g r` to find references.
(evil-define-key '(normal) 'global (kbd "g s") 'slime-edit-definition)

(evil-define-key '(normal) 'global (kbd "g l") 'imenu)

(evil-define-key '(normal) 'global (kbd "g t") 'project-find-regexp)
(evil-define-key '(normal) 'global (kbd "g T") 'hl-todo-occur)

(evil-define-key '(normal) 'global (kbd "g m") 'evil-jump-item)

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

;; FIXME allow to cycle window layout
(defun windmove-swap-states-auto ()
  (interactive)
  (ignore-errors (windmove-swap-states-right))
  (ignore-errors (windmove-swap-states-left))
  )
(evil-define-key '(normal) 'global (kbd "SPC w t") 'windmove-swap-states-auto)

(evil-define-key '(normal) 'global (kbd "SPC w o") 'other-window)
(evil-define-key '(normal) 'global (kbd "SPC w n") 'next-window)
(evil-define-key '(normal) 'global (kbd "SPC w p") 'previous-window)

;; TIP Undo the window that deleted accidently.
(winner-mode)
(evil-define-key '(normal) 'global (kbd "SPC w u") 'winner-undo)
(evil-define-key '(normal) 'global (kbd "SPC w U") 'winner-redo)


(evil-define-key '(normal) 'global (kbd "C-p") 'other-window)

(evil-define-key '(normal) 'global (kbd "C-h") 'evil-window-left)
(evil-define-key '(normal) 'global (kbd "C-j") 'evil-window-down)
(evil-define-key '(normal) 'global (kbd "C-k") 'evil-window-up)
(evil-define-key '(normal) 'global (kbd "C-l") 'evil-window-right)

(evil-define-key '(normal) 'global (kbd "SPC w d") 'delete-window)
(evil-define-key '(normal) 'global (kbd "SPC w D") 'delete-other-windows)

(evil-define-key '(normal) 'global (kbd "SPC w m") 'maximize-window)
(evil-define-key '(normal) 'global (kbd "SPC w n") 'minimize-window)

;;;; -- tab --
;; NOTE Besides the `tab-bar', there is a `tab-line' for each `tab'.
;; TIP Use `C-Tab` and `C-S-Tab` to cycle tabs.
;; TIP The `tab-switch` will switch to the named tab or create it.
;; NOTE Switch to a tab by its name (which reflects its buffer file name), not by its index.
(evil-define-key '(normal) 'global (kbd "SPC t s") 'tab-switch)
(evil-define-key '(normal) 'global (kbd "SPC t c") 'tab-bar-new-tab)
(evil-define-key '(normal) 'global (kbd "SPC t l") 'tab-list)
(evil-define-key '(normal) 'global (kbd "SPC t r") 'tab-bar-switch-to-recent-bar)

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
;; TODO explore the dired. 
;; TIP Should not decice the content of file based on 'file extension name' and 'file icon (provided by file explorer)'
(evil-define-key '(normal) 'global (kbd "SPC f t") 'dired)
(evil-define-key '(normal) 'global (kbd "SPC f r") 'recentf)

(evil-define-key '(normal) 'global (kbd "SPC f s") 'save-buffer)

;;;; -- project --
(evil-define-key '(normal) 'global (kbd "SPC p s") 'project-switch-project)

(evil-define-key '(normal) 'global (kbd "SPC p h") 'project-dired)
(evil-define-key '(normal) 'global (kbd "SPC p f") 'project-find-file)
(evil-define-key '(normal) 'global (kbd "SPC p d") 'project-find-dir)

;;;; -- compile --
(evil-define-key '(normal) 'global (kbd "SPC k k") 'compile)

;; FIXME not work
(defun project-find-file-other-window ()
  "This not work."
  (interactive)
  (project-other-window-command)
  (project-find-file))
(evil-define-key '(normal) 'global (kbd "SPC p F") 'project-find-file-other-window)

(evil-define-key '(normal) 'global (kbd "SPC p g") 'project-find-regexp)

(evil-define-key '(normal) 'global (kbd "SPC p n") 'project-remember-projects-under)
(evil-define-key '(normal) 'global (kbd "SPC p N") 'project-forget-project)

;;;; -- version --
(evil-define-key '(normal) 'global (kbd "SPC v") 'magit)


;;;; -- repl --
;; M-p -> previous input
;; C-<up>/<down>
;; TIP Use 'C-u' (back to indentation) and 'C-w' (word) to delete backward in insert/ex/search vi-state.
;; TIP Use 'M-p' and 'M-n' to navigate the `repl input history'.
;; TIP Use 'M-Ret' to 'close parens and return'.
;; TIP In `repl window`, you can mosue-click a `representation` to open `context-menu`.

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

;; TODO disable the `C-c M-i` comletion (C-c tab)
;; TODO disassemble
;; TODO profile
;; TODO the compile function. the compiler-notes for compile, not for evaluate
;;(evil-define-key '(normal) 'global (kbd "SPC e l") 'slime-list-compiler-notes)
;;(evil-define-key '(normal) 'global (kbd "SPC e e") 'slime-list-connections)
;; TODO resend last form

(evil-define-key '(normal) 'global (kbd "SPC e w") 'slime-repl)
(evil-define-key '(normal) 'global (kbd "SPC e c") 'slime-handle-repl-shortcut)

(evil-define-key '(normal) 'global (kbd "SPC e s") 'slime-interactive-eval)
(evil-define-key '(normal) 'global (kbd "SPC e S") 'slime-load-system)
;; TIP Don't use `slime-repl-region`, use `eval-defun` to treat the `defun-like-form` as minimal unit.
(evil-define-key '(normal) 'global (kbd "SPC e d") 'slime-eval-defun)
(evil-define-key '(normal) 'global (kbd "SPC e r") 'slime-eval-last-expression-in-repl)
(evil-define-key '(normal) 'global (kbd "SPC e b") 'slime-eval-buffer)

;; TODO eval and pprint

(evil-define-key '(normal) 'global (kbd "SPC e I") 'slime-interrupt)
(evil-define-key '(normal) 'global (kbd "SPC e p") 'slime-sync-package-and-default-directory)

(evil-define-key '(normal) 'global (kbd "SPC e t") 'slime-toggle-trace-fdefinition)
(evil-define-key '(normal) 'global (kbd "SPC e T") 'slime-trace-dialog)


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


;;;; -- grep --


;;;; -- describe --
;; NOTE the commands start with `describe-` is for `emacs lisp inferor`, and start with `slime-` is for `common lisp`.
(evil-define-key '(normal) 'global (kbd "SPC d d") 'slime-apropos-all)
;; NOTE The `slime-apropos` only list `external symbols`.
(evil-define-key '(normal) 'global (kbd "SPC d a") 'slime-apropos)
(evil-define-key '(normal) 'global (kbd "SPC d p") 'slime-apropos-package)

(evil-define-key '(normal) 'global (kbd "SPC d s") 'slime-describe-symbol)
(evil-define-key '(normal) 'global (kbd "SPC d S") 'slime-edit-definition)
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


(evil-define-key '(normal) 'global (kbd "SPC h p") 'describe-package)
(evil-define-key '(normal) 'global (kbd "SPC h P") 'finder-by-keyword)

(evil-define-key '(normal) 'global (kbd "SPC h s") 'apropos)

(evil-define-key '(normal) 'global (kbd "SPC h M") 'man)

(evil-define-key '(normal) 'global (kbd "SPC h d") 'apropos-documentation)
(evil-define-key '(normal) 'global (kbd "SPC h D") 'shortdoc)

(evil-define-key '(normal) 'global (kbd "SPC h o") 'apropos-user-option)

(evil-define-key '(normal) 'global (kbd "SPC h l") 'apropos-library)


;;;; -- comment --
(evil-define-key '(normal visual) 'global (kbd "SPC c") 'comment-line)
(evil-define-key '(normal visual) 'global (kbd "SPC C") 'comment-box)

;;;; -- paredit --
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


;;;; -- dashboard --
;; An idiot admires complexity, a genius admires simplicity.
;;                                                            ― Terry Davis

;;;; -- after init --
(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  (slime))


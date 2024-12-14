;; While any text editor can save your files, only Emacs can save your soul. 

;; NOTE: I steal some good ideas from "https://neovim.io/" and "https://www.gnu.org/software/emacs/". (Interesting to read the manual)


;; TODO: add the make compile command 
;; TODO: org mode
;; TODO: recovery session
;; TODO: search built in package
;; TODO: a better auto save extension
;; TODO: integratarchive-priorities`e ranger like filer explorer inside emacs.
;; TODO: vim surround plugin
;; TODO: a telescope like function -> fold zen
;; TODO: a LLM ai completion
;; TODO: highlight todo and fixme
;; TODO: emacs macro or viim macro?

;; TIP: Use `xmctrl` and `x global shortcut` to swtich to `emacs`: `wmctrl -a "gnu emacs" || emacs`
;; TIP: Use `Super+{num}' to switch to a window in KDE.
;; TIP: Use `Ctrl+{num}` to switch to a tab in browser.

;;;; extension: package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package- 
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;;(package-refresh-contents)

;;;; extension: evil
;; TIP: Use `C-z` to toggle between `vi-mode` and `emacs-mode`.
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

(evil-set-initial-state 'magit-status-mode 'emacs)

;;;; extension: evil-collection
(use-package evil-collection
  :after evil
  :ensure t
  :custom 
  (evil-collection-want-unimpaired-p nil) 
  
  ;; NOTE: evil-collection will setup a mode naemd `t` for `sldb` and `slime-inspector`.
  (evil-collection-setup-debugger-keys t)
  (evil-collection-want-find-usages-bindings t)
  (evil-collection-setup-minibuffer t)

  (evil-collection-term-sync-state-and-mode-p t)

  :config
  (evil-collection-init))


;;;; extension: evil-escape
(use-package evil-escape
  :ensure t)
(evil-escape-mode)
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)


;;;; extension: magit
(use-package magit
  :ensure t)

;;;; extension: elcord
(use-package elcord
  :ensure t)
(require 'elcord)
(elcord-mode)

;;;; extension: smart-parens
(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

;;;; extension: slime
(use-package slime
  :ensure t)

;; Setup load-path and autoloads
;;(add-to-list 'load-path "~/dir/to/cloned/slime")
;;(require 'slime-autoloads)

(setq inferior-lisp-program "ros dynamic-space-size=4GiB run")
(setq slime-contribs '(slime-asdf
                       slime-fancy
                       slime-banner
                       slime-xref-browser))

;;;;

;;;; extension: orderless
;;(use-package orderless
;;  :ensure t
;;  :custom
;;  (completion-styles '(orderless basic))
;;  (completion-category-overrides '((file (styles basic partial-completion)))))

;;;; extension: corfu

;;;; extension: auto-complete
(use-package auto-complete
  :ensure t)

(ac-config-default)

(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

(setq ac-auto-start 2)
(setq ac-auto-show-menu nil)

;; TIP: If you need the same number of key-stroke, why use fuzzy?
(setq ac-use-fuzzy t)


;;;; extension: ac-slime
(use-package ac-slime
  :ensure t)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;;;; extension: auto-read-only
;;(use-package auto-read-only
;;  :ensure t
;;  :custom
;;  (add-to-list 'auto-read-only-file-regexps ".*")
;;  :config
;;  (auto-read-only-mode 1)
;;  (add-to-list 'auto-read-only-file-regexps ".*")
;;  )

(use-package hardhat
  :ensure t
  :config
  (global-hardhat-mode 1)
  (push ".*/.roswell/src/.*" hardhat-fullpath-protected-regexps)
  )

;;;; extension: markdown-mode
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;;;; extension: base16 theme
(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-windows-highcontrast t))

;;(defvar base16-sakurawald-theme-colors
;;  '(:base00 "#1a1b26"
;;    :base01 "#16161e"
;;    :base02 "#2f3549"
;;    :base03 "#444b6a"
;;    :base04 "#787c99"
;;    :base05 "#a9b1d6"
;;    :base06 "#cbccd1"
;;    :base07 "#d5d6db"
;;    :base08 "#c0caf5"
;;    :base09 "#a9b1d6"
;;    :base0A "#0db9d7"
;;    :base0B "#9ece6a"
;;    :base0C "#b4f9f8"
;;    :base0D "#2ac3de"
;;    :base0E "#bb9af7"
;;    :base0F "#f7768e")
;;  "All colors for Base16 SakuraWald are defined here.")
;;(deftheme base16-sakurawald)
;;(base16-theme-define 'base16-sakurawald base16-sakurawald-theme-colors)
;;(provide-theme 'base16-sakurawald)
;;(provide 'base16-sakurawald-theme)

;;;; -- extension: smart mode line --
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;;;; -- extension: all-the-cons --
;; FIXME: the icon is not show
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;;(all-the-icons-wicon "tornado" :face 'all-the-icons-blue)

;;;; -- extension: avy --
;; TIP: You don't need to use `smooth scroll', just use `avy' or `grep'.
(use-package avy
  :ensure t
  :config 
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

;;;; -- extension: dimmer --
(use-package dimmer
  :ensure t
  :config 
  (dimmer-mode t))

;;;; -- extension: vertico -- 
;; TIP: The `which-key' extension is useless, just use `vertico` to search a command.

(use-package vertico
  :ensure t
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 50) ;; Show more candidates
  (vertico-resize nil) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle nil) ;; Enable cycling for `vertico-next/previous'
  :init 
  (vertico-mode))

;;;; -- extension: aggressive-indent --
(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  )

;;;; -- extension: keycast --
(use-package keycast
  :ensure t
  :config
  ;;(keycast-mode-line-mode)
  (keycast-header-line-mode)
  ;;(keycast-tab-bar-mode)
  )


;;;; -- extension: hl-todo --
(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("NOTE"  . "#FF0000")
        ("TIP"  . "#FF0000")))
  (global-highlight-todo-mode))

;;;; -- extension: magit-todos --
(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

;;;; -- extension: latex --
(use-package auctex
  :ensure t
  :config
  )

;;;; -- extension: snippet
(use-package yasnippet
  :ensure t
  :config
  ;; TIP: Use `M-e` in `vi-insert-mode' to expand the key into snippet.
  (define-key yas-minor-mode-map (kbd "M-e") yas-maybe-expand)
  (yas-global-mode 1)
  )


(use-package yasnippet-snippets
  :ensure t)


;;;; -- appearance --
;; TIP: Use `base16 theme', it's simple and beautiful.
;; TIP: Don't use the transparent frame, it's ugly.
(setq inhibit-startup-screen t)
(setq initial-major-mode 'lisp-mode)

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-tab-bar-mode-from-frame)
(toggle-scroll-bar nil)

(toggle-frame-maximized)

(global-display-line-numbers-mode)
(global-hl-line-mode t)

(blink-cursor-mode 0)
(set-face-background 'hl-line "#000066")

;; TODO: custom font
;; TODO: reset emacs options -> customize

;; -- mode line --
(setq column-number-mode t)


;;;; -- auto save --
(setq auto-save-interval 5)
(setq auto-save-no-message t)

;;;; -- formatter --
;; TIP: Use `formatter` instead of `<<` and `>>`.

;;(evil-define-key '(normal) 'global (kbd "M-j") 'delete-indentation)

;;;; -- better HL --
;; TIP: use `zz` to center current line.
;; TIP: use `M` to center current window.
;; TIP: The `J` is for `join lines`, and `K` for manual.
(evil-define-key '(normal visual) 'global "H" 'evil-beginning-of-line)
(evil-define-key '(normal visual) 'global "L" 'evil-end-of-line)

;;;; -- better escape --
;; TIP: Use "key-conversion", like: "C-[" = "Escape", "C-i" = "Tab" and "C-m" = "Return".
;; TIP: Use jk or C-g to vi-keyboard-quit in vi-mode. (use v key to switch from visual-mode to normal-mode)
;; TIP: The order: C-g > C-[ > Escape

;;;; -- location --
;; TIP: The `C-i` and `C-o` will jump in a jumplist, while the `''` will jump only between 2 entries.
;; NOTE: The `C-i` is equal to `Tab` key, if you bind the `Tab` key to a command, then `C-i` binding will not work.
;; TIP: Use `'.` to jump to `last change location`.


;;;; -- text object --


;;;; -- completion --
;; Use tab or C-p in insert-mode to trigger completion window

;;;; -- goto --
;; gj / gk -> logical line
;; ge / GE -> backward word end / backward broad word end
;; GJ -> join line
;; Gu / GU -> downcase / upcase
(evil-define-key '(normal) 'global (kbd "SPC g a") 'execute-extended-command)
(evil-define-key '(normal) 'global (kbd "SPC a") 'execute-extended-command)


;; TIP: Use `g d` to find definition, use `g r` to find references.
(evil-define-key '(normal) 'global (kbd "g s") 'slime-edit-definition)

(evil-define-key '(normal) 'global (kbd "g t") 'project-find-regexp)

(evil-define-key '(normal) 'global (kbd "g m") 'evil-jump-item)

;;;; -- buffer --
;; NOTE: buffer < window < tab < frame

;; TODO: buffers in project ?
(evil-define-key '(normal) 'global (kbd "SPC b l") 'list-buffers)
(evil-define-key '(normal) 'global (kbd "SPC b b") 'switch-to-buffer)
(evil-define-key '(normal) 'global (kbd "SPC b B") 'switch-to-buffer-other-tab)

(evil-define-key '(normal) 'global (kbd "SPC b n") 'switch-to-next-buffer)
(evil-define-key '(normal) 'global (kbd "SPC b p") 'switch-to-prev-buffer)
;; TIP: After the `buffer delete`, emacs will switch to `previous buffer`.
(evil-define-key '(normal) 'global (kbd "SPC b d") 'kill-buffer)

(evil-define-key '(normal) 'global (kbd "SPC b s") 'scratch-buffer)

;;;; -- window --
(evil-define-key '(normal) 'global (kbd "SPC s h") 'split-window-horizontally)
(evil-define-key '(normal) 'global (kbd "SPC s v") 'split-window-vertically)

(evil-define-key '(normal) 'global (kbd "SPC w o") 'other-window)
(evil-define-key '(normal) 'global (kbd "SPC w n") 'next-window)
(evil-define-key '(normal) 'global (kbd "SPC w p") 'previous-window)

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
;; TIP: Use `C-Tab` and `C-S-Tab` to cycle tabs.

;; TODO: 0-9 for quick tab switch
;; TIP: The `tab-switch` will switch to the named tab or create it.
(evil-define-key '(normal) 'global (kbd "SPC t s") 'tab-switch)
(evil-define-key '(normal) 'global (kbd "SPC t c") 'tab-bar-new-tab)
(evil-define-key '(normal) 'global (kbd "SPC t t") 'tab-switcher)
(evil-define-key '(normal) 'global (kbd "SPC t l") 'tab-list)
(evil-define-key '(normal) 'global (kbd "SPC t r") 'tab-bar-switch-to-recent-bar)

(evil-define-key '(normal) 'global (kbd "SPC t n") 'tab-next)
(evil-define-key '(normal) 'global (kbd "SPC t p") 'tab-previous)

(evil-define-key '(normal) 'global (kbd "SPC t N") 'tab-rename)

(evil-define-key '(normal) 'global (kbd "SPC t d") 'tab-delete)
(evil-define-key '(normal) 'global (kbd "SPC t o") 'tab-close-other)
(evil-define-key '(normal) 'global (kbd "SPC t u") 'tab-bar-undo-close-tab)

;;;; -- frame --
(evil-define-key '(normal) 'global (kbd "SPC z z") 'toggle-frame-fullscreen)
(evil-define-key '(normal) 'global (kbd "SPC z r") 'restart-emacs)

;;;; -- session --
(desktop-save-mode 1)

;;;; -- file --
(evil-define-key '(normal) 'global (kbd "SPC f t") 'dired)
(evil-define-key '(normal) 'global (kbd "SPC f r") 'recentf)

;;;; -- project --
;; TODO: integrate projectile
(evil-define-key '(normal) 'global (kbd "SPC p s") 'project-switch-project)

(evil-define-key '(normal) 'global (kbd "SPC p h") 'project-dired)
(evil-define-key '(normal) 'global (kbd "SPC p f") 'project-find-file)
(evil-define-key '(normal) 'global (kbd "SPC p d") 'project-find-dir)

;;;; -- compile --
(evil-define-key '(normal) 'global (kbd "SPC k k") 'compile)



;; FIXME: not work
(defun project-find-file-other-window ()
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
;; TIP: Use `M-Ret` to `close parens and return`.
;; TIP: Use `C-u` (back to indentation) and `C-w` (word) to delete backward in insert/ex/search vi-state.
;; TIP: In `repl window`, you can mosue-click a `representation` to open `context-menu`.

;; TODO: forward and backward repl input
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
;; TIP: Use `M-n` and `M-p` to see compiler notes.

;; TODO: disable the `C-c M-i` comletion (C-c tab)
;; TODO: disassemble
;; TODO: profile

;; TODO: the compile function. the compiler-notes for compile, not for evaluate
;;(evil-define-key '(normal) 'global (kbd "SPC e l") 'slime-list-compiler-notes)
;;(evil-define-key '(normal) 'global (kbd "SPC e e") 'slime-list-connections)
;; TODO: resend last form

(evil-define-key '(normal) 'global (kbd "SPC e w") 'slime-repl)
(evil-define-key '(normal) 'global (kbd "SPC e c") 'slime-handle-repl-shortcut)

(evil-define-key '(normal) 'global (kbd "SPC e s") 'slime-interactive-eval)
(evil-define-key '(normal) 'global (kbd "SPC e S") 'slime-load-system)
;; TIP: Don't use `slime-repl-region`, use `eval-defun` to treat the `defun-like-form` as minimal unit.
(evil-define-key '(normal) 'global (kbd "SPC e d") 'slime-eval-defun)
(evil-define-key '(normal) 'global (kbd "SPC e r") 'slime-eval-last-expression-in-repl)
(evil-define-key '(normal) 'global (kbd "SPC e b") 'slime-eval-buffer)

;; TODO: eval and pprint

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

;; TIP: if there is no symbol under cursor, then the command will ask for a form to inspect.
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

;; TIP: Use `M-n` and `M-p` to nagivate the `backtracd` with `source form`.

(evil-set-initial-state 'sldb-mode 'emacs)

;;;; -- grep --


;;;; -- describe --
;; NOTE: the commands start with `describe-` is for `emacs lisp inferor`, and start with `slime-` is for `common lisp`.
(evil-define-key '(normal) 'global (kbd "SPC d d") 'slime-apropos-all)
;; NOTE: The `slime-apropos` only list `external symbols`.
(evil-define-key '(normal) 'global (kbd "SPC d a") 'slime-apropos)
(evil-define-key '(normal) 'global (kbd "SPC d p") 'slime-apropos-package)

(evil-define-key '(normal) 'global (kbd "SPC d s") 'slime-describe-symbol)
(evil-define-key '(normal) 'global (kbd "SPC d S") 'slime-edit-definition)
(evil-define-key '(normal) 'global (kbd "SPC d f") 'slime-describe-function)

;; TIP: The command will ask for string if not string at point.
(evil-define-key '(normal) 'global (kbd "SPC d h") 'slime-documentation-lookup)
(evil-define-key '(normal) 'global (kbd "SPC d H") 'slime-documentation)

;;;; -- help --
;; TIP: The `info` is the all-in-one manual about emacs and its packages.
(evil-define-key '(normal) 'global (kbd "SPC h h") 'info)
;; TIP: Use `info-display-manual` to see `emacs package manual'.
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
;; TIP: Use `)` to move over the list.
;; TIP: Use `g m` to move to the matching item.
(evil-define-key '(normal visual) 'global (kbd "SPC s w") 'sp-wrap-round)
(evil-define-key '(normal visual) 'global (kbd "SPC s W") 'sp-splice-sexp)

(evil-define-key '(normal) 'global (kbd "SPC s m") 'sp-mark-sexp)
(evil-define-key '(normal) 'global (kbd "SPC s k") 'sp-kill-sexp)

(evil-define-key '(normal) 'global (kbd "SPC s s") 'sp-slurp)
(evil-define-key '(normal) 'global (kbd "SPC s b") 'sp-barf)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(hl-todo vertico sublimity smartparens restart-emacs orderless markdown-mode magit keycast hardhat evil-escape evil-collection elcord doom-modeline dimmer corfu base16-theme avy auto-read-only all-the-icons aggressive-indent ac-slime)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

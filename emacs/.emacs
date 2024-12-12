;; NOTE: I steal some good ideas from "https://neovim.io/" and "https://www.gnu.org/software/emacs/". (Interesting to read the manual)

;; TODO: recovery session

;;;; extension: package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;;(package-refresh-contents)

;;;; extension: evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

 (setq evil-want-C-u-scroll t)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(evil-mode 1)

(evil-set-initial-state 'magit-status-mode 'emacs)

;;;; extension: evil-escape
(use-package evil-escape
  :ensure t)
(evil-escape-mode)
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)


;;;; extension: magit
(use-package magit
  :ensure t)

;;;; evil-collection 
(use-package evil-collection
  :ensure t)
(evil-collection-init)


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

(setq inferior-lisp-program "ros dynamic-space-size=4GiB run")

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

(setq ac-use-fuzzy t)


;;;; extension: ac-slime
(use-package ac-slime
  :ensure t)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))


;;;; -- appearance --
(setq inhibit-startup-screen t)
(setq initial-major-mode 'lisp-mode)

(global-display-line-numbers-mode)

(tool-bar-mode -1)
(toggle-tab-bar-mode-from-frame)

(toggle-scroll-bar nil)
(toggle-frame-maximized)

;;(load-theme 'manoj-dark)

;; line wrap

;;;; -- auto save --


;;;; -- formatter --
;; TIP: Use `formatter` instead of `<<` and `>>`.

(evil-define-key '(normal) 'global (kbd "M-j") 'delete-indentation)

;;;; -- better HJKL --
;; TIP: use `zz` to center current line.
;; TIP: use `M` to center current window.
(evil-define-key '(normal) 'global "H" 'evil-beginning-of-line)
(evil-define-key '(normal) 'global "J" 'evil-forward-paragraph)
(evil-define-key '(normal) 'global "K" 'evil-backward-paragraph)
(evil-define-key '(normal) 'global "L" 'evil-end-of-line)

;;;; -- better escape --
;; TIP: Use "key-conversion", like: "C-[" = "Escape", "C-i" = "Tab" and "C-m" = "Return".
;; TIP: Use jk or C-g to vi-keyboard-quit in vi-mode. (use v key to switch from visual-mode to normal-mode)
;; TIP: The order: C-g > C-[ > Escape

;;;; -- location --
;; TIP: The `C-i` and `C-o` will jump in a jumplist, while the `''` will jump only between 2 entries.
;; NOTE: The `C-i` is equal to `Tab` key, if you bind the `Tab` key to a command, then `C-i` binding will not work.


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

;;;; -- buffer --
;; NOTE: buffer < window < tab < frame
(evil-define-key '(normal) 'global (kbd "SPC b l") 'list-buffers)
(evil-define-key '(normal) 'global (kbd "SPC b b") 'switch-to-buffer)
(evil-define-key '(normal) 'global (kbd "SPC b B") 'switch-to-buffer-other-tab)

(evil-define-key '(normal) 'global (kbd "SPC b n") 'switch-to-next-buffer)
(evil-define-key '(normal) 'global (kbd "SPC b p") 'switch-to-prev-buffer)
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
(evil-define-key '(normal) 'global (kbd "SPC t t") 'tab-select)
(evil-define-key '(normal) 'global (kbd "SPC t l") 'tab-list)
(evil-define-key '(normal) 'global (kbd "SPC t r") 'tab-bar-switch-to-recent-bar)
(evil-define-key '(normal) 'global (kbd "SPC t s") 'tab-switch)

(evil-define-key '(normal) 'global (kbd "SPC t d") 'tab-delete)
(evil-define-key '(normal) 'global (kbd "SPC t o") 'tab-close-other)
(evil-define-key '(normal) 'global (kbd "SPC t u") 'tab-bar-undo-close-tab)

(evil-define-key '(normal) 'global (kbd "SPC t n") 'tab-next)
(evil-define-key '(normal) 'global (kbd "SPC t p") 'tab-previous)

(evil-define-key '(normal) 'global (kbd "SPC t N") 'tab-rename)

;;;; -- frame --
(evil-define-key '(normal) 'global (kbd "SPC z z") 'toggle-frame-fullscreen)
(evil-define-key '(normal) 'global (kbd "SPC z r") 'restart-emacs)


;;;; -- file --
(evil-define-key '(normal) 'global (kbd "SPC f t") 'dired)
(evil-define-key '(normal) 'global (kbd "SPC f r") 'recentf)

;;;; -- repl --
;; M-p -> previous input
;; C-<up>/<down>
;; M-Ret -> close return
(evil-define-key '(normal) 'global (kbd "SPC r R") 'slime)
(evil-define-key '(normal) 'global (kbd "SPC r r") 'slime-restart-connection-at-point)
(evil-define-key '(normal) 'global (kbd "SPC r l") 'slime-list-connections)
(evil-define-key '(normal) 'global (kbd "SPC r t") 'slime-list-threads)

(evil-define-key '(normal) 'global (kbd "SPC r c") 'slime-repl-clear-buffer)

(evil-define-key '(normal) 'global (kbd "M-c") 'slime-repl-clear-buffer)

;;;; -- evaluate --
(evil-define-key '(normal) 'global (kbd "SPC e e") 'slime-list-connections)
(evil-define-key '(normal) 'global (kbd "SPC e w") 'slime-repl)

(evil-define-key '(normal) 'global (kbd "SPC e d") 'slime-eval-defun)
(evil-define-key '(normal) 'global (kbd "SPC e r") 'slime-eval-region)
(evil-define-key '(normal) 'global (kbd "SPC e b") 'slime-eval-buffer)
(evil-define-key '(normal) 'global (kbd "SPC e I") 'slime-interrupt)
(evil-define-key '(normal) 'global (kbd "SPC e p") 'slime-sync-package-and-default-directory)

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
(evil-set-initial-state 'sldb-mode 'emacs)

;;;; -- grep --

;;;; -- describe --
(evil-define-key '(normal) 'global (kbd "SPC d k") 'describe-key)
(evil-define-key '(normal) 'global (kbd "SPC d c") 'describe-command)
(evil-define-key '(normal) 'global (kbd "SPC d m") 'describe-mode)
(evil-define-key '(normal) 'global (kbd "SPC d b") 'describe-bindings)

(evil-define-key '(normal) 'global (kbd "SPC d p") 'describe-package)
(evil-define-key '(normal) 'global (kbd "SPC d s") 'slime-describe-symbol)
(evil-define-key '(normal) 'global (kbd "SPC d S") 'slime-edit-definition)
(evil-define-key '(normal) 'global (kbd "SPC d f") 'slime-describe-function)
(evil-define-key '(normal) 'global (kbd "SPC d h") 'slime-documentation)
(evil-define-key '(normal) 'global (kbd "SPC d H") 'slime-documentation-lookup)

;;;; -- comment --
(evil-define-key '(normal) 'global (kbd "SPC c") 'comment-line)
(evil-define-key '(normal) 'global (kbd "SPC C") 'comment-box)

;;;; -- paredit --
;; TIP: Use `)` to move over the list.
;; TIP: Use `g m` to move to the matching item.
(evil-define-key '(normal) 'global (kbd "SPC s w") 'sp-wrap-round)
(evil-define-key '(normal) 'global (kbd "SPC s W") 'sp-splice-sexp)

(evil-define-key '(normal) 'global (kbd "SPC s m") 'sp-mark-sexp)
(evil-define-key '(normal) 'global (kbd "SPC s k") 'sp-kill-sexp)

(evil-define-key '(normal) 'global (kbd "SPC s s") 'sp-slurp)
(evil-define-key '(normal) 'global (kbd "SPC s b") 'sp-barf)

(evil-define-key '(normal) 'global (kbd "SPC s t") 'sp-transpose-sexp)
(evil-define-key '(normal) 'global (kbd "SPC s r") 'sp-raise-sexp)

;;;; -- bookmark --


;;;; -- post effect --
(slime)

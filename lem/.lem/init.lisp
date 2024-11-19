(in-package :lem-user)

;; some useful commands: list-modes, load-library, ollama
;;(push "~/.roswell/lisp/quicklisp/quicklisp/" asdf:*central-registry*)

;; window opacity
(sdl2-ffi.functions:sdl-set-window-opacity (lem-sdl2/display::display-window lem-sdl2/display::*display*) (coerce 0.9 'single-float))

;; start in vi-mode
(lem-vi-mode:vi-mode)

;; load lem ollama lib
;;(lem-core::maybe-load-systems "lem-ollama")

;; set timeout for finder
(setf *find-program-timeout* 3)

;; line numbers
(lem/line-numbers:toggle-line-numbers)

;; auto save
(setf (lem:variable-value 'lem/auto-save::auto-save-checkpoint-frequency :global) 1.5)

;; don't set to 0, or the undo will fail in some case
(setf (lem:variable-value 'lem/auto-save::auto-save-key-count-threshold :global) 8)
(lem/auto-save::auto-save-mode t)

;; auto format
(setf lem:*auto-format* t)

;; line wrap
(setf (variable-value 'line-wrap :global) t)

;;(lem:add-hook lem-lisp-mode:*lisp-repl-mode-hook* 'toggle-line-wrap)

(define-key lem-vi-mode:*normal-keymap* "Space l w" 'lem-core/commands/window::toggle-line-wrap)

;; cursor color
(setf lem-vi-mode/core::*default-cursor-color* "#ffffff")

;; tab bar (the tar bar, is named "buffer" in vim actually.)
;;(lem/tabbar::toggle-tabbar)

;; For REPL mode, change the vi-mode to INSERT on startup.
(lem:add-hook lem-lisp-mode:*lisp-repl-mode-hook* 'lem-vi-mode/commands:vi-insert)

;; For SLDB mode, change the vi-mode to NORMAL.
(lem:add-hook lem-lisp-mode:*lisp-sldb-mode-hook* 'lem-vi-mode/commands:vi-normal)

;; keep lines above and below the cursor
(setf (lem-vi-mode:option-value "scrolloff") 5)

;; adjust for M-x popup window
;;(setf lem-core::*default-prompt-gravity* :center)
;;(setf lem/prompt-window::*prompt-completion-window-gravity* :horizontally-above-window)
;;(setf lem/prompt-window::*fill-width* t)

;; -- better escape -- 
;; use jk to escape vim. (use v key to switch from visual-mode to normal-mode)
(define-key lem-vi-mode:*insert-keymap* "j k" 'lem-vi-mode/commands:vi-normal)
(define-key lem-vi-mode:*ex-keymap* "j k" 'lem-vi-mode/commands:vi-normal)

(define-key lem-vi-mode:*visual-keymap* "Space j k" 'lem-vi-mode/commands:vi-normal)
(define-key lem-vi-mode:*normal-keymap* "Space j k" 'escape)

(define-key lem/prompt-window::*prompt-mode-keymap* "j k" 'escape)

(define-command completion-end () ()
  (lem/completion-mode::completion-end))
(define-key lem/completion-mode::*completion-mode-keymap* "j k" 'completion-end)

;; use C-j/C-k to select tab item.
(define-key lem/completion-mode::*completion-mode-keymap* "C-j" 'lem/completion-mode::completion-narrowing-down-or-next-line)
(define-key lem/completion-mode::*completion-mode-keymap* "C-k" 'lem/completion-mode::completion-previous-line)

;;(define-key lem/isearch::*isearch-keymap* "j k" 'lem/isearch::isearch-finish)

;; -- text object --
(define-key lem-vi-mode/binds::*inner-text-objects-keymap* "p" 'lem-vi-mode/binds::vi-inner-paren)
(define-key lem-vi-mode/binds::*outer-text-objects-keymap* "p" 'lem-vi-mode/binds::vi-a-paren)

(define-key lem-vi-mode/binds::*inner-text-objects-keymap* "s" 'mark-sexp)
(define-key lem-vi-mode/binds::*outer-text-objects-keymap* "s" 'mark-sexp)

;; -- better HJKL --
(define-key lem-vi-mode:*normal-keymap* "H" 'move-to-beginning-of-line)
(define-key lem-vi-mode:*visual-keymap* "H" 'move-to-beginning-of-line)

(define-key lem-vi-mode:*normal-keymap* "J" 'forward-paragraph)
(define-key lem-vi-mode:*visual-keymap* "J" 'forward-paragraph)

(define-key lem-vi-mode:*normal-keymap* "K" 'backward-paragraph)
(define-key lem-vi-mode:*visual-keymap* "K" 'backward-paragraph)

(define-key lem-vi-mode:*normal-keymap* "L" 'move-to-end-of-line)
(define-key lem-vi-mode:*visual-keymap* "L" 'move-to-end-of-line)

;; -- find and replace --
(define-key lem-vi-mode:*normal-keymap* "Space n h" 'lem/isearch::isearch-abort)

;; -- case switch --
(define-key lem-vi-mode:*normal-keymap* "~" 'lem-vi-mode/binds::vi-swapcase)

;; -- location --
(define-key lem-vi-mode:*normal-keymap* "M-i" 'lem-vi-mode/binds::vi-jump-next)
(define-key lem-vi-mode:*normal-keymap* "M-o" 'lem-vi-mode/binds::vi-jump-back)

;; -- buffer --
(define-key lem-vi-mode:*normal-keymap* "Space b b" 'select-buffer)
(define-key lem-vi-mode:*normal-keymap* "Space b B" 'select-buffer-next-window)
(define-key lem-vi-mode:*normal-keymap* "Space b d" 'kill-buffer)

;; -- window --
(define-key lem-vi-mode:*normal-keymap* "Space s H" 'split-active-window-horizontally)
(define-key lem-vi-mode:*normal-keymap* "Space s V" 'split-active-window-vertically)

(define-key lem-vi-mode:*normal-keymap* "C-Tab" 'switch-to-last-focused-window)

(define-key lem-vi-mode:*normal-keymap* "C-h" 'window-move-left)
(define-key lem-vi-mode:*normal-keymap* "C-j" 'window-move-down)
(define-key lem-vi-mode:*normal-keymap* "C-k" 'window-move-up)
(define-key lem-vi-mode:*normal-keymap* "C-l" 'window-move-right)

(define-key lem-vi-mode:*normal-keymap* "Space x x" 'delete-active-window)
(define-key lem-vi-mode:*normal-keymap* "Space x o" 'delete-other-windows)

;; -- tab --
(define-key lem-vi-mode:*normal-keymap* "Tab" 'lem/frame-multiplexer::frame-multiplexer-next)
(define-key lem-vi-mode:*normal-keymap* "Shift-Tab" 'lem/frame-multiplexer::frame-multiplexer-prev)

(define-key lem-vi-mode:*normal-keymap* "Space t c" 'lem/frame-multiplexer::frame-multiplexer-create-with-previous-buffer)
(define-key lem-vi-mode:*normal-keymap* "Space t d" 'lem/frame-multiplexer::frame-multiplexer-delete)

(define-key lem-vi-mode:*normal-keymap* "Space 0" 'lem/frame-multiplexer::frame-multiplexer-switch-0)
(define-key lem-vi-mode:*normal-keymap* "Space 1" 'lem/frame-multiplexer::frame-multiplexer-switch-1)
(define-key lem-vi-mode:*normal-keymap* "Space 2" 'lem/frame-multiplexer::frame-multiplexer-switch-2)
(define-key lem-vi-mode:*normal-keymap* "Space 3" 'lem/frame-multiplexer::frame-multiplexer-switch-3)
(define-key lem-vi-mode:*normal-keymap* "Space 4" 'lem/frame-multiplexer::frame-multiplexer-switch-4)
(define-key lem-vi-mode:*normal-keymap* "Space 5" 'lem/frame-multiplexer::frame-multiplexer-switch-5)
(define-key lem-vi-mode:*normal-keymap* "Space 6" 'lem/frame-multiplexer::frame-multiplexer-switch-6)
(define-key lem-vi-mode:*normal-keymap* "Space 7" 'lem/frame-multiplexer::frame-multiplexer-switch-7)
(define-key lem-vi-mode:*normal-keymap* "Space 8" 'lem/frame-multiplexer::frame-multiplexer-switch-8)
(define-key lem-vi-mode:*normal-keymap* "Space 9" 'lem/frame-multiplexer::frame-multiplexer-switch-9)

;; -- [] --
(define-key lem-vi-mode:*normal-keymap* "[ b" 'previous-buffer)
(define-key lem-vi-mode:*normal-keymap* "] b" 'next-buffer)

(define-key lem-vi-mode:*normal-keymap* "[ w" 'previous-window)
(define-key lem-vi-mode:*normal-keymap* "] w" 'next-window)

(define-key lem-vi-mode:*normal-keymap* "[ t" 'lem/frame-multiplexer::frame-multiplexer-prev)
(define-key lem-vi-mode:*normal-keymap* "] t" 'lem/frame-multiplexer::frame-multiplexer-next)

(define-key lem-vi-mode:*normal-keymap* "[ s" 'backward-sexp)
(define-key lem-vi-mode:*normal-keymap* "] s" 'forward-sexp)

(define-key lem-vi-mode:*normal-keymap* "[ l" 'up-list)
(define-key lem-vi-mode:*normal-keymap* "] l" 'down-list)

(define-key lem-vi-mode:*normal-keymap* "[ f" 'lem/language-mode::beginning-of-defun)
(define-key lem-vi-mode:*normal-keymap* "] f" 'lem/language-mode::end-of-defun)

(define-key lem-vi-mode:*normal-keymap* "[ i" 'lem-vi-mode/commands::vi-search-backward-symbol-at-point)
(define-key lem-vi-mode:*normal-keymap* "] i" 'lem-vi-mode/commands::vi-search-forward-symbol-at-point)

;; M-()
;; automatically load paredit when opening a lisp file
(defun pared-hook ()
  (lem-paredit-mode:paredit-mode t))
(add-hook lem-lisp-mode:*lisp-mode-hook* #'pared-hook)

;; -- s-exp --
(define-key lem-vi-mode:*normal-keymap* "Space s m" 'mark-sexp)
(define-key lem-vi-mode:*normal-keymap* "Space s k" 'lem-paredit-mode:paredit-kill)

(define-key lem-vi-mode:*normal-keymap* "Space s w" 'lem-paredit-mode:paredit-wrap-round)
(define-key lem-vi-mode:*normal-keymap* "Space s W" 'lem-paredit-mode:paredit-splice)

(define-key lem-vi-mode:*normal-keymap* "Space s b" 'lem-paredit-mode:paredit-barf)
(define-key lem-vi-mode:*normal-keymap* "Space s s" 'lem-paredit-mode:paredit-slurp)

(define-key lem-vi-mode:*normal-keymap* "Space s t" 'transpose-sexps)
(define-key lem-vi-mode:*normal-keymap* "Space s r" 'lem-paredit-mode:paredit-raise)

;; -- repl --
(define-command slime-start () ()
  (lem-lisp-mode:run-slime "ros dynamic-space-size=4GiB run"))
(define-key lem-vi-mode:*normal-keymap* "Space r R" 'slime-start)
(define-key lem-vi-mode:*normal-keymap* "Space r r" 'lem-lisp-mode/internal::slime-restart)

(define-key lem-vi-mode:*normal-keymap* "Space r l" 'lem-lisp-mode/connection-list::lisp-connection-list)

(define-key lem-vi-mode:*normal-keymap* "Space r c" 'lem-lisp-mode/internal::lisp-repl-shortcut)

;; -- evaluate --
(define-key lem-vi-mode:*normal-keymap* "Space e e" 'lem-lisp-mode/internal::lisp-eval-expression-in-repl)
(define-key lem-vi-mode:*normal-keymap* "Space e d" 'lem-lisp-mode/eval::lisp-eval-defun)
(define-key lem-vi-mode:*visual-keymap* "Space e r" 'lem-lisp-mode/eval::lisp-eval-region)
(define-key lem-vi-mode:*normal-keymap* "Space e b" 'lem-lisp-mode/eval::lisp-eval-buffer)
(define-key lem-vi-mode:*normal-keymap* "Space e s" 'lem-lisp-mode/eval::lisp-eval-string)
(define-key lem-vi-mode:*normal-keymap* "Space e p" 'lem-lisp-mode/internal::lisp-listen-in-current-package)

(define-key lem-vi-mode:*normal-keymap* "Space e C" 'lem-lisp-mode/eval::lisp-eval-clear)
(define-key lem-vi-mode:*normal-keymap* "Space e P" 'lem-lisp-mode/eval::lisp-eval-at-point)
(define-key lem-vi-mode:*normal-keymap* "Space e t" 'lem-lisp-mode/test-runner::lisp-test-runner-run-buffer)
(define-key lem-vi-mode:*normal-keymap* "Space e T" 'lem-lisp-mode/test-runner::lisp-test-runner-run-current)

;; fixme: start slime* if no repl connection.
(define-key lem-vi-mode:*normal-keymap* "Space e q" 'lem-lisp-mode/internal::lisp-quickload)

(define-key lem-vi-mode:*normal-keymap* "Space e w" 'lem-lisp-mode/internal::lisp-switch-to-repl-buffer)
(define-key lem-vi-mode:*normal-keymap* "Space e c" 'lem-lisp-mode/internal::lisp-repl-copy-down)

(define-key lem-lisp-mode/internal:*lisp-repl-mode-keymap* "M-r" 'lem/listener-mode::listener-isearch-history)
(define-key lem-lisp-mode/internal:*lisp-repl-mode-keymap* "M-c" 'lem/listener-mode::listener-clear-input)
(define-key lem-lisp-mode/internal:*lisp-repl-mode-keymap* "M-C" 'lem/listener-mode::listener-clear-buffer)

(define-key lem-lisp-mode/internal:*lisp-mode-keymap* "M-j" 'delete-indentation)

;; -- inspector --
;; q -> quit
;; l -> left
;; h -> history
;; enter -> fetch
;; p -> part 
;; e -> eval
;; d -> describe
;; v -> verbose
;; r -> re-inspect
(define-key lem-lisp-mode/inspector::*lisp-inspector-keymap* "r" 'lem-lisp-mode/inspector::lisp-inspector-reinspect)

(define-key lem-vi-mode:*normal-keymap* "Space i i" 'lem-lisp-mode/inspector::lisp-inspect)
(define-key lem-vi-mode:*normal-keymap* "Space i c" 'lem-lisp-mode/class-browser::lisp-browse-class-as-tree)

;; -- comment --
(define-key lem-vi-mode:*visual-keymap* "Space c" 'lem/language-mode::comment-or-uncomment-region)
(define-key lem-vi-mode:*normal-keymap* "Space c" 'lem/language-mode::comment-or-uncomment-region)

;; -- zen --
(define-key lem-vi-mode:*normal-keymap* "Space z" 'toggle-frame-fullscreen)

;; -- goto --
(define-key lem-vi-mode:*normal-keymap* "Space a" 'execute-command)
(define-key lem-vi-mode:*normal-keymap* "g a" 'execute-command)

(define-key lem-vi-mode:*normal-keymap* "g f" 'lem-core/commands/project:project-find-file)

(define-key lem-vi-mode:*normal-keymap* "g d" 'lem-lisp-mode/internal::find-definitions)
(define-key lem-vi-mode:*normal-keymap* "g r" 'lem-lisp-mode/internal::find-references)

(define-key lem-vi-mode:*normal-keymap* "g m" 'lem-vi-mode/binds::vi-move-to-matching-item)
(define-key lem-vi-mode:*normal-keymap* "g T" 'lem/grep::project-grep)

(define-key lem-vi-mode:*normal-keymap* "g n" 'lem/filer::filer)

;; -- trace --
;; The trace should be read in repl message buffer.
(define-key lem-vi-mode:*normal-keymap* "Space t t" 'lem-lisp-mode/trace::lisp-toggle-trace)
(define-key lem-vi-mode:*normal-keymap* "Space t T" 'lem-lisp-mode/trace::lisp-trace-list)

;; -- describe --
(define-key lem-vi-mode:*normal-keymap* "Space d d" 'lem-lisp-mode/internal::lisp-apropos)
(define-key lem-vi-mode:*normal-keymap* "Space d a" 'lem-lisp-mode/internal::lisp-apropos-all)
(define-key lem-vi-mode:*normal-keymap* "Space d p" 'lem-lisp-mode/internal::lisp-apropos-package)
(define-key lem-vi-mode:*normal-keymap* "Space d s" 'lem-lisp-mode/internal::lisp-describe-symbol)
(define-key lem-vi-mode:*normal-keymap* "Space d c" 'apropos-command)

(define-key lem-vi-mode:*normal-keymap* "Space d k" 'describe-key)
(define-key lem-vi-mode:*normal-keymap* "Space d b" 'describe-bindings)
(define-key lem-vi-mode:*normal-keymap* "Space d m" 'list-modes)
(define-key lem-vi-mode:*normal-keymap* "Space d D" 'documentation-describe-bindings)
;; use M-a to autodoc

;; -- file --
(define-key lem-vi-mode:*normal-keymap* "Space f t" 'lem/filer::filer)
(define-key lem-vi-mode:*normal-keymap* "Space o f" 'lem-core/commands/project:project-find-file)
(define-key lem-vi-mode:*normal-keymap* "Space r f" 'lem-core/commands/file:find-history-file)

(define-key lem-vi-mode:*normal-keymap* "Space f s" 'lem-core/commands/file:save-current-buffer)
(define-key lem-vi-mode:*normal-keymap* "Space f w" 'lem-core/commands/file:write-file)
(define-key lem-vi-mode:*normal-keymap* "Space f c" 'lem-core/commands/file:format-current-buffer)

;; -- project --
(define-key lem-vi-mode:*normal-keymap* "Space o p" 'lem-core/commands/project:project-switch)
(define-key lem-vi-mode:*normal-keymap* "Space p r" 'lem-core/commands/project:project-root-directory)

;; -- dashboard --
(in-package :lem-dashboard)
(set-dashboard (list (make-instance 'dashboard-splash
                                    :item-attribute 'document-metadata-attribute
                                    :splash-texts '("
An idiot admires complexity, a genius admires simplicity.
                                                            ― Terry Davis")
                                    :top-margin 4
                                    :bottom-margin 0)))

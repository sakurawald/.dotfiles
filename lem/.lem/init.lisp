;; WARNNING: This configuration file may contains some options that requires the latest build of lem editor in github.
;; NOTE: This configuration file is made for `lem sdl2 version`. You need to remove the `sdl2 frontend specific configurations` if you are using other frontend.
;; NOTE: I steal some good ideas from "https://neovim.io/" and "https://www.gnu.org/software/emacs/". (Interesting to read the manual)
(in-package :lem-user)

;; -- quicklisp --
;;(push "~/.roswell/lisp/quicklisp/quicklisp/" asdf:*central-registry*)
;;(push "~/.roswell/lisp/quicklisp/local-projects/" ql:*local-project-directories*)

;; -- appearence --
(lem-if:set-font-size (implementation) 25)

(sdl2-ffi.functions:sdl-set-window-opacity (lem-sdl2/display::display-window lem-sdl2/display::*display*) (coerce 0.95 'single-float))
(lem-core/commands/frame::maximize-frame)

;; TIP: To tweak the color, use `M-x color-preview`.
(setf lem-vi-mode/core::*default-cursor-color* "#ffffff")
(setf (lem:variable-value 'lem-core::highlight-line-color :global) "#000066")

(lem-base16-themes::define-base16-color-theme "sakurawald"
  :base00 "#131314" ;; default background
  :base01 "#393939" ;; status bar, line numbers and folding marks
  :base02 "#515151" ;; selection
  :base03 "#777777" ;; comment
  :base04 "#b4b7b4"
  :base05 "#cccccc"
  :base06 "#ff00ff" ;; repl value
  :base07 "#ffffff" ;; line numbers
  :base08 "#ff7f7b" ;; sldb condition
  :base09 "#ffbf70" ;; sldb restart
  :base0a "#fbf305" ;; multiplexier
  :base0b "#54b33e" ;; string
  :base0c "#54fcfc" ;; keyword symbol
  :base0d "#4a4ffc" ;; function name
  :base0e "#fc54fc" ;; operator
  :base0f "#ed864a")

(lem-core:load-theme "sakurawald")

;; -- vi mode --
(lem-vi-mode:vi-mode)
(lem:add-hook lem-lisp-mode:*lisp-repl-mode-hook* 'lem-vi-mode/commands:vi-insert)
(lem:add-hook lem-lisp-mode:*lisp-sldb-mode-hook* 'lem-vi-mode/commands:vi-normal)

(setf (lem-vi-mode:option-value "scrolloff") 5)

;; -- line numbers --
(lem/line-numbers:toggle-line-numbers)

;; -- auto save --
(setf (lem:variable-value 'lem/auto-save::auto-save-checkpoint-frequency :global) 1.5)
;; NOTE: Don't set the key count threshold, or it will conflict with the auto-save, causing it unable to undo.
(setf (lem:variable-value 'lem/auto-save::auto-save-key-count-threshold :global) 8)
(lem/auto-save::auto-save-mode t)

;; -- formatter --
;; TIP: Use `formatter` instead of `<<` and `>>`.
(setf lem:*auto-format* t)

(define-key lem-lisp-mode/internal:*lisp-mode-keymap* "M-j" 'delete-indentation)

;; -- line wrap --
(setf (variable-value 'line-wrap :global) t)
(define-key lem-vi-mode:*normal-keymap* "Space l w" 'lem-core/commands/window::toggle-line-wrap)

;; -- better escape -- 
;; TIP: Lem does follow the "key-conversion", like: "C-[" = "Escape", "C-i" = "Tab" and "C-m" = "Return".
;; TIP: Use jk or C-g to vi-keyboard-quit in vi-mode. (use v key to switch from visual-mode to normal-mode)
;; TIP: The order: C-g > C-[ > Escape
(define-key lem-vi-mode:*insert-keymap* "j k" 'lem-vi-mode/commands:vi-normal)
(define-key lem-vi-mode:*ex-keymap* "j k" 'lem-vi-mode/commands:vi-normal)

(define-key lem-vi-mode:*visual-keymap* "Space j k" 'lem-vi-mode/commands:vi-normal)
(define-key lem-vi-mode:*normal-keymap* "Space j k" 'escape)

(define-key lem/prompt-window::*prompt-mode-keymap* "j k" 'escape)

(define-command completion-end () ()
  (lem/completion-mode::completion-end))
(define-key lem/completion-mode::*completion-mode-keymap* "j k" 'completion-end)

;; -- prompt window--
;;(setf lem-core::*default-prompt-gravity* :bottom-display)
;;(setf lem/prompt-window::*prompt-completion-window-gravity* :horizontally-above-window)
;;(setf lem/prompt-window::*fill-width* t)

;; TIP: After enable the `show completion instantly`, you need to double press the `enter` key to confirm the selection.
(add-hook *prompt-after-activate-hook*
          (lambda ()
            (call-command 'lem/prompt-window::prompt-completion nil)))

(add-hook *prompt-deactivate-hook*
          (lambda ()
            (lem/completion-mode:completion-end)))

;; -- completion --
;; Use tab or C-p in insert-mode to trigger completion window
(define-key lem/completion-mode::*completion-mode-keymap* "C-j" 'lem/completion-mode::completion-narrowing-down-or-next-line)
(define-key lem/completion-mode::*completion-mode-keymap* "C-k" 'lem/completion-mode::completion-previous-line)

;; -- text object --
;; NOTE: In `vim`, the `iskeyword` table is associated with `file-type`. 
;; NOTE: You may ask where is the `sexp text-object`, well, it's defined as a `word`. To select a `sexp` is to select a `word` in lisp-mode. (The `iskeyword` is modified in lisp-mode)
;; NOTE: The `word-text-object` for `lisp-mode` excludes the following chars: `/`, `.`, `:` and `-`
(define-key lem-vi-mode/binds::*inner-text-objects-keymap* "p" 'lem-vi-mode/binds::vi-inner-paren)
(define-key lem-vi-mode/binds::*outer-text-objects-keymap* "p" 'lem-vi-mode/binds::vi-a-paren)

;; -- better HJKL --
;; TIP: use `zz` to center current line.
;; TIP: use `M` to center current window.
(define-key lem-vi-mode:*normal-keymap* "H" 'lem-vi-mode/binds::move-to-beginning-of-line)
(define-key lem-vi-mode:*visual-keymap* "H" 'lem-vi-mode/binds::move-to-beginning-of-line)

(define-key lem-vi-mode:*normal-keymap* "J" 'forward-paragraph)
(define-key lem-vi-mode:*visual-keymap* "J" 'forward-paragraph)

(define-key lem-vi-mode:*normal-keymap* "K" 'backward-paragraph)
(define-key lem-vi-mode:*visual-keymap* "K" 'backward-paragraph)

(define-key lem-vi-mode:*normal-keymap* "L" 'lem-vi-mode/binds::vi-move-to-end-of-line)
(define-key lem-vi-mode:*visual-keymap* "L" 'lem-vi-mode/binds::vi-move-to-end-of-line)

;; -- find and replace --
;; TIP: The `query-replace` can be used in `grep` window.
;; TIP: The `grep` window is edit-able. Use `M-o` to go to the other window.
;; TIP: Press `*` to search forward symbol at point.
(setf *find-program-timeout* 3)
(setf (lem-vi-mode:option-value "ignorecase") t)

(define-command isearch-end () ()
  (lem/isearch::isearch-end))
(define-key lem-vi-mode:*normal-keymap* "Space n h" 'isearch-end)

(setf lem/grep:*grep-args* "-niHI")
(setf lem/grep::*last-query* "git grep -niHI ")
  
;; -- location --
;; TIP: The `C-i` and `C-o` will jump in a jumplist, while the `''` will jump only between 2 entries.
;; NOTE: The `C-i` is equal to `Tab` key, if you bind the `Tab` key to a command, then `C-i` binding will not work.
(define-key lem-vi-mode:*normal-keymap* "C-Tab" 'lem/frame-multiplexer::frame-multiplexer-next)
(define-key lem-vi-mode:*normal-keymap* "C-Shift-Tab" 'lem/frame-multiplexer::frame-multiplexer-prev)

(define-key lem-vi-mode:*normal-keymap* "C-i" 'lem-vi-mode/binds::vi-jump-next)
(define-key lem-vi-mode:*normal-keymap* "C-o" 'lem-vi-mode/binds::vi-jump-back)

;; -- buffer --
(define-key lem-vi-mode:*normal-keymap* "Space b b" 'select-buffer)
(define-key lem-vi-mode:*normal-keymap* "Space b p" 'previous-buffer)
(define-key lem-vi-mode:*normal-keymap* "Space b n" 'next-buffer)
(define-key lem-vi-mode:*normal-keymap* "Space b B" 'select-buffer-next-window)
(define-key lem-vi-mode:*normal-keymap* "Space b d" 'kill-buffer)

;; -- window --
(define-key lem-vi-mode:*normal-keymap* "Space s h" 'split-active-window-horizontally)
(define-key lem-vi-mode:*normal-keymap* "Space s v" 'split-active-window-vertically)

(define-key lem-vi-mode:*normal-keymap* "Space w n" 'next-window)
(define-key lem-vi-mode:*normal-keymap* "Space w p" 'previous-window)

(define-key lem-vi-mode:*normal-keymap* "C-p" 'switch-to-last-focused-window)

(define-key lem-vi-mode:*normal-keymap* "C-h" 'window-move-left)
(define-key lem-vi-mode:*normal-keymap* "C-j" 'window-move-down)
(define-key lem-vi-mode:*normal-keymap* "C-k" 'window-move-up)
(define-key lem-vi-mode:*normal-keymap* "C-l" 'window-move-right)

(define-key lem-vi-mode:*normal-keymap* "Space x x" 'delete-active-window)
(define-key lem-vi-mode:*normal-keymap* "Space x o" 'delete-other-windows)

;; -- tab --
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
;; NOTE: the key-binding conflicting with paredit-mode. see https://github.com/lem-project/lem/issues/1611
(define-key lem-vi-mode:*normal-keymap* "[ s" 'backward-sexp)
(define-key lem-vi-mode:*normal-keymap* "] s" 'forward-sexp)

(define-key lem-vi-mode:*normal-keymap* "[ l" 'up-list)
(define-key lem-vi-mode:*normal-keymap* "] l" 'down-list)

(define-key lem-vi-mode:*normal-keymap* "[ f" 'lem/language-mode::beginning-of-defun)
(define-key lem-vi-mode:*normal-keymap* "] f" 'lem/language-mode::end-of-defun)

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
(define-command slime* () ()
  (lem-lisp-mode:run-slime "ros dynamic-space-size=4GiB run"))
(define-key lem-vi-mode:*normal-keymap* "Space r R" 'slime*)
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
(define-key lem-vi-mode:*normal-keymap* "Space e I" 'lem-lisp-mode/internal::lisp-interrupt)

(define-key lem-vi-mode:*normal-keymap* "Space e q" 'lem-lisp-mode/internal::lisp-quickload)

(define-key lem-vi-mode:*normal-keymap* "Space e w" 'lem-lisp-mode/internal::lisp-switch-to-repl-buffer)
(define-key lem-vi-mode:*normal-keymap* "Space e c" 'lem-lisp-mode/internal::lisp-repl-copy-down) ;; M-Ret

(define-key lem-lisp-mode/internal:*lisp-repl-mode-keymap* "M-r" 'lem/listener-mode::listener-isearch-history)
(define-key lem-lisp-mode/internal:*lisp-repl-mode-keymap* "M-c" 'lem/listener-mode::listener-clear-input)
(define-key lem-lisp-mode/internal:*lisp-repl-mode-keymap* "M-C" 'lem/listener-mode::listener-clear-buffer)

;; -- inspector --
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

(define-key lem-lisp-mode/inspector::*lisp-inspector-keymap* "r" 'lem-lisp-mode/inspector::lisp-inspector-reinspect)

;; TIP: if there is no symbol under cursor, then the command will ask for a form to inspect.
(define-key lem-vi-mode:*normal-keymap* "Space i i" 'lem-lisp-mode/inspector::lisp-inspect)
(define-key lem-vi-mode:*normal-keymap* "Space i c" 'lem-lisp-mode/class-browser::lisp-browse-class-as-tree)

;; -- sldb --
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

;; -- comment --
(define-key lem-vi-mode:*visual-keymap* "Space c" 'lem/language-mode::comment-or-uncomment-region)
(define-key lem-vi-mode:*normal-keymap* "Space c" 'lem/language-mode::comment-or-uncomment-region)

;; -- zen --
(define-key lem-vi-mode:*normal-keymap* "Space z" 'toggle-frame-fullscreen)

;; -- goto --
;; gj / gk -> logical line
;; ge / GE -> backward word end / backward broad word end
;; GJ -> join line
;; Gu / GU -> downcase / upcase
(define-key lem-vi-mode:*normal-keymap* "Space Space" 'execute-command)
(define-key lem-vi-mode:*normal-keymap* "Space a" 'execute-command)
(define-key lem-vi-mode:*normal-keymap* "g a" 'execute-command)

(define-key lem-vi-mode:*normal-keymap* "g c" 'recenter)

;; NOTE To set the `sbcl-source` dir, write `(sb-ext:set-sbcl-source-location "~/.roswell/src/sbcl-2.4.10/")` in `~/.roswell/init.lisp` file.
;; TIP: use `M-,` to pop definition stack, and use `M-.` vice verse.
(define-key lem-vi-mode:*normal-keymap* "g d" 'lem/language-mode::find-definitions)
(define-key lem-vi-mode:*normal-keymap* "g r" 'lem/language-mode::find-references)
(define-key lem-vi-mode:*normal-keymap* "g s" 'lem-lisp-mode/internal::lisp-search-symbol)
(define-key lem-vi-mode:*normal-keymap* "g f" 'lem/language-mode::beginning-of-defun)

(define-key lem-vi-mode:*normal-keymap* "g l" 'lem/detective:detective-all)

(define-key lem-vi-mode:*normal-keymap* "g m" 'lem-vi-mode/binds::vi-move-to-matching-item)
;; TIP: Then use `M-x query-replace` in result window
(define-key lem-vi-mode:*normal-keymap* "g t" 'lem/grep::project-grep)

(define-key lem-vi-mode:*normal-keymap* "g n" 'lem/filer::filer)
(define-key lem-vi-mode:*normal-keymap* "g N" 'lem/filer::filer-directory)

;; -- trace --
;; TIP: The output of `trace` can be read in lisp repl buffer.
(define-key lem-vi-mode:*normal-keymap* "Space t t" 'lem-lisp-mode/trace::lisp-toggle-trace)
(define-key lem-vi-mode:*normal-keymap* "Space t T" 'lem-lisp-mode/trace::lisp-trace-list)

;; -- describe --
(define-key lem-vi-mode:*normal-keymap* "Space d d" 'lem-lisp-mode/internal::lisp-apropos)
(define-key lem-vi-mode:*normal-keymap* "Space d c" 'apropos-command)
(define-key lem-vi-mode:*normal-keymap* "Space d a" 'lem-lisp-mode/internal::lisp-apropos-all)
(define-key lem-vi-mode:*normal-keymap* "Space d p" 'lem-lisp-mode/internal::lisp-apropos-package)
(define-key lem-vi-mode:*normal-keymap* "Space d s" 'lem-lisp-mode/internal::lisp-describe-symbol)
(define-key lem-vi-mode:*normal-keymap* "Space d S" 'lem-lisp-mode/internal::lisp-search-symbol)
(define-key lem-vi-mode:*normal-keymap* "Space d h" 'lem-lisp-mode/hyperspec::hyperspec-at-point)
(define-key lem-vi-mode:*normal-keymap* "Space d H" 'lem-lisp-mode/hyperspec::hyperspec-lookup)

(define-key lem-vi-mode:*normal-keymap* "Space d k" 'describe-key)
(define-key lem-vi-mode:*normal-keymap* "Space d b" 'describe-bindings)
(define-key lem-vi-mode:*normal-keymap* "Space d m" 'list-modes)
(define-key lem-vi-mode:*normal-keymap* "Space d D" 'documentation-describe-bindings)
;; TIP: Use M-a to autodoc

;; -- file --
(define-key lem-vi-mode:*normal-keymap* "Space f t" 'lem/filer::filer)
(define-key lem-vi-mode:*normal-keymap* "Space f i" 'lem/filer::filer-directory)
(define-key lem-vi-mode:*normal-keymap* "Space f a" 'lem/filer::filer-at-directory)

(define-key lem-vi-mode:*normal-keymap* "Space o f" 'lem-core/commands/project:project-find-file)
(define-key lem-vi-mode:*normal-keymap* "Space o F" 'find-file-next-window)
(define-key lem-vi-mode:*normal-keymap* "Space r f" 'lem-core/commands/file:find-history-file)

(define-key lem-vi-mode:*normal-keymap* "Space o s" 'lisp-scratch)

(define-key lem-vi-mode:*normal-keymap* "Space o d" 'lem/directory-mode::find-file-directory)
(define-key lem-vi-mode:*normal-keymap* "Space p d" 'lem-core/commands/project::project-root-directory)
(define-key lem/directory-mode::*directory-mode-keymap* "C-h" 'lem/directory-mode::directory-mode-up-directory)
(define-key lem/directory-mode::*directory-mode-keymap* "C-l" 'lem/directory-mode::directory-mode-find-file)
(define-key lem/directory-mode::*directory-mode-keymap* "C-j" 'lem/directory-mode::directory-mode-next-line)
(define-key lem/directory-mode::*directory-mode-keymap* "C-k" 'lem/directory-mode::directory-mode-previous-line)

;; TIP: Use automatic save, don't save file manually.
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

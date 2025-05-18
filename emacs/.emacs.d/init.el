(defun <links> () "The interesting links.")
;; Emacs resources:
;; - https://emacsdocs.org/docs/emacs/The-Emacs-Editor
;; - https://magit.vc/
;; - https://www.emacswiki.org/emacs/SiteMap
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/index.html
;; - https://www.gnu.org/software/emacs/
;; - https://www.gnu.org/fun/
;; - https://emacsconf.org/
;; - https://sourceware.org/
;; - https://docs.doomemacs.org/
;; - https://godbolt.org/ (Compiler Explorer)
;; - https://emacsredux.com/archive/
;; - https://melpa.org/#/
;; - https://alexschroeder.ch/geocities/kensanata/emacs-defense.html
;; - https://github.com/emacs-tw/awesome-elisp
;; - https://github.com/emacs-tw/awesome-emacs
;; - https://www.gutenberg.org/about/
;;
;; Blogs:
;; - https://nullprogram.com/index/
;;
;; Lisp resources:
;; - https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node1.html
;; - http://www.sbcl.org/sbcl-internals/
;; - https://funcall.blogspot.com/
;; - http://www.sbcl.org/manual/index.html
;;
;; Configuration resources:
;; - https://github.com/caisah/emacs.dz
;; - https://github.com/sergeyklay/.emacs.d
;; - https://github.com/hrs/dotfiles
;; - https://github.com/protesilaos/dotfiles
;;
;; Misc:
;; https://keepachangelog.com/zh-TW/1.1.0/
;;
;; Some interesting sentences:
;; - While any text editor can save your files, only Emacs can save your soul.
;; - While Vim is an extensible editor, the Emacs is an extended editor.
;; - While Vim is a text editor, the Emacs has a text editor.
;; - Emacs is a Lisp-machine (environment) that supports Elisp as one of its languages.
;; - A nice Vim macro a day, keeps the VS Code away.
;; - An idiot admires complexity, a genius admires simplicity. -- Terry Davis
;; - Finally, There are only 2 great languages: C and Lisp.
;; - To learn, is to connect.
;; - Patterns mean "I have run out of language." -- Rich Hickey
;; - Design patterns is a compromise to the lack of expressiveness of the language (evaluator).
;; - Premature optimization is the root of all evil. -- Donald Knuth (https://wiki.c2.com/?PrematureOptimization)
;; - If it works, don't touch it.
;; - Object is a lie, use function instead.
;; - Function as the abstract machine.
;; - Declarative language is like intention language.
;; - The only difficulty is the lack of information (bits).
;; - I hope symobls from Emacs packages all have a good name, with intuitive prefix and suffix.
;; - Programming = Modeling + Translating
;; - Notation is nothing without denotation.
;; - Learning Emacs is painful in the beginning, and painful in the end.
;; - Deprecated means stable.
;; - A fancy GUI application usually has less features (configurations).
;; - A text editor is a video game.
;; - Programs must be written for people to read, and only incidentally for machines to execute. -- Harold Abelson (SICP)
;; - I want a map if i am in forest.
;; - Garbage in, garbage out.
;; - Noise or book, that's a question.
;; - Entertainment is not teaching.
;; - To learn a language is to use it.
;; - Some text are just hard to read, that's the problem of the author.
;; - Composition over inheritance. (We love atom/primitives)
;; - Grep is powerful, since it works in string level (escaped from the semantics), and always works.
;; - Comment is one of the most important meta-data for a document.
;; - 95% of tech problems can be solved via RTFM and STFW.
;; - Function as a black box.
;; - You can learn Emacs everyday.
;; - Operating systems, GUI toolkits and competing editors come and go, but Emacs is forever!
;; - Figure out the problems solves half of the problem.
;; - The most useful part of a function is its name.
;; - When in doubt, try brute-force.
;; - No reality, only interpretation.
;; - Emacs as a general solution compared to specialized programs.
;; - Only do optimizatoin after a profiler report.
;; - Read log before debug.
;; - Information Quality: text (book > article) > image > audio > video.
;; - It will merely be more complicated to learn two things at the same time.
;; - Document the use-case of the function, not what the function it does.
;; - Websites that block Tor network are not worth reading.
;; - Emacs does a good abstration for applications.
;; - Vim is a keymap, not a text editor.
;; - Find the source.
;; - Words for separation.
;; - A good text should increase the understanding of the reader.

;; TODO bookmark to add known path: github, script, .config ... (with dired command)

;; TODO explore tree-sitter, try to write own parser.

;; TODO grep (saerch and replace) on file/project scope.

;; TODO let helm support multi pattern for all commands. (gs command)

;; TODO sometimes buffers opened by lsp-java mode out of sync.

;; TODO combine evil-mode with bm.

;; TODO refactor for cpp and java -> https://github.com/thoni56/c-xrefactory

;; NOTE Emacs 30.1 build 2 is very fast. (Build it yourself)
;; NOTE Features provided by Jetbrains: https://www.jetbrains.com/idea/features/
;; NOTE To operate on an object, using the CRUD name-conversion: 'create', 'read', 'update', 'delete'.
;; NOTE The default 'prefix-keymap': https://www.gnu.org/software/emacs/manual/html_node/emacs/Prefix-Keymaps.html
;; NOTE It's also okay to steal some ideas from others' dotfiles.
;; TIP Basically, you need a good text-editor and a good compiler to work on a project. And a keymap-machine to define a key-macro to run a script (like `magit' package).
;; TIP Reduce the following inputs, to stay in the home row: `F1-F12', `Caps_Lock', `Escape', `Tab', `Return', `Backspace', `ArrowKeys', `NumberKeys', `MouseInput'.
;; TIP All the modifier keys are your friend: `Ctrl', `Shift', `Meta', `Super'. (Treat modifier keys as the decorator/combinator to alphabet keys)
;; TIP What I learned from Vim is to remap `CapsLock' into `Ctrl'. CapsLock is useless, since it's not a modifier-key, and you can replace it with Shift+{a-z}. (https://emacsredux.com/blog/2017/12/31/a-crazy-productivity-boost-remapping-return-to-control-2017-edition/)
;; TIP Get some good ideas from https://github.com/t3chnoboy/awesome-awesome-awesome

;; [My Computing Software]
;; Terminal (Alacritty + Tmux): Interact with the OS.
;; Emacs: As application platform.
;; Browser (Tor Browser + Firefox): As application platform.
;; Jetbrains IDEs: Very useful for some languages.
;; Image Editor: gimp
;; Wireshark: Traffic analyzer.
;; VLC: media player.
;; OBS Studio: media recording.
;; KeePassXC: password manager.
;; Spectacle: screen-shot.

;; [My Computing Hardware]
;; Keyboard: HHKB (The bluetooth connection is slow, use the wired connection)
;; Mouse: GPro (The wireless connection is stable and fast)
;; Monitor: at least 4K 120Hz

(defun <top-level> () "Top-level init form.")
;; Measure the current start up time.
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "Emacs ready in %s with %d garbage collections."
            (format "%.2f seconds"
                    (float-time
                     (time-subtract after-init-time before-init-time)))
            gcs-done)))

;; NOTE `defun' is not a special-form, read the `info' packge for `special-form'.

(defun <package> () "Emacs package manage.")
(defun --->package-manager () "Add melpa-repo into the package.el.")

;; Load `package'
(require 'package)
;; Set package archive.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (setq package-archive-priorities
;;       '(("gnu"      . 100)
;;         ("nongnu"   . 50)
;;         ("melpa"    . 10)))

;; Initialize packages.
;; (setq package-quickstart t)
(package-initialize)
;; (package-refresh-contents t)

;; NOTE A built-in package doesn't care the value of `use-package-always-ensure'.
;; https://emacsredux.com/blog/2025/01/12/ensure-all-packages-are-installed-by-default-with-use-package/
;; (setq use-package-always-ensure t)

;; NOTE Use `use-package' package for its defer loading.
;; (require 'use-package)
;; NOTE To bypass the package requirement restriction, use `package-vc-install'. (Or just upgrade the Emacs version)

;; NOTE The `benchmark-init' package gives the wrong sample time.
;; NOTE A shorten startup-time doesn't means a better user-experience. (You will get stutter when loading packages while using commands.)
;; TIP Enable use-package statistics, to sample the loading time of all packages. (Use `use-package-report' command to read it.)
(setq use-package-compute-statistics t)

(use-package package
  :config
  ;; TIP Emacs 30.1 is shipped with native-compiler and native-json processing.
  ;; TIP See https://zenodo.org/records/3736363
  ;; NOTE Performance comparing: https://www.gnu.org/software/emacs/manual/html_node/elisp/Native-Compilation.html
  ;; Native compile a package when installing it.
  (setq package-native-compile t)
  )

(use-package emacs
  :ensure nil
  :config
  ;; TIP Use (native-compile-directory package-user-dir) to compile the installed packages.
  (setq native-comp-async-jobs-number 8)
  ;; (setq native-comp-bootstrap-deny-list '("dap.*"))
  )

;;(require 'benchmark-init)
;; To disable collection of benchmark data after init is done.
;;(add-hook 'after-init-hook 'benchmark-init/deactivate)

(defun --->vim-emulator () "Vim emulator.")
(use-package evil
  :ensure t
  :custom
  ;; keymap: integrate with evil-collection
  (evil-want-integration t) ;; This is optional since it's already set to t by default.
  (evil-want-keybinding nil)

  ;; keymap: vanilla vim
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  (evil-undo-system 'undo-redo)
  (evil-symbol-word-search t)

  ;; scroll
  (scroll-margin 5)

  ;; search
  (evil-flash-delay 5)

  ;; macro
  ;;(setq evil-kbd-macro-suppress-motion-error t)

  ;; Don't display the state in 'echo-area', it will conflicts with the 'slime-quickdoc'.
  (evil-echo-state nil)

  :config
  ;; NOTE The manual of vim: https://neovim.io/
  ;; NOTE The vim golf makes the text-editing interesting: https://www.vimgolf.com/
  ;; NOTE Define keys for evil-mode: https://evil.readthedocs.io/en/latest/keymaps.html#evil-define-key
  ;; TIP Use 'C-z' to toggle between 'vi-mode' and 'emacs-mode'.
  ;; TIP To edit 'similar-text', use 'vi-visual-block-mode', 'vim-macro' or 'vim-repeat-command'.

  ;; NOTE The kill-ring in Emacs can be replaced by evil registers. (We don't want to use the Emacs registers, to see it use `list-registers' command)
  ;; TIP To list registers ':reg'. Useful registers: 0 (last yank), " (unnamed register), * (x11 clipboard).
  ;; TIP To access a register, use '"<register-name>{py}'.

  ;; TIP The 'v', 'c', 'y', 'd' are all 'vi-operator'.
  ;; TIP The 'w', 'e' and 'b' itself is also a 'motion-command'.
  ;; TIP Useful single-key command: 'C', 'D' and 'S' = 'cc'.

  ;; TIP Use 'C-o' to use one command in 'vi-normal-state' and re-enter 'vi-insert-state'.
  ;; TIP Use 'C-r' in 'vi-insert-state' to paste content from a register.

  ;; TIP Use `f/F/t/T' to find/till char in current line, and `;' and `,' to repeat.
  ;; TIP Use `Tab' and `Shift-Tab' to jump to next/previous `semantic-unit' (token).

  ;; TIP Useful combination of vi-commands: `v{w/W}', `v{b/B}', `v{e/E}'.
  ;; TIP For `v{i/o}{text-object}', the `i' means `inner', and the `o' means `outer'.

  ;; Navigate mini-buffer history entries.
  (evil-define-key '(normal insert visual) minibuffer-mode-map (kbd "C-j") 'next-history-element)
  (evil-define-key '(normal insert visual) minibuffer-mode-map (kbd "C-k") 'previous-history-element)

  ;; Deletion in insert-state.
  (define-key evil-insert-state-map (kbd "C-x C-n") nil)
  (define-key evil-insert-state-map (kbd "C-x C-p") nil)
  (define-key evil-insert-state-map (kbd "C-x") 'evil-delete-backward-char-and-join)

  ;; Yank and kill-ring.
  (define-key evil-normal-state-map (kbd "C-p") 'helm-show-kill-ring)
  (define-key evil-insert-state-map (kbd "C-p") 'helm-show-kill-ring)

  ;; Disable the evil digraph mode. (See more in evil-ex-show-diagraphs command.)
  (define-key evil-insert-state-map (kbd "C-k") nil)

  ;; TIP Use `C-c' prefix to be compatible with the `vterm' package.
  (evil-set-toggle-key "C-c z")

  ;; TIP undefine `C-z' to avoid mis-typed. (https://emacsredux.com/blog/2023/03/14/avoid-accidentally-minimizing-emacs/)
  (define-key global-map (kbd "C-z") nil)
  (define-key global-map (kbd "M-z") nil)

  ;; Enable evil-mode.
  (evil-mode 1))

(use-package evil-visualstar
  :ensure t
  :config
  ;; TIP Press `*' or `#' key in visual-state to start a search.
  (global-evil-visualstar-mode))

(use-package evil-collection
  :ensure t
  :after (evil)
  :custom
  ;; TIP The 'evil-collection' provides 'possible' and `sensible' bindings for all `evil-mode'.

  ;; The default umimpaired bindings is useless.
  (evil-collection-want-unimpaired-p nil)

  ;; NOTE evil-collection will setup a mode naemd 't' for 'sldb' and 'slime-inspector'.
  (evil-collection-setup-debugger-keys t)
  (evil-collection-want-find-usages-bindings t)
  (evil-collection-setup-minibuffer t)

  (evil-collection-term-sync-state-and-mode-p t)

  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-outline-bind-tab-p t)
  :config

  ;; TIP See the default list in evil-collection--supported-modes variable.
  (evil-collection-init evil-collection--supported-modes))

(use-package evil-escape
  :ensure t
  :after (evil)
  :custom
  ;; TIP Use 'key-convention': 'C-[' = 'Escape', 'C-i' = 'Tab' and 'C-m' = 'Return'. (Other convention: n/p -> j/k, BackSpace (insert-state) -> C-w/C-u/C-x)
  ;; TIP The order to escape: 'jk' > 'C-g' > 'C-[' > 'Escape'

  ;; Ignore the case for escape-key-sequence.
  (evil-escape-key-sequence "jk")
  (evil-escape-case-insensitive-key-sequence t)

  ;; Use a shorten delay, so that the `j' key is more responsive.
  (evil-escape-delay 0.1)

  ;; Exclude these modes, we use `q' key to quit in them.
  (evil-escape-excluded-major-modes '(magit-status-mode magit-diff-mode magit-log-mode
                                                        treemacs-mode))

  ;; Exclude the visual-state to make the visual selecting smooth.
  (evil-escape-excluded-states '(visual emacs))

  :config
  (evil-escape-mode))

(use-package evil-surround
  :ensure t
  :after (evil)
  :config
  ;; TIP To 'add' a 'surrounding', use 'S' in 'vi-visual-state' or use 'ys<textobject><surrounding>' in 'vi-normal-state' (The 'ys' is a new vi-operator.).
  ;; TIP to 'change' a 'surrounding', use 'cs<old-textobject><new-textobject>'.
  ;; TIP to 'delete' a 'surrounding', use 'ds<text-object>'.
  (global-evil-surround-mode 1))

(defun <help> () "The help for Emacs.")
(defun --->help () "The help commands.")

(use-package view
  :ensure nil
  :config
  (evil-define-key '(normal) view-mode-map (kbd "SPC") nil))

(use-package woman
  :ensure nil
  :commands (woman)
  :config
  (evil-define-key '(normal) woman-mode-map (kbd "SPC") nil)

  (evil-define-key '(normal) 'global (kbd "SPC u k") 'hl-todo-occur))

(use-package helpful
  :ensure t
  :commands (helpful-symbol helpful-function helpful-variable helpful-command helpful-key)
  :config
  ;; TIP Replace `describe-{object}' with `helpful-{object}' to get better *help* buffer.
  )

(use-package emacs
  :init
  ;; Shadow bindings.
  (evil-define-key '(normal) help-mode-map (kbd "SPC") nil)
  (evil-define-key '(normal) help-mode-map (kbd "S-SPC") nil)

  ;; TIP To list the 'built-in' packages in 'Emacs'.
  ;; TIP You can use `zm' to `fold' the help-buffer.
  (evil-define-key '(normal) 'global (kbd "SPC h e") 'finder-by-keyword)

  ;; NOTE The 'helm' package will define lots of 'helm-info-{package-name}' command for all packages.
  ;; TIP Use 'info' to read 'the manual of emacs', and 'info-display-manual' to read 'the manual of a package'.
  (evil-define-key '(normal) 'global (kbd "SPC h h") 'info)
  (evil-define-key '(normal) 'global (kbd "SPC h H") 'info-display-manual)

  (evil-define-key '(normal) 'global (kbd "SPC h d") 'apropos-documentation)
  (evil-define-key '(normal) 'global (kbd "SPC h D") 'shortdoc)

  (evil-define-key '(normal) 'global (kbd "SPC h b") 'describe-bindings)
  (evil-define-key '(normal) 'global (kbd "SPC h B") 'view-lossage)

  ;; TIP To read the `global-map' first.
  (evil-define-key '(normal) 'global (kbd "SPC h k") 'helpful-key)
  (evil-define-key '(normal) 'global (kbd "SPC h K") 'describe-keymap)

  ;; TIP Use `zm' to fold all sections.
  (evil-define-key '(normal) 'global (kbd "SPC h m") 'describe-mode)
  (evil-define-key '(normal) 'global (kbd "SPC h M") 'man)

  (evil-define-key '(normal) 'global (kbd "SPC h s") 'helpful-symbol)
  (evil-define-key '(normal) 'global (kbd "SPC h S") 'apropos)

  (evil-define-key '(normal) 'global (kbd "SPC h f") 'helpful-function)
  (evil-define-key '(normal) 'global (kbd "SPC h F") 'apropos-function)

  (evil-define-key '(normal) 'global (kbd "SPC h t") 'describe-text-properties)
  (evil-define-key '(normal) 'global (kbd "SPC h T") 'describe-face)

  (evil-define-key '(normal) 'global (kbd "SPC h v") 'helpful-variable)
  (evil-define-key '(normal) 'global (kbd "SPC h V") 'apropos-variable)

  (evil-define-key '(normal) 'global (kbd "SPC h l") 'apropos-local-variable)
  (evil-define-key '(normal) 'global (kbd "SPC h L") 'apropos-local-value)

  (evil-define-key '(normal) 'global (kbd "SPC h c") 'helpful-command)
  (evil-define-key '(normal) 'global (kbd "SPC h C") 'apropos-command)

  (evil-define-key '(normal) 'global (kbd "SPC h p") 'describe-package)
  (evil-define-key '(normal) 'global (kbd "SPC h P") 'apropos-library)

  (evil-define-key '(normal) 'global (kbd "SPC h o") 'apropos-user-option)
  (evil-define-key '(normal) 'global (kbd "SPC h O") 'apropos-value)
  :config
  ;; TIP Use `view-emacs-news' command to read Emacs change log.
  )

(use-package apropos
  :defer t
  :config
  ;; Color the apropos command.
  (set-face-attribute 'apropos-symbol nil
                      :foreground "#00FF00"))


(defun --->key-cast () "Display the inputed key and executed command.")
(use-package keycast
  :disabled t
  :ensure t
  :config
  ;; (keycast-tab-bar-mode)
  ;; (keycast-header-line-mode)
  )

(defun <assistant> () "The assist for life.")
(defun --->org () "Org-mode related.")
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  ;; TIP Use 'S-{arrow}' to control the 'priority' and 'status'. (Or 'SPC o {hjkl}')
  ;; TIP Use 'M-{arrow}' to control 'order' and 'level'.
  ;; TIP Use 'C-Ret' to insert a 'contextual-heading'.
  ;; TIP Use `info' command to read the manual of `org' package inside `Emacs'.
  (evil-define-key '(normal) org-mode-map (kbd "SPC o h") 'org-shiftleft)
  (evil-define-key '(normal) org-mode-map (kbd "SPC o j") 'org-shiftdown)
  (evil-define-key '(normal) org-mode-map (kbd "SPC o k") 'org-shiftup)
  (evil-define-key '(normal) org-mode-map (kbd "SPC o l") 'org-shiftright)

  ;; Set the search path for agenda files.
  (setq org-agenda-files (file-expand-wildcards "~/Workspace/github/note/*.org"))
  (add-to-list 'evil-normal-state-modes 'org-agenda-mode)

  (evil-define-key '(normal) org-mode-map (kbd "SPC o a") 'org-agenda)
  (evil-define-key '(normal) org-mode-map (kbd "SPC o c") 'org-goto-calendar)
  (evil-define-key '(visual) org-mode-map (kbd "SPC o s") 'org-sort)
  (evil-define-key '(normal) org-mode-map (kbd "SPC o m") 'org-babel-mark-block)

  ;; Face
  (set-face-background 'org-block-begin-line "#000000")
  (set-face-background 'org-block-end-line "#000000")

  (set-face-foreground 'org-headline-done "#00FF00")

  (set-face-foreground 'org-level-1 "#5454FC")
  (set-face-foreground 'org-level-2 "#FF0000")
  (set-face-foreground 'org-level-3 "#FFFF00")
  (set-face-foreground 'org-level-4 "orange")
  (set-face-foreground 'org-level-5 "sky blue")
  (set-face-foreground 'org-level-6 "SeaGreen1")

  ;; babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . nil)
     (lisp . t)
     ;; Use return(42) to provide the result value.
     (python . t)
     (C . t)
     (plantuml . t)))

  ;; (add-to-list 'org-babel-tangle-lang-exts '("c" "cpp"))

  ;; (setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml.jar"))
  ;; (setq org-plantuml-exec-mode 'jar)

  ;; TIP Insert `scale 1800*900' in plantuml source to control the output image resolution.
  (setq org-plantuml-exec-mode 'plantuml)
  (setq org-plantuml-executable-path "plantuml")
  (setq org-plantuml-args (list "-headless" "-DPLANTUML_LIMIT_SIZE=65536"))

  (setq org-confirm-babel-evaluate nil))

(use-package org-modern
  :ensure t
  :after (org)
  :hook (org-mode . org-modern-mode)
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  ;; Face
  (set-face-foreground 'org-modern-priority "gray")
  (set-face-foreground 'org-modern-todo "red")
  (set-face-foreground 'org-modern-done "green")

  ;; Ellipsis styling
  (setq org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

  ;; Customize star style.
  (setq org-modern-star 'replace)
  (setq org-modern-hide-stars 'nil))


;; NOTE A fancy render-engine for org is useless.
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package org-pomodoro
  :disabled t
  :ensure t
  :config
  )

(defun --->chat () "Chat with AI.")
(use-package gptel
  :ensure t
  :after (evil helm)
  :commands (gptel)
  :init
  (defun gptel-start-or-menu ()
    (interactive)
    (if (bound-and-true-p gptel-mode)
        (call-interactively 'gptel-menu)
      (call-interactively 'gptel)))

  (evil-define-key '(normal visual) 'global (kbd "SPC g") 'gptel-menu)
  (evil-define-key '(normal visual) 'global (kbd "SPC G") 'gptel)
  :config
  ;; NOTE It's recommemded to host an open-source chat-model locally.
  ;; TIP The key-point of `gptel' is `gpt-everywhere-interactively' and `use-tools'.
  ;; TIP gptel marks the model response with text-properties, so you can `append' or `in-place-editting' new input text freely without a prompt-prefix.
  ;; TIP Hover the `cursor' on `gpt-response', you can `tweak' the response.

  ;; TIP The possibility of chat includes: text generate, text complete, text improve, text expand, text shorten, text translate.
  ;; TIP Use `C-x' in `gptel-menu' to show advanced-menu.
  ;; TIP Use GPT for broad-phase exploration.
  ;; TIP Combine `gpt' with `org-babel', to evaluate the source code.
  ;; TIP Use `GPT' to `explore' new ideas, and get some inspiration.
  ;; TIP Use `G' to goto the last-line within prompt.
  ;; TIP Press `Ret' in `rewrite-mark' to accepr or reject the result.

  ;; TIP Image you are using a control pad to launch projectile. (gptel-menu)

  ;; Define api keys.
  (load "~/.emacs.d/token.el")

  ;; Transient
  ;; (setq transient-display-buffer-action
  ;;    '(display-buffer-below-selected
  ;;         (dedicated . t)
  ;;         (inhibit-same-window . t)))

  ;; Use org-mode.
  (setq gptel-default-mode 'org-mode)

  ;; Disable line truncate.
  (add-hook 'gptel-mode-hook 'toggle-truncate-lines)

  ;; Set backends and models.
  ;; TIP One backend/provider can contains more than one model.
  (setq gptel-model 'phi4:latest)

  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(phi4:latest
              llama3.2:latest
              deepseek-r1:1.5b
              deepseek-r1:8b
              deepseek-r1:14b
              llava:13b
              ))

  (put 'llava:13b :capabilities '(media))

  (setq gptel-model 'gemini-2.0-flash)
  (setq gptel-backend (gptel-make-gemini "Gemini"
                        :key (my/gemini-api-key)
                        :stream t))


  ;; (setq gptel-backend )


  ;; Fix conflicting between helm and transient. (https://github.com/magit/transient/discussions/361)
  (defun helm-current-window-configuration ()
    "Like `current-window-configuration' but deal with Transient incompatibility."
    (when (and (fboundp 'transient--preserve-window-p)
               (fboundp 'transient--delete-window)
               (not (transient--preserve-window-p)))
      ;; Noop if `transient--window' isn't a live window.
      (transient--delete-window))
    (current-window-configuration))

  (setq helm-save-configuration-functions '(set-window-configuration
                                            ;; Replacing `current-window-configuration':
                                            . helm-current-window-configuration))


  )

(use-package ellama
  :disabled t
  :ensure t
  :commands (ellama-transient-main-menu)
  :after (evil)
  :init
  (evil-define-key '(normal visual) 'global (kbd "SPC g") 'ellama-transient-main-menu)
  :config
  ;; TIP Use `ollama run llama3.1' to download the model and use `ellama-provider-select' to select a model.

  (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message)

  ;; Customize the LLM model.
  (setq ellama-provider (make-llm-ollama
                         :chat-model "phi4"
                         :embedding-model "phi4"))

  (setq ellama--current-session-id nil)
  (setq ellama-session-auto-save nil)

  ;; Customize chat-buffer.
  (setq ellama-naming-scheme #'(lambda (_provider _action _prompt) "LLM Chat Buffer"))
  (setq ellama-assistant-nick "Model"))

(defun --->todo () "Keyword highlight.")

(use-package hl-todo
  :ensure t
  :config

  ;; Enable globally.
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FFFF00")
          ("FIXME"  . "#FF0000")
          ("NOTE"  . "#0000FF")
          ("TIP"  . "#00FF00")
	  ("QUESTION" . "chocolate")
	  ("HERE" . "dark green")
	  ("DOC" . "gray50")
	  ("SUB-STEP" . "#00FF00")
	  ("STEP" . "#FFFF00")))
  (global-hl-todo-mode)

  ;; Bind.
  ;; (evil-define-key '(normal) 'global (kbd "SPC u t") 'hl-todo-occur)
  )

(defun --->feed () "The stable and topic-oriented information source.")
(use-package newsticker
  :ensure nil
  :commands (newsticker-treeview)
  :custom
  (newsticker-url-list '(("Emacs Wiki" "https://www.emacswiki.org/emacs?action=rss" nil 3600 nil)
                         ("Hacker News" "https://news.ycombinator.com/rss" nil 3600 nil)))
  :init
  ;; Keymap.
  (evil-define-key '(normal) 'global (kbd "SPC u r") 'newsticker-treeview)
  (evil-define-key '(normal) newsticker-treeview-mode-map (kbd "q") 'newsticker-treeview-quit))

;; (use-package elfeed
;;   :ensure t
;;   :config
;;   )

(defun <view> () "The display for Emacs.")
;; NOTE Exwm is still buggy, and easy to hang. I would try it again in the future. (The model used by exwm is buggy, due to the single-threaded event handling of Emacs)
;; NOTE KWin worsk, and I only needs less than 3 applications running in the fore-ground.

(defun --->display () "The appeareance of Emacs.")

;; NOTE Use a mono-spaced-font like 'source code pro' or 'hack'. (Font is set by KDE)
(use-package emacs
  :config
  ;;; Customize scratch-buffer.
  ;; The default mode for scratch buffer is lisp-interaction-mode.
  (setq initial-major-mode 'emacs-lisp-mode)
  (defun switch-to-scratch ()
    "Get a scratch buffer."
    (interactive)
    (let* ((name "*scratch*")
           (buf (get-buffer name)))
      (pop-to-buffer
       (if (bufferp buf)
           buf  ; Existing scratch buffer
         ;; New scratch buffer
         (with-current-buffer (get-buffer-create name)
           (current-buffer))))))

  ;; NOTE I have to ask the point of displaying line number of a file.
  ;; (global-display-line-numbers-mode)

  ;; (toggle-word-wrap)
  (setq-default inhibit-startup-screen t)

  ;; Don't blank the cursor.
  (blink-cursor-mode 0))

;; (use-package ultra-scroll
;;   :ensure t
;;   :config
;;   )

(defun --->dimmer () "Dim other windows.")

(use-package dimmer
  :ensure t
  :config
  (dimmer-mode t))

(defun --->theme () "The theme for Emacs.")
(use-package base16-theme
  :ensure t
  :config
  ;; TIP Don't use the 'transparent-frame', it's ugly.
  ;; NOTE See spec in https://github.com/chriskempson/base16/blob/main/styling.md
  ;; TIP Use 'list-colors-display' to see all known-colors.

  ;; NOTE Instead of defining a new theme, we modify the existing one for convinence.
  (setq base16-sakura-theme-colors
        '(:base00 "#000000" ;; default background: window
                  :base01 "#1C1C1C" ;; status bar, line numbers and folding marks
                  :base02 "#383838" ;; modeline, selection
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
  (load-theme 'base16-sakura t)

  (set-face-foreground 'tab-bar-tab "#00FF00")

  ;; Set the mark-region color.
  (set-face-background 'region "#006280")

  ;; Set the background of `fringe' the same as `base00', making it looks thin.
  (set-face-background 'fringe "#000000"))

(use-package hl-line
  :ensure t
  :after (base16-theme)
  :config
  ;; TIP You don't need `beacon' package if you line is high-lighted.

  ;; Override the 'hl-line' face.
  (global-hl-line-mode t)
  ;; Other choice: "dark blue", "navy", "#000066",
  (set-face-background 'hl-line "#000066"))

;; TIP Color the hex color code, useful for web development.
(use-package colorful-mode
  :ensure t
  ;; NOTE The global colorful mode will makes the `list-face-display' failed to work.
  :hook (prog-mode . colorful-mode))

(defun --->mini-buffer () "Customize mini-buffer.")
(use-package helm
  :after (evil)
  :init
  ;; TIP The 'which-key' extension is useless, just use 'mini-buffer' to search for a command, or the `describe-bindings' command to list the bindings in current buffer.
  ;; TIP The 'helm' package provides lots of 'decorated-commands': https://github.com/emacs-helm/helm/wiki/Fuzzy-matching.
  ;; NOTE In the default 'mini-buffer' provided by 'emacs', it allows you to select one entry from 'single-source'. In the 'mini-window' provided by 'helm', you can select one entry from 'multiple-source'.
  ;; TIP Use 'C-h m' to display the 'helm' manual.
  ;; TIP To pass a 'universal-arg' to 'helm', just hold-on the 'C-u-9' or 'M-9' after execute 'helm-M-x' command.
  ;; TIP Use `C-o' to execute `helm-next-source'.
  ;; TIP Read `helm-easymenu.el' to see the outline of functions provided by helm.
  ;; TIP If you are using `helm' as the completion-ui, you don't need the `orderless' package. Helm is `multi-pattern-matching' separated by `space' key by default.

  ;; Override the default emacs bindings.
  (evil-define-key '(normal) 'global (kbd "SPC a") 'helm-M-x)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)

  ;; Display short-doc for commands.
  (setq helm-M-x-show-short-doc t)

  ;; Enable mode.
  (helm-mode 1)

  :config

  ;; Faces.
  (set-face-background 'helm-selection "#000066")

  ;; NOTE It's impossible to re-map bindinds in 'helm-M-x-mode' using 'evil'.
  ;; Bind the navigation keys.
  (evil-define-key '(normal insert) helm-map (kbd "C-j") 'helm-next-line)
  (evil-define-key '(normal insert) helm-map (kbd "C-k") 'helm-previous-line)

  (evil-define-key '(normal insert) helm-map (kbd "C-d") 'helm-next-page)
  (evil-define-key '(normal insert) helm-map (kbd "C-u") 'helm-previous-page)

  ;; (evil-define-key '(normal insert) helm-map (kbd "C-l") 'helm-execute-persistent-action)

  ;; Used to overwrite the `TIP' message.
  (define-key helm-map (kbd "C-j") 'helm-next-line)

  ;; Ignore helm-buffers in buffer switcher.
  (push "\*helm.*" switch-to-prev-buffer-skip-regexp)

  ;; Ignore Ibuffer in buffer switcher.
  (push "\*Ibuffer.*" switch-to-prev-buffer-skip-regexp)

  ;; Set fuzzy matching.
  (setq helm-M-x-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)

  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)

  (setq helm-ff-fuzzy-matching t)
  (setq helm-file-cache-fuzzy-match t)
  (setq helm-locate-fuzzy-match t)
  (setq helm-session-fuzzy-match t)
  (setq helm-etags-fuzzy-match t)

  (setq helm-completion-style 'helm)


  ;; CUstomize helm-semantic module.
  (setq helm-semantic-fuzzy-match t)

  ;; Customize helm-menu module.
  ;; TIP Use `helm-imenu' module provided by `helm' pakcage instead of `imenu-anywhere' package.
  (setq helm-imenu-all-buffer-assoc '((emacs-lisp-mode . lisp-mode)
                                      (lisp-mode . c-mode)
                                      (c-mode . c++-mode)))
  (setq helm-imenu-fuzzy-match t)
  (setq helm-imenu-use-icon t))


(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  ;; NOTE The `projectile-find-file' does not support fuzzy-matching, so install this package for `helm-projectile-find-file' command.
  )

(defun --->mode-line () "Customize mode-line.")
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  ;; NOTE Currently, the 'doom-modeline' is the only one that actively developed.

  ;; Display the full buffer file name.
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project)

  ;; Display the 'evil-state' as 'text'.
  (setq doom-modeline-modal-icon nil)

  ;; Display the column num.
  (setq column-number-mode t)

  ;; Display the macro-register.
  (setq doom-modeline-always-show-macro-register t)

  ;; Display project name.
  (setq doom-modeline-project-name t)

  ;; Display match counts in visual-replace.
  (setq visual-replace-display-total t))

(use-package which-func
  :config
  ;; TIP Display the function name at point in mode-line.
  (which-function-mode))

(use-package time
  :custom
  ;; Use 24hrs.
  (display-time-24hr-format t)

  ;; Enable day-and-date indicator.
  (display-time-day-and-date t)

  ;; Disable the system load indicator.
  (display-time-default-load-average nil)

  :config
  ;; Display date and time in Emacs.
  ;; (display-time-mode)
  )

(use-package evil-anzu
  :ensure t
  :config
  ;; NOTE Enable hit-counter for evil-isearch in mode-line.
  (global-anzu-mode +1))

;; (use-package sideline
;;   :ensure t
;;   :init
;;   (setq sideline-backends-left '(sideline-flycheck)
;;         sideline-backends-right '(sideline-lsp))

;;   (setq sideline-backends-left-skip-current-line t   ; don't display on current line (left)
;; 	sideline-backends-right-skip-current-line t  ; don't display on current line (right)
;; 	sideline-order-left 'down                    ; or 'up
;; 	sideline-order-right 'up                     ; or 'down
;; 	sideline-format-left "%s   "                 ; format for left aligment
;; 	sideline-format-right "   %s"                ; format for right aligment
;; 	sideline-priority 100                        ; overlays' priority
;; 	sideline-display-backend-name t))            ; display the backend name

(defun --->buffer () "Buffer related.")

(use-package emacs
  :config
  ;; NOTE buffer < window < tab < frame
  ;; TIP You should not use 'bookmark' facility at all, if there is 'recentf'. (Try increasing the `recentf-max-history')
  ;; TIP To filter the result with '.lisp', using the pattern '*lisp'.
  ;; TIP The 'helm-mini' combines 'list-buffers' and 'recentf' as multiple sources.
  (evil-define-key '(normal) 'global (kbd "SPC b b") 'helm-mini)
  (evil-define-key '(normal) 'global (kbd "SPC b B") (lambda ()
                                                       (interactive)
                                                       (call-interactively 'tab-bar-new-tab)
                                                       (call-interactively 'helm-mini)))

  (defun my/indent-buffer ()
    "Indents an entire buffer using the default intenting scheme."
    (interactive)
    (point-to-register 'o)
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))
    (jump-to-register 'o))
  (evil-define-key '(normal) 'global (kbd "SPC b f") 'my/indent-buffer)

  (evil-define-key '(normal) 'global (kbd "SPC b l") 'list-buffers)

  (evil-define-key '(normal) 'global (kbd "SPC b n") 'switch-to-next-buffer)
  (evil-define-key '(normal) 'global (kbd "SPC b p") 'switch-to-prev-buffer)

  (evil-define-key '(normal) 'global (kbd "SPC b d") 'kill-buffer)

  (evil-define-key '(normal) 'global (kbd "SPC b s") 'switch-to-scratch))

(defun --->window () "Window related.")

(use-package emacs
  :config
  ;; TIP It's okay to use vim window related bindings: 'C-w-{s/v}', 'C-w-{hjklw}', 'C-w{qx}'
  (evil-define-key '(normal) 'global (kbd "SPC s h") (lambda ()
                                                       (interactive)
                                                       (call-interactively 'split-window-horizontally)
                                                       (call-interactively 'evil-window-next)))

  (evil-define-key '(normal) 'global (kbd "SPC s v") (lambda ()
                                                       (interactive)
                                                       (call-interactively 'split-window-vertically)
                                                       (call-interactively 'evil-window-next)))

  ;; TIP The 'window-swap-states' can 'transpose' current-window and next-window.
  ;; NOTE To write window layout rules, use a window manager -> https://depp.brause.cc/shackle/
  (evil-define-key '(normal) 'global (kbd "SPC w t") 'window-swap-states)
  (evil-define-key '(normal) 'global (kbd "SPC w h") 'windmove-swap-states-left)
  (evil-define-key '(normal) 'global (kbd "SPC w j") 'windmove-swap-states-down)
  (evil-define-key '(normal) 'global (kbd "SPC w k") 'windmove-swap-states-up)
  (evil-define-key '(normal) 'global (kbd "SPC w l") 'windmove-swap-states-right)

  (evil-define-key '(normal) 'global (kbd "SPC w m") 'maximize-window)
  (evil-define-key '(normal) 'global (kbd "SPC w M") 'minimize-window)
  (evil-define-key '(normal) 'global (kbd "SPC w b") 'balance-windows)

  ;; TIP If the key-bindings conflicts with the major-mode, fallback to 'C-w-{whjkl}'.
  (evil-define-key '(normal) 'global (kbd "C-h") 'evil-window-left)
  (evil-define-key '(normal) 'global (kbd "C-j") 'evil-window-down)
  (evil-define-key '(normal) 'global (kbd "C-k") 'evil-window-up)
  (evil-define-key '(normal) 'global (kbd "C-l") 'evil-window-right)

  (evil-define-key '(normal) 'global (kbd "SPC w d") 'delete-window)
  (evil-define-key '(normal) 'global (kbd "SPC w D") 'delete-other-windows))

(use-package winner
  :config
  ;; TIP Undo the window that deleted accidently.
  (winner-mode)
  (evil-define-key '(normal) 'global (kbd "SPC w u") 'winner-undo)
  (evil-define-key '(normal) 'global (kbd "SPC w U") 'winner-redo))

(defun --->tab () "Tab related.")
(use-package tab-bar
  :config
  ;; NOTE Besides the `tab-bar', there is a `tab-line' for each `tab'.
  ;; TIP Use `C-Tab` and `C-S-Tab` to cycle tabs.
  ;; TIP The `tab-switch` will switch to the named tab or create it.

  ;; Customize the view of tabs.
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-button-show nil)
  (setq tab-bar-tab-hints t)

  ;; Disable the auto-width function.
  (setq tab-bar-auto-width nil)

  ;; Set tab naming function.
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-truncated)

  (evil-define-key '(normal) 'global (kbd "SPC t t") 'tab-switch)
  (evil-define-key '(normal) 'global (kbd "SPC t c") 'tab-bar-new-tab)

  (keymap-unset evil-motion-state-map "SPC")
  (evil-define-key '(normal motion) 'global (kbd "SPC 0") (lambda () (interactive) (tab-bar-switch-to-recent-tab)))
  (evil-define-key '(normal motion) 'global (kbd "SPC 1") (lambda () (interactive) (tab-select 1)))
  (evil-define-key '(normal motion) 'global (kbd "SPC 2") (lambda () (interactive) (tab-select 2)))
  (evil-define-key '(normal motion) 'global (kbd "SPC 3") (lambda () (interactive) (tab-select 3)))
  (evil-define-key '(normal motion) 'global (kbd "SPC 4") (lambda () (interactive) (tab-select 4)))
  (evil-define-key '(normal motion) 'global (kbd "SPC 5") (lambda () (interactive) (tab-select 5)))
  (evil-define-key '(normal motion) 'global (kbd "SPC 6") (lambda () (interactive) (tab-select 6)))
  (evil-define-key '(normal motion) 'global (kbd "SPC 7") (lambda () (interactive) (tab-select 7)))
  (evil-define-key '(normal motion) 'global (kbd "SPC 8") (lambda () (interactive) (tab-select 8)))
  (evil-define-key '(normal motion) 'global (kbd "SPC 9") (lambda () (interactive) (tab-select 9)))

  (evil-define-key '(normal) 'global (kbd "SPC t n") 'tab-next)
  (evil-define-key '(normal) 'global (kbd "SPC t p") 'tab-previous)

  (evil-define-key '(normal) 'global (kbd "SPC t m") 'tab-bar-move-tab-to)

  (evil-define-key '(normal) 'global (kbd "SPC t d") 'tab-close)
  (evil-define-key '(normal) 'global (kbd "SPC t D") 'tab-close-other)

  (evil-define-key '(normal) 'global (kbd "SPC t u") 'tab-bar-undo-close-tab)

  ;; Fix: the lsp-mode buffer wil trigger <tab-bar> <mouse-movement> is undefined
  (define-key tab-bar-map (kbd "<mouse-movement>") #'ignore)

  ;; Macros. (Deprecated)
  (defmacro with-new-tab-bar (&rest body)
    "Create a new tab and execute BODY in the new tab context."
    `(progn
       (tab-bar-new-tab)
       ,@body))
  )


(defun --->frame () "Frame related.")
(use-package emacs
  :config
  ;; TIP Use 'Super+{alpha}' to switch to a useful program. (wmctrl -a "gnu emacs" || emacs): terminal, emacs, browser.
  ;; NOTE `super-q' to jump to Emacs, `super-w' to jump to web browser.
  ;; TIP Use 'Super+{num}' to switch to a window in KDE.
  (evil-define-key '(normal) 'global (kbd "SPC z z") 'toggle-frame-fullscreen)
  (evil-define-key '(normal) 'global (kbd "SPC z m") 'toggle-frame-maximized)

  ;; TIP To quit in vi, use 'ZQ'.
  (evil-define-key '(normal) 'global (kbd "Z R") 'restart-emacs)

  ;; TIP Use 'ZM' to escape from a 'dead-window'.
  (evil-define-key '(normal) 'global (kbd "Z M") 'helm-mini))

(defun --->session () "Session related.")
;; NOTE The desktop-save-mode saves the whole state of Emacs, making it hard to debug and test new functions.
;; (desktop-save-mode)
;; (setq desktop-restore-eager nil)

(defun <file> () "Files for Emacs.")
(defun --->file () "File related.")
(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 200)
  :config
  ;; Exclude files.
  (add-to-list 'recentf-exclude ".*pdf.*")
  (add-to-list 'recentf-exclude (temporary-file-directory))
  (add-to-list 'recentf-exclude (concat (file-name-as-directory package-user-dir)
                                        ".*-autoloads\\.el\\'")))

(use-package bm
  :ensure t
  :config
  ;; TIP One-time buffer-local bookmark with line-highlighting.

  ;; Keymap.
  (evil-define-key '(normal) global-map (kbd "SPC m m") 'bm-toggle)
  (evil-define-key '(normal) global-map (kbd "SPC m p") 'bm-previous)
  (evil-define-key '(normal) global-map (kbd "SPC m n") 'bm-next)

  ;; Face.
  (set-face-background 'bm-face "gray50"))


(use-package dired
  :ensure nil
  :config
  ;; Keymap.
  (evil-define-key '(normal) dired-mode-map (kbd "SPC") nil)
  (evil-define-key '(normal) dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key '(normal) dired-mode-map (kbd "l") 'dired-find-file))

;; (use-package visual-replace
;;   :disabled t
;;   :ensure t
;;   ;; :bind (("C-c r" . visual-replace)
;;   ;;        :map isearch-mode-map
;;   ;;        ("C-c r" . visual-replace-from-isearch))

;;   :config
;;   (visual-replace-global-mode 1)
;;   )

(use-package visual-regexp
  :ensure t
  :commands (vr/query-replace)
  :config)

(use-package treemacs
  :ensure t
  :after (projectile)
  :init
  ;; NOTE Should not use `treemacs-tab-bar' package, it's buggy. The default model used by treemacs is powerful, which supports to make muptile projects as a workspace.
  ;; TIP A good editor will not let you save files 'manually'.
  (evil-define-key '(normal) 'global (kbd "SPC f f") 'helm-find-files)

  ;; TIP To expand a node recursively, push a `prefix-arg'.
  (evil-define-key '(normal) 'global (kbd "SPC f t") 'treemacs)
  (evil-define-key '(normal) 'global (kbd "SPC f d") 'dired)

  (evil-define-key '(normal) 'global (kbd "SPC f c") 'treemacs-create-file)
  (evil-define-key '(normal) 'global (kbd "SPC f C") 'treemacs-create-dir)

  (evil-define-key '(normal) 'global (kbd "SPC f g") 'vr/query-replace)
  (evil-define-key '(normal) 'global (kbd "SPC f G") 'rg)


  (evil-define-key '(normal) 'global (kbd "SPC f w s") 'treemacs-switch-workspace)
  (evil-define-key '(normal) 'global (kbd "SPC f w e") 'treemacs-edit-workspaces)
  (evil-define-key '(normal) 'global (kbd "SPC f w c") 'treemacs-create-workspace)
  (evil-define-key '(normal) 'global (kbd "SPC f w d") 'treemacs-remove-workspace)
  (evil-define-key '(normal) 'global (kbd "SPC f w r") 'treemacs-rename-workspace)

  :config
  ;; NOTE For better integration, use 'treemacs' as the file explorer.
  ;; TIP Press '?' in the 'treemacs window' for 'normal-help'. Press 'C-?' for 'advanced-help'.
  ;; TIP To navigate in treemacs-window, use 'hjkl' and 'C-{j/k}'.
  ;; TIP Use 'M-m' to mark multiple files.
  (progn
    (setq treemacs-collapse-dirs                       (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay          0.5
          treemacs-directory-name-transformer        #'identity
          treemacs-display-in-side-window            t
          treemacs-eldoc-display                     'simple
          treemacs-file-event-delay                  2000
          treemacs-file-extension-regex      treemacs-last-period-regex-value
          treemacs-file-follow-delay                 0.2
          treemacs-file-name-transformer             #'identity
          treemacs-follow-after-init                 t
          treemacs-expand-after-init                 t
          treemacs-find-workspace-method             'find-for-file-or-pick-first
          treemacs-git-command-pipe                  ""
          treemacs-goto-tag-strategy                 'refetch-index
          treemacs-header-scroll-indicators          '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory            nil
          treemacs-indentation               2
          treemacs-indentation-string                " "
          treemacs-indent-guide-style                'line
          treemacs-is-never-other-window             nil
          treemacs-max-git-entries                   5000
          treemacs-missing-project-action            'ask
	  ;; Disable mouse-dragging action.
          treemacs-move-files-by-mouse-dragging    nil
          treemacs-move-forward-on-expand            nil
          treemacs-no-png-images                     nil
          treemacs-no-delete-other-windows           t
          treemacs-project-follow-cleanup            nil
          treemacs-persist-file              (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                          'left
          treemacs-read-string-input                 'from-child-frame
          treemacs-recenter-distance                 0.1
          treemacs-recenter-after-file-follow        nil
          treemacs-recenter-after-tag-follow         nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories                '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home          nil
          treemacs-show-cursor               nil
          treemacs-show-hidden-files                 t
          treemacs-silent-filewatch                  nil
          treemacs-silent-refresh                    nil
          treemacs-sorting                           'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes          t
          treemacs-tag-follow-cleanup                t
          treemacs-tag-follow-delay                  1.5
          treemacs-text-scale                        nil
          treemacs-user-mode-line-format             nil
          treemacs-user-header-line-format           nil
          treemacs-wide-toggle-width                 70
          treemacs-width                             30
          treemacs-width-increment                   1
          treemacs-width-is-initially-locked         t
          treemacs-workspace-switch-cleanup          nil)

    ;; icon
    (treemacs-resize-icons 22)

    ;; toggles
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

    ;; Should not display the .gitignore files.
    (setq treemacs-hide-gitignored-files-mode t)

    ;; Enable the indent in treemacs.
    (treemacs-indent-guide-mode)

    ;; Override keymap
    (evil-define-key 'treemacs treemacs-mode-map (kbd "C-h")  'evil-window-left)
    (evil-define-key 'treemacs treemacs-mode-map (kbd "C-l")  'evil-window-right)
    (evil-define-key 'treemacs treemacs-mode-map (kbd "SPC j") 'avy-goto-word-0)))

(use-package treemacs-evil
  :ensure nil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :ensure nil
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :ensure nil
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-icons-dired
  :ensure nil
  :after (treemacs dired)
  :config
  ;; TIP Decorate the `dired' command with `icons' version.
  (treemacs-icons-dired-mode))

(use-package quickrun
  :ensure t
  :config
  ;; TIP Compile and run anything using single-file project.
  (evil-define-key '(normal) global-map (kbd "SPC b r") 'quickrun))

(defun --->project () "Project related.")
(use-package rg
  :ensure t
  :hook (projectile-mode . ignore)
  :config
  (evil-define-key '(normal) rg-mode-map (kbd "M-j") 'rg-next-file)
  (evil-define-key '(normal) rg-mode-map (kbd "M-k") 'rg-prev-file))

;; NOTE Use 'projectile' as a project interface layer, to 'discovery' and 'indexing' projects.
(use-package projectile
  :ensure t
  :after (tab-bar)
  :config
  ;; TIP Enforce the 'projectile-commands' to be executed inside a 'project' indicated by a 'project-makrer'. (I don't want to use projectile commands in the home directory.)
  (setq projectile-require-project-root nil)

  ;; Include current project in the project switcher.
  (setq projectile-current-project-on-switch 'keep)

  ;; Include the top-level dir in find-dir.
  (setq projectile-find-dir-includes-top-level t)

  ;; Set completion system.
  (setq projectile-completion-system 'helm)

  ;; Auto-discovery projects.
  (setq projectile-auto-discover t)
  (setq projectile-project-search-path '("~/Workspace/github"
					 "~/.roswell/lisp/quicklisp/local-projects/"))

  ;; Bind
  (evil-define-key '(normal) 'global (kbd "SPC p p") 'projectile-switch-project)
  (evil-define-key '(normal) 'global (kbd "SPC p P") (lambda ()
                                                       (interactive)
                                                       (tab-bar-new-tab)
                                                       (projectile-switch-project)))

  (evil-define-key '(normal) 'global (kbd "SPC p b") 'projectile-switch-to-buffer)
  (evil-define-key '(normal) 'global (kbd "SPC SPC") 'helm-projectile-find-file)
  (evil-define-key '(normal) 'global (kbd "SPC p f") 'helm-projectile-find-file)

  ;; NOTE For `gitignored files', the `treemacs' shows them, but `projectile' hides them.
  (evil-define-key '(normal) 'global (kbd "SPC p i") 'projectile-project-info)
  (evil-define-key '(normal) 'global (kbd "SPC p h") 'projectile-dired)
  (evil-define-key '(normal) 'global (kbd "SPC p F") 'projectile-find-file-other-window)
  (evil-define-key '(normal) 'global (kbd "SPC p d") 'projectile-find-dir)
  (evil-define-key '(normal) 'global (kbd "SPC p r") 'projectile-recentf)

  (evil-define-key '(normal) 'global (kbd "SPC p w") 'projectile-save-project-buffers)

  (evil-define-key '(normal) 'global (kbd "SPC p s") 'projectile-run-vterm-other-window)
  (evil-define-key '(normal) 'global (kbd "SPC p S") 'projectile-run-shell-command-in-root)

  ;; TIP The `tags' does make errors, but the `grep'.
  ;; TIP Use 'C-j' and 'C-k' to show the details in 'grep-result-window'.
  ;; TIP Use `ripgrep' for: better result highlight, respect .gitignore file.
  (evil-define-key '(normal) 'global (kbd "SPC p g") (lambda () (interactive)
                                                       ;; Push the 4 as prefix-arg to enable the regex pattern.
                                                       (let ((current-prefix-arg 4))
                                                         (call-interactively #'projectile-ripgrep))))
  (evil-define-key '(normal) 'global (kbd "SPC p G") 'projectile-replace-regexp)

  (evil-define-key '(normal) 'global (kbd "SPC p !") 'projectile-run-shell-command-in-root)
  (evil-define-key '(normal) 'global (kbd "SPC p &") 'projectile-run-async-shell-command-in-root)

  (evil-define-key '(normal) 'global (kbd "SPC p C") 'projectile-compile-project)
  (evil-define-key '(normal) 'global (kbd "SPC p R") 'projectile-run-project)
  (evil-define-key '(normal) 'global (kbd "SPC p D") 'dap-hydra)
  (evil-define-key '(normal) 'global (kbd "SPC p P") 'projectile-package-project)
  (evil-define-key '(normal) 'global (kbd "SPC p I") 'projectile-install-project)
  (evil-define-key '(normal) 'global (kbd "SPC p T") 'projectile-test-project)

  (evil-define-key '(normal) 'global (kbd "SPC p m") 'projectile-add-known-project)
  (evil-define-key '(normal) 'global (kbd "SPC p M") 'projectile-remove-known-project)
  ;; Pin a project to treemacs.
  (evil-define-key '(normal) 'global (kbd "SPC p P") 'treemacs-projectile)

  (evil-define-key '(normal) 'global (kbd "SPC p L") 'projectile-toggle-project-read-only)

  (evil-define-key '(normal) 'global (kbd "SPC p v") 'projectile-vc)

  ;; Enable mode.
  (projectile-mode +1))

(use-package magit
  :ensure t
  ;; :commands (magit)
  ;; :hook (prog-mode . ignore)
  :config
  ;; Set a high-contrast color for hunk high-light. Or #000066, #001847.
  (set-face-attribute 'magit-diff-context-highlight nil
                      :background "navy"))

(use-package diff-hl
  :ensure t
  :after (magit)
  :config
  ;; Integrate with magit vc-backend package.
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  ;; Left click the diff-hl indicator to show the hunk.
  (global-diff-hl-show-hunk-mouse-mode)

  ;; Display the diff region in all buffers.
  (global-diff-hl-mode))

(defun <navigation> () "The navigation in Emacs.")
(defun --->goto () "Goto commands for vi.")
(use-package emacs
  :ensure nil
  :config
  ;; TIP The 'g' and 'z' are prefix-key left for user.
  ;; gj / gk ---> logical line
  ;; ge / GE ---> backward word end / backward broad word end
  ;; GJ ---> join line (without one space) ('J' = join line with one space)
  ;; Gu / GU ---> downcase / upcase operator (e.g. 'guiw'). (Or you can just press 'u/U' in vi-visual-state, or use combination `gUiw')
  ;; gz -> goto 'emacs-lisp-repl' provided by 'ielm'.

  ;; gd / gr ---> find definition / find references.
  ;; TIP Use 'C-j' and 'C-k' to show the details in 'xref-window'.
  (evil-define-key '(normal) 'global (kbd "g s") 'helm-lsp-workspace-symbol)
  (evil-define-key '(normal) 'global (kbd "g S") 'helm-lsp-global-workspace-symbol)

  (evil-define-key '(normal) 'global (kbd "g h") 'lsp-treemacs-call-hierarchy)
  (evil-define-key '(normal) 'global (kbd "g H") 'lsp-treemacs-type-hierarchy)

  (evil-define-key '(normal) 'global (kbd "g c") 'flycheck-list-errors)
  (evil-define-key '(normal) 'global (kbd "g C") 'lsp-treemacs-errors-list)

  ;; gm -> as a shortcut of '%'.
  (evil-define-key '(normal visual) 'global (kbd "g m") 'evil-jump-item)

  (evil-define-key '(normal) 'global (kbd "g b") 'beginning-of-defun)

  ;; Find files.
  (evil-define-key '(normal) 'global (kbd "g f") 'helm-for-files)

  ;; NOTE Vim use 'gt' and 'gT' to cycle 'tab', but we use it to goto a 'tag'.
  (evil-define-key '(normal) 'global (kbd "g t") 'helm-imenu-in-all-buffers)
  (evil-define-key '(normal) 'global (kbd "g T") 'helm-semantic-or-imenu)

  (evil-define-key '(normal) 'global (kbd "g o") 'helm-occur)

  ;; Yank and kill-ring.
  (evil-define-key '(normal) 'global (kbd "g p") 'helm-show-kill-ring)

  ;; TIP Use `gf' and `gF' to find file at point.
  (evil-define-key '(normal) 'global (kbd "g x") 'browse-url-at-point)
  (evil-define-key '(normal) 'global (kbd "g X") 'browse-url-xdg-open))

(defun --->impaired () "Impaired commands for vi.")

(use-package emacs
  :ensure nil
  :config
  ;; TIP Use `}}' to forward `paragraph', and use `]]' to forward `section'.
  (evil-define-key '(normal) 'global (kbd "[ [") 'evil-backward-section-begin)
  (evil-define-key '(normal) 'global (kbd "] ]") 'evil-forward-section-begin))

(defun --->jump-anywhere () "Jump to anywhere.")

(use-package avy
  :ensure t
  :config
  ;; TIP Jump the cursor to anywhere, even in 'vi-visual-state' and 'vi-insert-state' mode.
  ;; NOTE You don't need to use 'smooth-scroll', just use `avy' or `grep'.

  (evil-define-key '(normal visual) 'global (kbd "SPC j") 'avy-goto-word-0)

  ;; Define the face to be clearer.
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

(defun --->jump-list () "The jump-list in vi.")
;; TIP To navigate the 'jump-list', use 'C-i' and 'C-o'. (Use double ' to 'jump-back-and-forth')

(defun --->change-list () "The change-list in vi.")
;; TIP To navigate the 'change-list', use "g;" and "g,".
;; TIP To jump to 'last-change-location', use "'.".
;; TIP To jump to 'last-change-location' and enter 'vi-insert-state' mode, use 'gi'.
(evil-define-key '(normal visual) 'global (kbd "[ c") 'evil-goto-last-change)
(evil-define-key '(normal visual) 'global (kbd "] c") 'evil-goto-last-change-reverse)

(defun --->better-HL () "Easy way to press & and $.")
(use-package evil
  :config
  ;; NOTE The 'J' is used to join following lines to current-line, and 'K' is used for manual.
  ;; TIP use 'zz' to center current line.
  ;; TIP use 'M' to center current window. (Use 'C-e' and 'C-y' to move-screen one-line down/up)
  ;; TIP Use '{' and '}' to forward/backward a paragraph.
  (evil-define-key '(normal visual) 'global "H" 'evil-beginning-of-line)
  (evil-define-key '(normal visual) 'global "L" 'evil-end-of-line))

(defun <edit> () "The edit in Emacs.")

(defun --->saver () "Auto save files.")
(progn
  (setq auto-save-interval 20)
  (setq auto-save-timeout 1)
  (setq auto-save-no-message nil)

  ;; Suppress the `Wrote file...' message.
  (setq save-silently t)

  ;; The instant method, performance may be poor.
  ;; (defun save-buffer-instantly (begin end prev-text)
  ;;     (when (and (buffer-file-name)
  ;;         (file-writable-p (buffer-file-name))
  ;;         (buffer-modified-p))
  ;;       (save-buffer)))
  ;; (add-hook 'after-change-functions 'save-buffer-instantly)

  ;; (add-hook 'before-save-hook 'delete-trailing-whitespace*)
  ;; (defun delete-trailing-whitespace* ()
  ;;   (when (derived-mode-p 'prog-mode)
  ;;     (delete-trailing-whitespace)))

  ;; Delete trailing whitespace on save.
  (defun my/save-buffer ()
    (interactive)
    (when (and (buffer-file-name)
               (buffer-modified-p)
               (evil-normal-state-p))
      (save-buffer)))

  ;; Save buffer when Emacs lose the focus.
  (add-hook 'focus-out-hook 'my/save-buffer)

  ;; Save buffer when Vi enters the normal-state.
  (add-hook 'evil-normal-state-entry-hook 'my/save-buffer)

  ;; Save buffer after execution of some commands.
  (add-hook 'post-command-hook (lambda ()
                                 (when (or (eq this-command 'evil-delete)
                                           (eq this-command 'evil-delete-char)
                                           (eq this-command 'evil-join))
                                   (my/save-buffer))))

  ;; Save buffer when window state changed.
  (add-hook 'window-state-change-hook 'my/save-buffer)
  )

(defun --->read-only () "Read-only files.")
(use-package hardhat
  :ensure t
  :config
  ;; TIP Need to re-enable the global mode to apply user option changes.
  (global-hardhat-mode 1)

  ;; Source of lisp implementations.
  (push ".*/.roswell/src/.*" hardhat-fullpath-protected-regexps)

  ;; Source of libraries.
  (push ".*/.roswell/lisp/quicklisp/dists/.*" hardhat-fullpath-protected-regexps)

  ;; Source of Emacs.
  (push ".*/github/emacs/.*" hardhat-fullpath-protected-regexps)
  (push ".*/.cache/yay/.*" hardhat-fullpath-protected-regexps)

  ;; (push ".*/github/raylib/.*" hardhat-fullpath-protected-regexps)
  )


(defun --->complete () "Complete text.")
;; NOTE A good 'complete' package only requires you to press 'return' key, and never let you press 'tab' key.
;; NOTE If you need the same number of key-stroke, why use fuzzy?
(use-package company
  :ensure t
  :config
  ;; NOTE The 'company' package has better integration than 'auto-complete' package.

  ;; Trigger the completion with only 1 prefix character.
  (setq company-minimum-prefix-length 1)
  ;; (setq company-idle-delay
  ;;       (lambda () (if (company-in-string-or-comment) nil 0)))

  (setq company-idle-delay 0)

  ;; Transform the sorted result.
  (setq company-transformers '(delete-consecutive-dups
                               company-sort-by-occurrence
			       company-sort-prefer-same-case-prefix))

  ;; Inhibit the completion inside symbol.
  ;; TODO The company completion will not replace the behind characters, making it hard to use inside symbol.
  (setq company-inhibit-inside-symbols t)

  ;;; Keymap.
  ;; TIP Use 'C-n' and 'C-p' to in 'vi-insert-state' to trigger/select the entry in completion-window.
  ;; TIP Use 'key-conversion' to translate 'C-m' to 'RET'.
  ;; TIP Use 'C-h' to open the 'quick-doc', use 'C-w' to show the 'source'.
  ;; TIP Use 'M-{digit}' to quick select the completion entry in popup window.
  ;; TIP To see advanced usage, list the `company-active-map'.
  (define-key company-active-map (kbd "C-w") nil)
  (define-key company-active-map (kbd "C-l") 'company-show-location)

  (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)

  ;; (add-hook 'c-mode-hook (lambda ()

  ;;                       ;; Add yasnippet company-backend with slime company-backend.
  ;;                       ;; (when (member 'company-slime company-backends)
  ;;                       ;;   (delete 'company-slime company-backends)
  ;;                       ;;   (push '(company-slime :with company-yasnippet) company-backends))

  ;;                       (when (member 'company-capf company-backends)
  ;;                         (delete 'company-capf company-backends)
  ;;                         (push '(company-capf :with company-yasnippet) company-backends))
  ;;                       ))

  (defmacro company-backend-for-hook (hook backends)
    "Add a HOOK to set `company-backends' dynamically.

HOOK is the name of the hook to which the configuration function
is added. BACKENDS is the list of backends to set for
`company-backends' in the local buffer when the hook is run.

Example usage:

  (company-backend-for-hook 'prog-mode-hook
                            '((company-capf :with company-yasnippet)
                              company-dabbrev-code))

This will configure `company-backends' for all `prog-mode'
buffers to include `company-capf' (with optional yasnippet) and
`company-dabbrev-code'."
    `(add-hook ,hook (lambda()
                       (set (make-local-variable 'company-backends)
                            ,backends))))


  ;; Fix: for `(sb-vm:)' string, you can't press tab key to open the company completion window.
  ;; NOTE: Only set the <tab> key for `prog-mode'. Should not set it for helm-M-x mode, or you will get `Company not enabled.' in mini-buffer.
  (add-hook 'prog-mode-hook
            (lambda ()
              (define-key evil-insert-state-local-map (kbd "<tab>") 'indent-for-tab-command)))

  ;; (define-key evil-insert-state-map (kbd "<tab>") 'company-indent-or-complete-common)
  ;; (add-hook 'helm-mode-hook
  ;;        (lambda ()
  ;;          (define-key evil-insert-state-local-map
  ;;                      (kbd "<tab>") 'indent-for-tab-command)))
  ;; (define-key evil-insert-state-map (kbd "<tab>") 'company-indent-or-complete-common)

  ;; Enable global mode.
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-quickhelp
  :ensure t
  :after (company)
  :config
  (company-quickhelp-mode))

;; NOTE company-tabnine is the trash.

(defun --->fold () "Fold text.")
(use-package hideshow
  :config
  ;; TIP Use 'zc' (fold-close) and 'zo' (fold-open).
  ;; TIP Use 'zm' (fold-more) and 'zr' (fold-reduce).
  ;; TIP Use 'zR' (fold-remove).
  ;; TIP Use 'za' (fold-toggle).

  ;; TIP You don't need the 'index-menu' if you have 'text-fold' function.
  (add-hook 'prog-mode 'hs-minor-mode))

(defun --->snippet () "Snippet text.")
(use-package yasnippet
  :ensure t
  :after (company)
  :hook (prog-mode . yas-minor-mode)
  :config
  ;; TIP Good to have a 'template' system to avoid stupid codes in some stupid languages. (I am not saying about Java).
  ;; TIP Use 'Tab' in `vi-insert-mode' to expand the key into snippet. e.g. 'cls<Tab>' in common-lisp mode.

  ;; Add company backend
  (add-to-list 'company-backends '(company-clang :with company-yasnippet))

  ;;TriggerKey
  ;; (evil-define-key 'insert yas-minor-mode-map (kbd "SPC") 'yas-expand)
  )

(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet)
  )

(defun --->checker () "Check text.")
;; NOTE The 'correctness' of 'flycheck' extension is much better than 'flymake'.
(use-package flycheck
  :ensure t
  :config
  ;; NOTE flycheck = the external executable 'checker' + the abstraction for 'error' object.

  ;; User options.
  (setq flycheck-display-errors-delay 2.0)

  ;; The fringe indicator is too tiny in hi-res mode:
  ;; - https://github.com/flycheck/flycheck/pull/1744/files
  ;; - https://emacs.stackexchange.com/questions/52829/fringe-indicators-very-tiny
  ;; - https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31203
  (setq-default left-fringe-width 16 right-fringe-width 16
                left-margin-width 0 right-margin-width 0)

  ;; Enable it.
  (global-flycheck-mode)

  ;; Bind keys.
  (evil-global-set-key 'normal (kbd "[ e") 'previous-error)
  (evil-global-set-key 'normal (kbd "] e") 'next-error))

;; Spell checker.
;; (use-package jinx
;;   :ensure t
;;   :config
;;   (jinx-mode))

(use-package eldoc
  :ensure nil
  :config
  ;; NOTE For `slime-autodoc', you need to load the lisp system first, to get the documentation.

  ;; Display the documentation instantly in echo area.
  (setq eldoc-idle-delay 0))

(use-package emacs
  :config
  ;; TIP Highlight the trailing whitespace.
  (add-hook 'text-mode-hook (lambda () (setq show-trailing-whitespace t)))
  (add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t))))

(defun --->formatter () "Format text.")
;; See https://www.gnu.org/software/emacs/manual/html_node/efaq/Changing-the-length-of-a-Tab.html
;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Display.html
;; TIP I don't care the `identation' (level of document) is represented by `spaces' or `tabs', but they should be rendered correctly, to be easy to read by human.
;; TIP Use 'formatter' to format buffer 'automatically', instead of `<<` and `>>` to indent text manually.
(use-package aggressive-indent
  :ensure t
  :config
  ;; NOTE Only enable this mode for these modes.
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'lisp-data-mode #'aggressive-indent-mode)
  ;; (add-hook 'c-mode-hook #'aggressive-indent-mode)
  ;; (add-hook 'java-mode-hook #'aggressive-indent-mode)
  (add-hook 'markdown-mode-hook #'aggressive-indent-mode)
  (add-hook 'tex-mode-hook #'aggressive-indent-mode)

  ;; NOTE Code is text that read by human, the indentation means the level of code, should be human-readable. (at least 4 spaces width, or just use tab character.)
  ;; NOTE The `indentation' should be enlarged, not highlighted.

  ;; Set indent-offset for human readable.
  (setq lisp-indent-offset nil))

(use-package editorconfig
  :ensure nil
  :config
  (editorconfig-mode))

;; NOTE The indent highlight is distracting, just use the auto formatter and the space characters width.
;; (use-package indent-bars
;;   :ensure t
;;   :hook ((emacs-lisp-mode lisp-mode) . indent-bars-mode)
;;   :config
;;   ;; (setq
;;   ;;  indent-bars-color '(highlight :face-bg t :blend 0.15)
;;   ;;  indent-bars-pattern "."
;;   ;;  indent-bars-width-frac 0.1
;;   ;;  indent-bars-pad-frac 0.1
;;   ;;  indent-bars-zigzag nil
;;   ;;  indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
;;   ;;  indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
;;   ;;  indent-bars-display-on-blank-lines t)
;;   ;; (setq
;;   ;;  ;; indent-bars-color '(ansi-color-white :face-bg t :blend 0.5)
;;   ;;  indent-bars-pattern "."
;;   ;;  indent-bars-width-frac 0.1
;;   ;;  indent-bars-pad-frac 0.1
;;   ;;  indent-bars-zigzag nil
;;   ;;  indent-bars-color-by-depth nil
;;   ;;  indent-bars-highlight-current-depth '(:face default :blend 0.4)
;;   ;;  indent-bars-display-on-blank-lines nil)
;;   ;; (setq
;;   ;;  indent-bars-pattern "."
;;   ;;  indent-bars-width-frac 0.1
;;   ;;  indent-bars-pad-frac 0.1
;;   ;;  indent-bars-color-by-depth nil
;;   ;;  indent-bars-display-on-blank-lines t
;;   ;;  indent-bars-highlight-current-depth '(:face default :blend 0.4))
;;   )

(defun --->text-object () "Analyse text.")
;; TIP Useful vi text-objects: 'b' = 'parenthesis', 'B' = 'curly', 't' = 'tag', 's' = 'sentence', 'a' = 'argument', 'f' = 'function', 'c' = 'class', 'o' = 'symbol'.
;; TIP To select cuurent function and jump between beginning and end: 'vifoo'
;; TIP It's very useful to use `vio' and `vib' in lisp family language.
;; TIP To list all possible `va{text-object}', see `evil-outer-text-objects-map'.


;; TIP The built-in package is named `treesit' package, use `{language}-ts-mode' to enable tree-sitter for the buffer.
(use-package tree-sitter
  ;; :disabled t
  :hook (lsp-mode . tree-sitter-mode)
  :init
  ;; NOTE Enable 'tree-sitter-mode' provided by 'tree-sitter.el' in Emacs v29.0. (Not use the 'treesit.el')
  ;; (global-tree-sitter-mode)
  )

;; (use-package treesit-auto
;;   :ensure t
;;   :config
;;   (global-treesit-auto-mode))

(use-package evil-textobj-tree-sitter
  :ensure t
  :after (tree-sitter)
  :config
  ;; TIP Extend the evil with custom `text-object' and `goto' based on tree-sitter is a killer-feature.
  ;; NOTE The evil-textobj-tree-sitter doesn't provide parsers for lisp-family languages in tree-sitter parsers.
  (define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))

  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))

  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer")))

(use-package highlight-thing
  :ensure t
  :config
  ;; TIP Auto highlight the thing at point.
  (global-highlight-thing-mode)

  ;; Set excluded modes.
  (add-to-list 'highlight-thing-excluded-major-modes 'pdf-view-mode)
  (add-to-list 'highlight-thing-excluded-major-modes 'vterm-mode)

  ;; Highlight the thing at point instantly. (It's more responsive.)
  (setq highlight-thing-delay-seconds 0) ;; Or 0.5

  ;; Ignore the case.
  (setq highlight-thing-case-sensitive-p nil)
  (set-face-attribute 'highlight nil
                      :background nil
		      :inverse-video t
		      :bold nil
		      :underline nil
                      ;; :underline '(:color "#00FF00"
                      ;;                     :style line)
		      ))

(defun --->comment () "Comment text.")
(use-package evil-nerd-commenter
  :ensure t
  :config
  ;; NOTE The `newcomment' package has bugs in evil-mode: it will also comment the next line.
  ;; TIP Only use the `line-comment' in all languages, the semantics of line-comment is better than `region-comment'.
  ;; TIP You can execute the commadn in `evil-visual-state', it worsk efficiently.
  (defun my/comment-or-uncomment-lines ()
    (interactive)
    (call-interactively 'evilnc-comment-or-uncomment-lines)
    (call-interactively 'my/save-buffer))

  (evil-define-key '(normal visual) 'global (kbd "SPC c") 'my/comment-or-uncomment-lines))

(defun --->parenthesis () "Parenthesis related.")
(use-package smartparens
  :ensure t
  ;; NOTE Only enable 'smartparens-mode' in these mode.
  :hook (prog-mode text-mode markdown-mode slime-repl-mode ielm-mode)
  :init

  ;; TIP Use 'closed-char' to 'move-over' the paird-structure.
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

  ;; (evil-define-key '(normal visual) 'global (kbd "[ s") 'evil-goto-last-change)
  ;; (evil-define-key '(normal visual) 'global (kbd "] s") 'evil-goto-last-change-reverse)

  ;; (evil-define-key '(normal insert visual) lisp-mode-map (kbd ")") 'sp-forward-sexp)
  ;; (evil-define-key '(normal insert visual) lisp-mode-map (kbd "(") 'sp-backward-sexp)
  :config
  ;; Load default config.
  (require 'smartparens-config)
  )

;; (use-package evil-smartparens
;;   :ensure t
;;   :config
;;   (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
;;   )

;; (use-package evil-cleverparens
;;   :ensure t
;;   :config
;;   (add-hook 'smartparens-enabled-hook #'evil-cleverparens-mode)
;;   )

(defun <utility> () "Utility tools in Emacs.")

;; NOTE Don't use un-stable applications, or you have to fix it yourself.
;; [Useful Tools]
;; Operation System: Arch Linux
;; Desktop Environment: KDE, x11
;; System: Alacritty, Spectacle, Wine
;; Input Method: fcitx
;; Encrypt: KeePassXC
;; Office: only-office, Acrobat Reader
;; Development: Emacs, Jetbrains Toolbox (DataGrip, IDEA, CLion, WebStorm), Qt Creator
;; Media: OBS Studio, VLC
;; Internet: Spicy, Chromium, Tor Browser, Wireshark.

(defun --->utility () "Minor utility tools.")
(use-package dictionary
  :init
  ;; TIP It's a convenient way to query the definition of a word.
  ;; TIP The `dictionary-search' will lookup the `symbol' at point.
  (evil-define-key '(normal) 'global (kbd "SPC u d") 'dictionary-search)
  :config
  ;; NOTE Always use a online dict server, instead of the offline server named `dictd'.
  ;; NOTE Other dict service https://www.collinsdictionary.com/jp/dictionary/english/
  (setq dictionary-server "dict.org"))

(use-package calendar
  :config
  (evil-define-key '(normal) 'global (kbd "SPC u c") 'calendar))

(use-package time
  :config
  ;; Set the world clocks.
  (setq world-clock-list
        '(("Asia/Tokyo" "Tokyo")
          ("Asia/Shanghai" "Beijing")
          ("America/Los_Angeles" "Seattle")
          ("America/New_York" "New York")
          ("Europe/London" "London")
          ("Europe/Paris" "Paris")
          ("Europe/Sofia" "Sofia")
          ("Asia/Calcutta" "Bangalore")))

  (evil-define-key '(normal) 'global (kbd "SPC u t") 'world-clock))

(use-package vterm
  :ensure t
  :after (evil hl-line)
  :commands (vterm)
  :init
  ;;; Keymap.
  ;; TIP The function to set window layout https://emacsredux.com/blog/2013/03/29/terminal-at-your-fingertips/
  (evil-define-key '(normal) 'global (kbd "SPC u s") 'vterm)
  :custom
  ;; TIP As a convention, the prefix key `C-c <key-to-send>' is used as `escape-sequence', to send next-key into libvterm directly.
  (vterm-keymap-exceptions '("C-c"))
  :config
  ;; TIP The libvterm supports interactive terminal programs like `Vim'. (The keys `input' and text `render' is also handled by Emacs)
  ;; TIP The `vterm' package is a wrapper for `libvterm.so', so it's possible to execute elisp forms inside `vterm-mode'.

  ;; NOTE The `evil-collection-vterm-setup' defines a minimal vim-emulator layer over the vterm, used to interact with the libvterm in evil-mode.
  ;; NOTE For some interactive terminal programs, like vim, you need to use emacs-state to input keys properly.

  ;; Set default shell program.
  (setq vterm-shell "/usr/bin/zsh")

  ;; Increase max lines of history.
  (setq vterm-max-scrollback 100000)

  ;; Close the vterm-buffer when the process is terminated.
  (setq vterm-kill-buffer-on-exit t)

  ;; Refresh the content of terminal without latency.
  (setq vterm-timer-delay nil)

  ;; Disable hl-line mode for vterm.
  ;; See https://emacsredux.com/blog/2020/11/21/disable-global-hl-line-mode-for-specific-modes/
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

  ;; Start with insert-state.
  (evil-set-initial-state 'vterm-mode 'insert)

  ;; Toggle between `Emacs' and `Vim' mode.
  ;; Set the default state to insert mode, since most of terminal programs are not interactive, so they work well in evil-insert-state.
  ;; If you want to run an interactive terminal program in vterm-buffer, you should toggle into emacs-state.
  (define-key vterm-mode-map (kbd "C-c z") 'evil-exit-emacs-state)
  (define-key vterm-mode-map (kbd "C-c M-x") 'helm-M-x)
  (define-key vterm-mode-map (kbd "C-c b") 'helm-mini)
  (define-key vterm-mode-map (kbd "C-c h") 'evil-window-left)
  (define-key vterm-mode-map (kbd "C-c j") 'evil-window-down)
  (define-key vterm-mode-map (kbd "C-c k") 'evil-window-up)
  (define-key vterm-mode-map (kbd "C-c l") 'evil-window-right)
  (define-key global-map (kbd "C-c 0") (lambda () (interactive) (tab-bar-switch-to-recent-tab)))
  (define-key global-map (kbd "C-c 1") (lambda () (interactive) (tab-select 1)))
  (define-key global-map (kbd "C-c 2") (lambda () (interactive) (tab-select 2)))
  (define-key global-map (kbd "C-c 3") (lambda () (interactive) (tab-select 3)))
  (define-key global-map (kbd "C-c 4") (lambda () (interactive) (tab-select 4)))
  (define-key global-map (kbd "C-c 5") (lambda () (interactive) (tab-select 5)))
  (define-key global-map (kbd "C-c 6") (lambda () (interactive) (tab-select 6)))
  (define-key global-map (kbd "C-c 7") (lambda () (interactive) (tab-select 7)))
  (define-key global-map (kbd "C-c 8") (lambda () (interactive) (tab-select 8)))
  (define-key global-map (kbd "C-c 9") (lambda () (interactive) (tab-select 9))))

(use-package eww
  :commands (eww-browse eww-browse-url eww-browse-url-new-window-is-tab)
  :init
  (setq browse-url-browser-function 'eww-browse-url)
  :config
  ;; TIP To browse the firefox, use 'vimium' extension. (It's convenient to read manual online.)

  (evil-define-key '(normal) eww-mode-map (kbd "SPC") nil)
  (evil-define-key '(normal) eww-mode-map (kbd "i") 'evil-insert-state))

;; TIP Alternatives: monkeytype, typit
(use-package speed-type
  :ensure t
  :init
  ;; TIP Typing-game is the most useful game.
  (evil-define-key '(normal) 'global (kbd "SPC u g") 'speed-type-text)
  :commands (speed-type-text))

(use-package emacs
  :ensure nil
  :config
  ;; TIP List the sub-processes of Emacs process.
  (evil-define-key '(normal) 'global (kbd "SPC q p") 'list-processes)
  )

(use-package webpaste
  :ensure t
  :config
  ;; Configure the service provider.
  (setq webpaste-provider-priority '("ix.io" "paste.rs" "dpaste.com" "dpaste.org" "paste.mozilla.org"
				     "paste.ubuntu.com" "gist.github.com" "bpa.st"))

  ;; Require confirmation for creating a paste.
  (setq webpaste-paste-confirmation t)

  ;; Keymap.
  (evil-define-key '(normal visual) 'global (kbd "SPC u p") 'webpaste-paste-buffer-or-region))

(defun --->customize () "The customize in Emacs.")
;; TIP Use 'customize' command to list the options provided by a package, and export them into '.emacs' later.
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))



(defun <language> () "Language related.")

(defun --->lsp () "Language Server Protocol.")
;; NOTE A LSP server provides: completion, snippet, index, documentation, cheker, refactor, code-action, formatter.
;; NOTE A good LSP server should provide the correct AST.
;; NOTE For IDE users, use this equation: IDE = Editor + LSP + DAP.

(use-package lsp-mode
  :ensure t
  :after (evil)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         ;; NOTE To let clangd indexing the project (or includes the proper header files.), you should let the compiler generate the compile flags file: cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 (or set the flag in CMakeList.txt file)
         ;; NOTE Since the AST is generated via compiling, so the pre-processor works for source file, be careful with the #ifdef macro!
         ;; https://clangd.llvm.org/config#files
	 ;; NOTE Specify `gcc' for `c' and `g++' for `cpp' in `Makefile'.
	 ;; NOTE The `lsp-mode' will identify the `c' or `cpp' by `file-extension' name.
         (c-mode . lsp)
         (c++-mode . lsp)
         (java-mode . lsp)
         (glsl-mode . lsp)

         ;; (yaml-ts-mode . lsp)
	 )
  :commands lsp
  :config
  ;; TIP Use `lsp-describe-session' command to check the abilities of the active lsp server.

  ;; Disable copilot server, it's too slow. (Use a specified pakcage for copilot instead.)
  (setq lsp-copilot-enabled nil)

  ;; Options
  ;; (push "--compile-commands-dir=./build" lsp-clients-clangd-args)

  ;; Bind find-references function.
  ;; TODO need to re-enter normal mode to apply the keymap.
  (evil-define-key 'normal lsp-mode-map (kbd "gr") 'lsp-find-references)
  (evil-define-key 'normal lsp-mode-map (kbd "ga") 'xref-apropos)

  ;; Bind document function.
  (evil-define-key 'normal lsp-mode-map (kbd "K") 'lsp-describe-thing-at-point)

  ;; Bind keys.
  (evil-define-key '(normal) 'global (kbd "SPC l w d") 'lsp-describe-session)
  (evil-define-key '(normal) 'global (kbd "SPC l w l") 'lsp-workspace-show-log)
  (evil-define-key '(normal) 'global (kbd "SPC l w R") 'lsp-workspace-restart)
  (evil-define-key '(normal) 'global (kbd "SPC l w K") 'lsp-workspace-shutdown)

  (evil-define-key '(normal) 'global (kbd "SPC l w w") 'lsp-workspace-folders-open)
  (evil-define-key '(normal) 'global (kbd "SPC l w a") 'lsp-workspace-folders-add)
  (evil-define-key '(normal) 'global (kbd "SPC l w r") 'lsp-workspace-folders-remove)
  (evil-define-key '(normal) 'global (kbd "SPC l w b") 'lsp-workspace-blocklist-remove)

  (evil-define-key '(normal) 'global (kbd "SPC l c a") 'helm-lsp-code-actions)

  (evil-define-key '(normal) 'global (kbd "SPC l f b") 'lsp-format-buffer)
  (evil-define-key '(normal) 'global (kbd "SPC l f r") 'lsp-format-region)

  (evil-define-key '(normal) 'global (kbd "SPC l r o") 'lsp-organize-imports)
  (evil-define-key '(normal) 'global (kbd "SPC l r n") 'lsp-rename))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list
  :config

  (setq lsp-treemacs-call-hierarchy-expand-depth 32)
  (setq lsp-treemacs-type-hierarchy-expand-depth 5)

  (setq lsp-treemacs-error-list-expand-depth 2))

;; NOTE The `dap-mode' package seems buggy, and IDE from jetbrains works perfect.
;; NOTE You can try other external gdb frontend, may be it will solve your problem.
;; TIP A better solution is to run `gdb --tui' in vterm.
(use-package dap-mode
  :ensure t
  ;; :hook (lsp-mode . ignore)
  ;; :commands (dap-register-debug-template)
  :config
  ;; TIP To configure the debugger: https://sourceware.org/gdb/current/onlinedocs/gdb.html/Debugger-Adapter-Protocol.html

  ;; Enable mode.
  (dap-mode 1)
  (setq dap-auto-configure-features '(sessions locals brekapoints expressions tooltip))

  ;; Add lldb debugger.
  (require 'dap-lldb)
  ;; NOTE Use (setq) instead of (push), or it will not work.
  (setq dap-lldb-debug-program '("/usr/bin/codelldb"))

  ;; Open hydra if breakpoint hit.
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

;; NOTE The `lsp-bridge' package has a poor integration with Emacs environment.
;; (use-package emacs
;;   :after (yasnippet)
;; :config
;; (add-to-list 'load-path "/home/sakurawald/.emacs.d/elpa/lsp-bridge")
;;   (require 'lsp-bridge)
;;   (global-lsp-bridge-mode))

(defun --->language:lisp () "Lisp Language.")

(use-package slime
  :ensure t
  :commands (slime-repl)
  :hook (lisp-mode . ignore)
  :config
  ;; TIP Some useful tips -> https://www.tumblr.com/slime-tips
  ;; NOTE The 'slime' env is composed by 'emacs' as the 'client-side', and 'swank' as the 'server-side'.
  ;; NOTE The 'slime-who-references', 'slime-who-binds' and 'slime-who-sets' only works for 'global-variable'.
  ;; NOTE The 'slime-who-calls', 'slime-calls-who', 'slime-list-callers' and 'slime-list-callees' only works for 'function' and 'method'.
  ;; NOTE Read more about xref in: https://slime.common-lisp.dev/doc/html/Xref-buffer-commands.html#Xref-buffer-commands
  ;; NOTE The 'semantic-identation' feature: https://slime.common-lisp.dev/doc/html/Semantic-indentation.html#Semantic-indentation
  ;; TIP The 'quote-form' is not counted as 'slime-edit-uses', like the 'make-instance'.
  ;; TIP To configure the `swank', see https://slime.common-lisp.dev/doc/html/Other-configurables.html#Other-configurables

  ;; Set the inferior-program.
  ;; TIP Put (sb-ext:set-sbcl-source-location "~/.roswell/src/sbcl-2.5.1") in ~/.roswell/init.lisp to set the src files of sbcl.
  (setq inferior-lisp-program "ros dynamic-space-size=4GiB run")

  ;; NOTE Use 'slime-setup' instead of `(setq slime-contribs '(slime-fancy))`.
  (slime-setup '(
                 slime-asdf
                 slime-quicklisp
                 ;; TIP The 'slime-fancy' contrib includes the following packages: slime-repl slime-autodoc slime-c-p-c slime-editing-commands slime-fancy-inspector slime-fancy-trace slime-fuzzy slime-mdot-fu slime-macrostep slime-presentations slime-scratch slime-references slime-package-fu slime-fontifying-fu slime-trace-dialog slime-indentation.
                 slime-fancy
                 slime-banner
                 slime-xref-browser
                 ;; slime-highlight-edits (this only works for compile)

                 slime-company
                 ))

  ;; Options for contribs.
  (setq slime-startup-animation nil)

  ;; Color the apropos command.
  (set-face-attribute 'slime-apropos-symbol nil
                      :foreground "#00FF00")

  ;; Start slime automatically if needed.
  (setq slime-auto-start 'always)
  ;; NOTE Auto execute 'slime' command.
  ;;(add-hook 'slime-mode-hook
  ;;            (lambda ()
  ;;              (unless (slime-connected-p)
  ;;            (save-excursion (slime)))))

  ;; Add font-lock in slime-repl mode.
  ;; https://stackoverflow.com/questions/25809493/how-can-i-get-syntax-highlighting-for-common-lisp-in-slimes-repl
  ;; https://comp.emacs.narkive.com/AWoywbFs/tweaking-slime
  (defun enable-slime-repl-font-lock ()
    (defvar slime-repl-font-lock-keywords lisp-font-lock-keywords-2)
    (defun slime-repl-font-lock-setup ()
      (setq font-lock-defaults '(slime-repl-font-lock-keywords
                                 ;; From lisp-mode.el
                                 nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
                                 (font-lock-syntactic-face-function
                                  . lisp-font-lock-syntactic-face-function))))

    (add-hook 'slime-repl-mode-hook 'slime-repl-font-lock-setup)

    (defadvice slime-repl-insert-prompt (after font-lock-face activate)
      (let ((inhibit-read-only t))
        (add-text-properties
         slime-repl-prompt-start-mark (1- (point))
         '(font-lock-face
           slime-repl-prompt-face
           rear-nonsticky
           (slime-repl-prompt read-only font-lock-face intangible))))))

  ;; Enable will remove the font-face of loggers.
  ;; (enable-slime-repl-font-lock)


  ;; Fix bindings.
  (evil-define-key '(normal) slime-mode-map "gr" 'slime-edit-uses)

  (evil-define-key '(insert) slime-repl-mode-map (kbd "C-j") 'slime-repl-next-input)
  (evil-define-key '(insert) slime-repl-mode-map (kbd "C-k") 'slime-repl-previous-input)
  ;; (evil-define-key 'normal slime-mode-map "C-m" 'slime-repl-return)
  )

(use-package slime-company
  :ensure t
  :after (slime company)
  :config

  ;; NOTE 'slime-company' must put after 'slime' to work.
  (setq slime-company-completion 'fuzzy)

  ;; Integrate with yasnippet.
  (advice-add 'slime-company-maybe-enable :after (lambda ()
                                                   (add-to-list 'company-backends '(company-slime :with company-yasnippet))))

  ;;(setq slime-company-after-completion nil)
  ;;(setq slime-company-after-completion 'slime-company-just-one-space)
  )

(defun --->repl () "Lisp repl.")
;; TIP To navigate 'input-history', use 'C-p' and 'C-n'.
;; TIP Use 'C-i' in repl to list all packages. (via 'slime-fuzzy-complete-symbol')
;; TIP Use 'C-u' (back to indentation) and 'C-w' (word) to delete backward in insert/ex/search vi-state.
;; TIP Use 'M-Ret' to 'close parens and return'.
;; TIP In `repl window`, you can mosue-click a `representation` to open `context-menu`.

;; NOTE If you start 'slime' via 'slime' command, then all the IO will be re-directed to the REPL by default. (https://slime.common-lisp.dev/doc/html/Global-IO-Redirection.html)
(evil-define-key '(normal) 'global (kbd "SPC r r") (lambda ()
                                                     (interactive)
                                                     (slime-repl)
                                                     (slime-restart-inferior-lisp)))
(evil-define-key '(normal) 'global (kbd "SPC r l") 'slime-list-connections)
(evil-define-key '(normal) 'global (kbd "SPC r t") 'slime-list-threads)

(evil-define-key '(normal) 'global (kbd "SPC r c") 'slime-repl-clear-buffer)

(evil-define-key '(normal) 'global (kbd "M-c") 'slime-repl-clear-buffer)

(defun --->evaluate () "Lisp evaluate.")

;; TIP See slime logs in 'slime-event-buffer'.

(defun emacs-lisp-mode-p ()
  "Is current buffer for emacs-lisp?"
  (or (eq 'emacs-lisp-mode major-mode)
      (eq 'inferior-emacs-lisp-mode major-mode)))

(defun switch-to-lisp-repl-buffer ()
  (interactive)
  "Switch to common-lisp or other lisp dialet repl buffer."
  (call-interactively 'slime-repl)
  (call-interactively 'evil-insert-state))

(defun switch-to-emacs-lisp-repl-buffer ()
  "Switch to emacs lisp repl buffer."
  (interactive)
  (let* ((name "*ielm*")
         (buf (get-buffer name)))
    (pop-to-buffer
     (if (bufferp buf)
         buf  ; Existing scratch buffer
       ;; New scratch buffer
       (with-current-buffer (ielm)
         (current-buffer))))

    ;; Goto the point-max position.
    (goto-char (point-max))
    (call-interactively 'evil-insert-state)))

(evil-define-key '(normal) 'global (kbd "SPC e w") (lambda ()
                                                     (interactive)
                                                     (if (emacs-lisp-mode-p)
                                                         (switch-to-emacs-lisp-repl-buffer)
                                                       (switch-to-lisp-repl-buffer))))
(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e c") (lambda ()
								 (interactive)
								 (call-interactively 'switch-to-lisp-repl-buffer)
								 (call-interactively 'slime-handle-repl-shortcut)))

;; NOTE We bind keys into `slime-autodoc-mode' minor mode, since this mode will be enabled in both `lisp-mode' and `slime-repl-mode'.
;; TIP Don't use `slime-repl-region`, use `eval-defun` to treat the `defun-like-form` as minimal unit.
(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e d") 'slime-eval-defun)
(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e b") 'slime-eval-buffer)
(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e s") 'slime-interactive-eval)
(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e e") 'slime-eval-last-expression)
(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e E") 'slime-eval-last-expression-in-repl)
(evil-define-key '(visual) 'slime-autodoc-mode (kbd "SPC e r") 'slime-eval-region)
;; TIP Use repl to 'resend' the last form to repl.
(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e R") (lambda ()
                                                                 (interactive)
                                                                 ;; The slime-repl-resend only works in slime-repl window.
                                                                 (call-interactively 'slime-repl)
                                                                 (call-interactively 'slime-repl-resend)))


(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e q") 'slime-repl-quicklisp-quickload)
(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e S") 'slime-load-system)

(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e D") 'slime-disassemble-symbol)

(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e p") (lambda ()
                                                                 (interactive)
                                                                 (call-interactively 'slime-sync-package-and-default-directory)
                                                                 (call-interactively 'slime-repl)
                                                                 (call-interactively 'evil-insert-state)))

(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e t") 'slime-toggle-trace-fdefinition)
(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e T") 'slime-trace-dialog)

;; NOTE Only the 'slime-compile-...' commands will add compiler notes. (The 'slime-eval-...' will not.)
(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e l") 'slime-list-compiler-notes)

;; NOTE The 'profile' should be done by automatical scripts.
;; TIP Use 'slime-toggle-profile-fdefinition' to profile a function, and 'slime-profile-package' to profile functions in a package.
(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e P") 'slime-profile-report)

(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e m m") 'slime-macroexpand-all)
(evil-define-key '(normal) 'slime-autodoc-mode (kbd "SPC e m M") 'slime-macroexpand-1)

(defun --->inspect () "Lisp inspector.")
;; v ---> verbose

;; h ---> history entries
;; RET ---> operate on point
;; > ---> fetch all
;; l ---> prev entry (left)
;; n ---> next entry
;; q ---> quit

;; M-RET ---> copy down to repl
;; g ---> re-inspect

;; p ---> pprint
;; d ---> describe
;; e ---> eval
;; . ---> show-source

;; TIP if there is no symbol under cursor, then the command will ask for a form to inspect.
(evil-define-key '(normal) 'global (kbd "SPC e i") (lambda (point)
                                                     (interactive "d")
						     (cl-multiple-value-bind (presentation start end whole-p)
							 (slime-presentation-around-or-before-point (point))
						       (if presentation
							   (call-interactively 'slime-inspect-presentation-at-point)
							 (call-interactively 'slime-inspect)))))

(evil-define-key '(normal) 'global (kbd "SPC e I") 'slime-interrupt)

;; Define keys for 'slime-inspector-mode' major-mode.
(evil-define-key '(normal) slime-inspector-mode-map (kbd "SPC v") 'slime-inspector-toggle-verbose)
(evil-define-key '(normal) slime-inspector-mode-map (kbd "SPC h") 'slime-inspector-history)
(evil-define-key '(normal) slime-inspector-mode-map (kbd "SPC >") 'slime-inspector-fetch-all)
(evil-define-key '(normal) slime-inspector-mode-map (kbd "SPC l") 'slime-inspector-pop)
(evil-define-key '(normal) slime-inspector-mode-map (kbd "SPC n") 'slime-inspector-next)
(evil-define-key '(normal) slime-inspector-mode-map (kbd "SPC q") 'slime-inspector-quit)

(evil-define-key '(normal) slime-inspector-mode-map (kbd "SPC d") 'slime-inspector-describe)
(evil-define-key '(normal) slime-inspector-mode-map (kbd "SPC g") 'slime-inspector-reinspect)

;; TIP Current inspected object is bound to `*' variable.
(evil-define-key '(normal) slime-inspector-mode-map (kbd "SPC e") 'slime-inspector-eval)
(evil-define-key '(normal) slime-inspector-mode-map (kbd "SPC p") 'slime-inspector-pprint)

;; NOTE This does the same functionality as `gd' command.
(evil-define-key '(normal) slime-inspector-mode-map (kbd "SPC .") 'slime-inspector-show-source)

(defun --->sldb () "Lisp debugger.")
;; NOTE The 'slime' use a 'custom-top-level': https://slime.common-lisp.dev/doc/html/Loading-Contribs.html#Loading-and-unloading-_0060_0060on-the-fly_0027_0027

;; n ---> down
;; p ---> up
;; TIP Use 'M-n' and 'M-p' to nagivate the `backtrace` with `source form`.
;; M-n ---> details down
;; M-p ---> details up

;; c ---> continue
;; a ---> abort
;; r ---> restart
;; 0..9 ---> invoke restart by number
;; I ---> invoke restart by name

;; v ---> frame source
;; d ---> eval in frame
;; e ---> inspect in frame

;; s ---> step
;; x ---> next
;; o ---> out
;; b ---> break on return
;; C ---> inspect condition
;; M-Ret ---> copy down to repl

(defun --->describe () "Lisp describe.")
;; NOTE the commands start with `describe-` is for `emacs lisp inferor`, and start with `slime-` is for `common lisp`.
;; TIP Use `K' key to query the manual under point.
(evil-define-key '(normal) 'global (kbd "SPC d d") 'slime-apropos-all)

;; NOTE The `slime-apropos` only list `external symbols`.
(evil-define-key '(normal) 'global (kbd "SPC d a") 'slime-apropos)
(evil-define-key '(normal) 'global (kbd "SPC d p") 'slime-apropos-package)

;; TIP Use 'gs' to goto the definition of a symbol.
(evil-define-key '(normal) 'global (kbd "SPC d s") 'slime-describe-symbol)
(evil-define-key '(normal) 'global (kbd "SPC d f") 'slime-describe-function)

;; NOTE Use `slime-browse-classes' to show the 'children' of the 'target class'.
(evil-define-key '(normal) 'global (kbd "SPC d c") 'slime-browse-classes)

;; TIP The command will ask for string if not string at point.
(evil-define-key '(normal) 'global (kbd "SPC d m") 'slime-documentation-lookup)
(evil-define-key '(normal) 'global (kbd "SPC d M") (lambda ()
                                                     (interactive)
                                                     (let ((browse-url-browser-function 'browse-url-default-browser))
                                                       (slime-documentation-lookup))))

(defun --->language:markdown () "Markdown language.")
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))


(defun --->language:latex () "LaTeX language.")
;; NOTE For latex language, use the built-in 'reftex' package.

(use-package auctex
  :ensure t
  :config
  ;; TIP The correct command is `LaTeX-mode'.

  )

(use-package company-auctex
  :ensure t
  :hook (tex-mode . ignore)
  :config
  (company-auctex-init))

(defun --->language:java () "Java language.")
(use-package lsp-java
  :ensure t
  :hook (java-mode . lsp)
  :after (lsp-mode)
  :config
  ;; Download a newer version jdtls server, to support Java 21.
  (setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.46.0/jdt-language-server-1.46.0-202503271314.tar.gz")

  ;; Choose a better decompiler. (FernFlower sucks)
  ;; TIP Use `cfr' decompiler saves your life: https://www.benf.org/other/cfr/
  ;; TIP The built-in `cfr' decompiler bundled with `jdtls' in `<jdtls-instllation-path>/bundles/dg.jdt.ls.decompiler.cfr-0.0.2-201802221740.jar' is old. You need to extract the .jar file first, and replace the old one with the latest one, and re-compress the files into a .jar file.
  (setq lsp-java-content-provider-preferred "cfr")

  ;; Set JVM args for jdtls.
  ;; Set vmargs for jdtls server. (Attach lombok java-agent.)
  (setq lsp-java-vmargs '(
			  ;; Add lombok support.
			  "-javaagent:/home/sakurawald/Programs/lombok/lombok.jar"
			  "-XX:+UseParallelGC" "-XX:GCTimeRatio=4"
			  "-XX:AdaptiveSizePolicyWeight=90"
			  "-Dsun.zip.disableMemoryMapping=true"
			  ;; Increase the memory to reduce lag.
			  "-Xmx8G"
			  "-Xms2G"))

  (setq lsp-java-completion-match-case "off")

  ;; Bind hook.
  (add-hook 'java-mode-hook #'lsp))

(defun java-bytecode-view ()
  "Run javap command to view the bytecode of a .class file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (error "Buffer is not visiting a file"))
    (let ((cmd (concat "javap -verbose -classpath " (java-get-jar-file-name)
		       " " (shell-quote-argument (file-name-base filename))))
	  (output-buffer (switch-to-buffer-other-window "*java-bytecode-viewer*")))
      (with-current-buffer output-buffer
	(setq-local buffer-read-only nil)
	(erase-buffer)
	(shell-command-to-string cmd)
	(insert (shell-command-to-string cmd))
	(goto-char (point-min))
	(setq-local buffer-read-only t)))))

(defun java-get-jar-file-name ()
  "Get .jar file name for current .java file in lsp-java mode."
  (interactive)
  (let ((buffer-file-directory (file-name-directory (buffer-file-name)))
	(buffer-uri lsp-buffer-uri)
	(magic-string nil))
    ;; Get .jar file name.
    (setf magic-string (s-match "%5C\\(.+?\\)=" buffer-uri))
    (if magic-string
	(setf magic-string (car magic-string)))
    (setf magic-string (s-replace "=" "" magic-string))
    (setf magic-string (s-replace "%5C" "" magic-string))

    ;; Return the .jar file name.
    magic-string))

(defun --->language:elisp () "Elisp language.")
;; Fix the xref backend function for elisp in ielm mode.
(use-package emacs
  :config
  ;; Disable output truncate.
  (setq eval-expression-print-length nil)

  ;; Set window rules for *ielm* buffer.
  (push '("*ielm*" display-buffer-pop-up-window)
        display-buffer-alist)

  ;; Set emacs source dir.
  (setq find-function-C-source-directory (expand-file-name "~/.cache/yay/emacs-git/src/emacs-git/src")))

(use-package ielm
  :config

  ;; Fix the xref backend functions for ielm mode.
  (add-hook 'ielm-mode-hook (lambda ()
                              (push 'elisp--xref-backend xref-backend-functions)))

  (add-hook 'apropos-mode-hook (lambda ()
                                 (push 'elisp--xref-backend xref-backend-functions)))

  ;; Evaluate.
  (evil-define-key '(normal) emacs-lisp-mode-map (kbd "SPC e d") 'eval-defun)
  (evil-define-key '(normal) emacs-lisp-mode-map (kbd "SPC e b") 'eval-buffer)
  (evil-define-key '(normal) emacs-lisp-mode-map (kbd "SPC e s") 'eval-expression)
  (evil-define-key '(normal) emacs-lisp-mode-map (kbd "SPC e e") 'eval-last-sexp)
  (evil-define-key '(normal) emacs-lisp-mode-map (kbd "SPC e E") 'eval-print-last-sexp)
  (evil-define-key '(visual) emacs-lisp-mode-map (kbd "SPC e r") 'eval-region)

  ;; REPL
  (evil-define-key '(insert) inferior-emacs-lisp-mode-map (kbd "C-j") 'comint-next-input)
  (evil-define-key '(insert) inferior-emacs-lisp-mode-map (kbd "C-k") 'comint-previous-input)

  ;; Key binding.
  (evil-define-key '(normal) 'global (kbd "SPC u e") 'switch-to-emacs-lisp-repl-buffer))

(defun --->language:pdf () "The pdf filetype.")
(use-package pdf-tools
  :ensure t
  :hook (doc-view-mode . pdf-view-mode)
  :config
  ;; TIP Need to install the `libpoppler' package to render the pdf document.
  ;; TIP The `pdf-tools' package supports the `pdfview' commands.
  ;; TIP Better to treat `pdf' documents as the `pure-text' document, which can enable the power of `isearch' and `occur'.
  ;; TIP Use `pdf-outline' command to get the TOC of the document.
  ;; TIP Use `right-click' on the pdf page to discover functions.
  ;; TIP Use `annotation' to make notes on pdf.
  ;; TIP Use `F' key to `follow' a link in pdf.


  (evil-define-key '(normal) pdf-view-mode-map (kbd "SPC") nil)
  (evil-define-key '(normal) pdf-view-mode-map (kbd "g o") 'pdf-occur)

  ;; (evil-define-key '(normal) pdf-view-mode-map (kbd "v") nil)
  (evil-define-key '(normal) pdf-occur-buffer-mode-map (kbd "SPC") nil)

  ;; Use `evil-search-forward' as the default search engine.
  (evil-define-key '(normal) pdf-occur-buffer-mode-map (kbd "/") nil)

  (evil-define-key '(normal) pdf-occur-buffer-mode-map (kbd "C-j") (lambda ()
								     (interactive)
								     (call-interactively 'evil-next-line)
								     ;; (call-interactively 'pdf-occur-goto-occurrence)
								     ))
  (evil-define-key '(normal) pdf-occur-buffer-mode-map (kbd "C-k") (lambda ()
								     (interactive)
								     (call-interactively 'evil-previous-line)
								     ;; (call-interactively 'pdf-occur-goto-occurrence)
								     ))

  (evil-define-key '(normal) pdf-outline-buffer-mode-map (kbd "SPC") nil)

  ;; Execute commands when open a pdf file.
  (add-hook 'pdf-view-mode-hook (lambda ()
				  (interactive)
				  (call-interactively 'pdf-view-fit-page-to-window)))

  ;; Document fold.
  (add-hook 'pdf-outline-buffer-mode-hook 'outline-minor-mode)

  ;; Document Imenu.
  (setq pdf-outline-enable-imenu t)

  ;; Document Annotation
  (add-hook 'pdf-view-mode-hook 'pdf-annot-minor-mode)
  (setq pdf-view-selection-style 'glyph)
  (add-hook 'activate-mark-hook (lambda ()
				  (when (and (eq major-mode 'pdf-view-mode)
					     (region-active-p)
					     mark-active)
				    (call-interactively 'pdf-annot-add-highlight-markup-annotation))))



  )

(defun --->language:glsl () "Glsl language.")
(use-package glsl-mode
  :ensure t
  :after (lsp-mode)
  :mode (("\\.glsl\\'" . glsl-mode)
	 ("\\.fs\\'" . glsl-mode)
	 ("\\.vs\\'" . glsl-mode)
	 ("\\.gs\\'" . glsl-mode)
	 ("\\.ts\\'" . glsl-mode)
	 ("\\.vert\\'" . glsl-mode)
	 ("\\.tesc\\'" . glsl-mode)
	 ("\\.tese\\'" . glsl-mode)
	 ("\\.geom\\'" . glsl-mode)
	 ("\\.comp\\'" . glsl-mode))
  :config
  (setf lsp-glsl-executable "glsl_analyzer")
  ;; (glsl-mode)
  )

(defun --->language:yaml () "Yaml language.")
(use-package yaml-ts-mode
  :ensure nil
  :mode (("\\.yaml\\'" . yaml-ts-mode)
	 ("\\.yml\\'" . yaml-ts-mode))
  :config)

(defun --->language:binary () "Binary language.")
;; TIP Use `hexl-mode' to edit any binary files.
;; TIP Use `elf-mode' to edit elf binary files.

(use-package elf-mode
  :ensure t
  :config
  )

(provide '.emacs)
;;; .emacs ends here

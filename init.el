(setq emacs-dir "~/.emacs.d/")
(defun in-emacs-d (path)
  (concat emacs-dir path))

(setq mode-dir (in-emacs-d "modes/"))
(add-to-list 'load-path mode-dir)

(let ((default-directory mode-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Custom file
(setq custom-file (in-emacs-d ".emacs-custom.el"))
(if (file-exists-p custom-file)
    (load-file custom-file))

(add-to-list 'custom-theme-load-path (in-emacs-d "themes"))
(load-theme 'tomorrow-night-bright t)

;; Misc
(setq inhibit-splash-screen t)

;; cua-selection-mode - enables typing over a region to replace it
(cua-selection-mode t)
(column-number-mode)

;; Window settings
(menu-bar-mode -1) ; get rid of the annoying menubars/toolbars etc.
(tool-bar-mode 0)
(scroll-bar-mode t)
(modify-all-frames-parameters '((scroll-bar-width . 10)))
(setq auto-window-vscroll nil)

;; Winner mode
(winner-mode 1)

;; Don't warn on some functions
(put	'downcase-region 'disabled nil)
(put	'upcase-region 'disabled nil)
(put	'narrow-to-region 'disabled nil)

;; Encoding
(prefer-coding-system		'utf-8)
(set-default-coding-systems	'utf-8)
(set-terminal-coding-system	'utf-8)
(set-keyboard-coding-system	'utf-8)

;; Auto-revert
(setq auto-revert-verbose nil)
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t)

;; Show-paren
(setq show-paren-delay 0)
(show-paren-mode)

;; Get rid of the secondary selection
(global-set-key [remap mouse-drag-secondary]		'mouse-drag-region)
(global-set-key [remap mouse-set-secondary]		'mouse-set-region)
(global-set-key [remap mouse-start-secondary]		'mouse-set-point)
(global-set-key [remap mouse-yank-secondary]		'mouse-yank-primary)
(global-set-key [remap mouse-secondary-save-then-kill]	'mouse-save-then-kill)

;; Smart-parens
(require 'smartparens)

(defun my/sp-sexp-replace-from-kill-ring (&rest args)
  "Replace current s-expression with the last copied string"
  (interactive)
  (sp-kill-sexp)
  (pop kill-ring)
  (save-excursion
    (insert (current-kill 1)))
  )

(savehist-mode 1)
(setq savehist-file (in-emacs-d ".savehist"))
(if (file-exists-p savehist-file)
    (load-file savehist-file))

;; Global Linum
(global-linum-mode)
(setq-default indicate-empty-lines t)

;; Usability
(fset 'yes-or-no-p 'y-or-n-p) ; yes/no turns to y/n

;; starting a daemon process
(setq server-socket-dir (in-emacs-d "server"))
(server-start)

;; C comment edit mode
(require 'c-comment-edit)

; drag stuff
(require 'drag-stuff)
(setq drag-stuff-modifier '(super control))
(drag-stuff-global-mode t)

(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

(defun my/compilation-hook ()
  (make-local-variable 'show-trailing-whitespace)
  (setq show-trailing-whitespace nil))
(add-hook 'compilation-mode-hook 'my/compilation-hook)

(require 'ansi-color)
(defun my/colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'my/colorize-compilation-buffer)

(recentf-mode 1)
(setq recentf-max-menu-items 1000)
(setq max-lisp-eval-depth 6000)

(setq auto-save-list-file-prefix nil)
(setq make-backup-files nil)

;;
;; Flycheck setup
;;

(require 'flycheck)

(flycheck-define-checker my/project-aware-checker
    "A syntax checker using make."
    :command ("~/.emacs.d/bin/project-aware-checker" source source-original)
    :error-patterns
    ((error line-start
	   (message "In file included from") " " (file-name) ":" line ":"
	   line-end)
    (info line-start (file-name) ":" line ":" column
	  ": note: " (message) line-end)
    (warning line-start (file-name) ":" line ":" column
	  ": warning: " (message) line-end)
    (error line-start (file-name) ":" line ":" column
	  ": " (or "fatal error" "error") ": " (message) line-end))
   :error-filter
   (lambda (errors)
     (flycheck-fold-include-levels
      (flycheck-sanitize-errors errors) "In file included from"))
   :modes (c-mode c++-mode))

(global-flycheck-mode)

(set-face-attribute 'flycheck-error nil :background "#990000")
(set-face-attribute 'flycheck-warning nil :background "#505000")

;; Haskell setup

(add-hook 'haskell-mode-hook 'my/haskell-mode-keys)
(add-hook 'haskell-cabal-mode-hook 'my/haskell-cabal-mode-keys)
(add-hook 'interactive-haskell-mode-hook 'my/interactive-haskell-mode-keys)

(load-file (in-emacs-d "my-haskell.el"))
(load-file (in-emacs-d "my-editing.el"))
(load-file (in-emacs-d "my-colors.el"))

;; Shell

(defun my/shell-script-mode-hook ()
  (setq indent-tabs-mode nil)
)
(add-hook 'shell-script-mode-hook 'my/shell-script-mode-hook)

;; Emacs 24 bugfix for face value after new-frame

(defun my/after-make-frame-hook (&rest frame)
  (interactive)
  (if window-system
      (let ((f (if (car frame)
		   (car frame)
		 (selected-frame))))
	(progn
	  (set-face-background 'cursor "#00ff00" f)
	  (set-face-foreground 'mode-line "#dedede" f)))))
(add-hook 'after-make-frame-functions 'my/after-make-frame-hook t)

(defun my/emacsclient-post-frame-fixups ()
  (my/after-make-frame-hook (selected-frame))
  ;; (load-file custom-file)
  ;; (my/reset-default-face-font-height)
  )

;; Helm

(require 'helm-config)
(require 'helm-ls-git)
(require 'helm-git-grep)
(require 'helm-proc)

(defun my/helm-find-files-navigate-forward (&rest args)
  (interactive)
  (if (file-directory-p (helm-get-selection))
      (helm-execute-persistent-action)
    (apply 'helm-maybe-exit-minibuffer args)))

(defun my/helm-find-files-navigate-back (&rest args)
  (interactive)
  (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
      (helm-find-files-up-one-level 1)
    (apply 'helm-ff-delete-char-backward args)))

;; Redisplay
(load-file (in-emacs-d "my-redisplay.el"))

;; C++/C
(defun my/c-mode-hook ()
;;  (whitespace-mode)
  (setq c-indent-level 8)
  (setq c-brace-imaginary-offset 0)
  (setq c-basic-offset 8)
  (setq c-brace-offset -8)
  (setq c-argdecl-indent 8)
  (setq c-label-offset -8)
  (setq c-continued-statement-offset 8)
  (setq indent-tabs-mode t)
  (linum-mode)
  (my/c-cc-mode-hook-set-keys)
  (setq tab-width 8))

(add-hook 'c-mode-hook 'my/c-mode-hook)

(defun my/c++-mode-hook ()
  (setq c-indent-level 4)
  (setq c-brace-imaginary-offset 0)
  (setq c-basic-offset 4)
  (setq c-brace-offset -4)

  (setq c-argdecl-indent 4)
  (setq c-label-offset -4)
  (setq c-continued-statement-offset 4)
  (setq indent-tabs-mode nil)
  (linum-mode)
  (my/c-cc-mode-hook-set-keys)
  (setq tab-width 4))
(add-hook 'c++-mode-hook 'my/c++-mode-hook)

;; Magit
(defun my/magit-show-diff-current-head ()
  (interactive)
  (magit-diff "HEAD~1" "HEAD")
)

(defun my/magit-show-diff-current-head-working-tree ()
  (interactive)
  (magit-diff-working-tree "HEAD")
)

(defun my/magit-log-branches ()
  (interactive)
  (magit-log-branches (quote ("--graph" "--decorate" "--color")))
)

(add-hook 'git-commit-mode-hook 'my/git-commit-mode-hook)
(require 'magit)

;; YASnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Diff HL
(require 'diff-hl)
(global-diff-hl-mode)

;; Smooth scrolling
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)

;; Buffer move
(require 'buffer-move)

;; Rust
(require 'rust-mode)
(require 'flycheck-rust)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; Go
(require 'go-mode)

;; D lang
(require 'd-mode)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

(defun my/d-mode-hook ()
  (setq c-indent-level 4)
  (setq c-brace-imaginary-offset 0)
  (setq c-basic-offset 4)
  (setq c-brace-offset -4)
  (setq c-argdecl-indent 4)
  (setq c-label-offset -4)
  (setq c-continued-statement-offset 4)
  (setq indent-tabs-mode nil)
  (linum-mode)
  (my/d-mode-hook-set-keys)
  (setq tab-width 4))

(add-hook 'd-mode-hook 'my/d-mode-hook)

;; Markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(defun my/markdown-mode-hook ()
  (interactive)

  (setq indent-tabs-mode nil)
  (electric-indent-local-mode -1)
  (my/markdown-mode-set-keys)
  (visual-line-mode)
  )

(add-hook 'markdown-mode-hook 'my/markdown-mode-hook)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Repository root
(require 'repository-root)
(add-to-list 'repository-root-matchers repository-root-matcher/git)

;; Sticky windows
(require 'sticky-windows)

(defun my/append-to-kill-ring (&optional arg)
  "Append to clipboard"
  (interactive "P")
  (append-next-kill)
  (kill-ring-save (mark) (point))
)

(defun my/open-repository-root-dir ()
  (interactive)
  (find-file (repository-root))
)

(defun my/generalized-shell-command (command arg) ;; From StackOverflow
  "Unifies `shell-command' and `shell-command-on-region'. If no region is
selected, run a shell command just like M-x shell-command (M-!).  If
no region is selected and an argument is a passed, run a shell command
and place its output after the mark as in C-u M-x `shell-command' (C-u
M-!).  If a region is selected pass the text of that region to the
shell and replace the text in that region with the output of the shell
command as in C-u M-x `shell-command-on-region' (C-u M-|). If a region
is selected AND an argument is passed (via C-u) send output to another
buffer instead of replacing the text in region."
  (interactive (list (read-from-minibuffer "Shell command: " nil nil nil 'shell-command-history)
                     current-prefix-arg))
  (let ((p (if mark-active (region-beginning) 0))
        (m (if mark-active (region-end) 0)))
    (if (= p m)
        ;; No active region
        (if (eq arg nil)
            (shell-command command)
          (shell-command command t))
      ;; Active region
      (if (eq arg nil)
          (shell-command-on-region p m command t t)
        (shell-command-on-region p m command)))))

;; Unbound
(require 'unbound)

;; Highlight-symbol
(require 'highlight-symbol)
(setq highlight-symbol-on-navigation-p t)

;; Dired list
(require 'dired-list)

;; Git grep
(defcustom git-grep-switches "--extended-regexp -I --no-color -n"
  "Switches to pass to `git grep'."
  :type 'string)

(require 'grep-a-lot)
(grep-a-lot-advise git-grep)

(defun git-grep (command-args)
  ;; Read command-args
  (interactive
   (let ((root (vc-git-root default-directory)))
     (if root
       (list
	  (read-shell-command
	   "Run git-grep (like this): "
	   (format (concat
		    "'\\b%s\\b'")
		   (let ((thing (and
				 buffer-file-name
				 (thing-at-point 'symbol))))
		     (or (and thing (progn
				      (set-text-properties 0 (length thing) nil thing)
				      (shell-quote-argument (regexp-quote thing))))
			 "")))
	   'git-grep-history))
       (list))))

   ;; Do the actual work
   (if command-args
     (let ((grep-use-null-device nil)
	   (root (vc-git-root default-directory)))
       (grep (format (concat "cd %s && git --no-pager grep %s -e %s") root git-grep-switches command-args)))
     (message "Not a git tree"))
  )

;;
;; Various functions
;;

(defun my/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun my/copy-projectile-buffer-file-name-to-kill-ring ()
  "Insert the curernt projectile-relative filename to the kill ring, so it
   can be pasted somewhere else"
  (interactive)

  (let ((a (projectile-project-root))
	(b (buffer-file-name)))
    (if (string-prefix-p a b)
	(progn
	  (kill-new (substring b (length a)))
	))
    )
  )

(defun my/auto-spell ()
  (interactive)
  (let ((x (point)))
    (mark-whole-buffer)
    (my/generalized-shell-command "auto-spell - -" nil)
    (goto-char x)
    )
)

(defun my/spawn-terminal-in-current-project-root ()
  "---"
  (interactive)
  (let (root)
    (setq root (projectile-project-root))
    (shell-command-to-string (concat "cd " root " && setsid dup >/dev/null 2>/dev/null &")))
)

(defun my/set-default-face-height (h)
  (set-face-attribute 'default nil :height h)
)

(setq my/toggle-default-face-font-height-large nil)

(defun my/reset-default-face-font-height ()
  (interactive)
  (if (eq my/toggle-default-face-font-height-large nil)
      (my/set-default-face-height 105)
      (my/set-default-face-height 150)
    )
  )

(defun my/toggle-default-face-font-height ()
  (interactive)
  (setq my/toggle-default-face-font-height-large (not my/toggle-default-face-font-height-large))
  (my/reset-default-face-font-height)
  )

(defun my/what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun my/ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

(defun my/git-comment-amend-no-questions ()
  (interactive)
  (shell-command "git commit --amend --no-edit --date=\"`date -R`\" -a")
  (magit-refresh-all)
  (diff-hl-update)
  )

(defun my/magit-log-new-frame ()
  (interactive)
  (make-frame-command)
  (magit-log)
  (delete-other-windows)
  )

(defun my/c-backslash-align ()
  (interactive)
  (let ((col (read-number "Column: ")))
    (setq c-backslash-max-column col)
    (call-interactively 'c-backslash-region)
    ))

(defun my/dired-list-git-ls-files (dir)
  "List all files in DIR managed by git and display results as a `dired' buffer."
  (interactive "Directory: ")
  (dired-list dir
              (concat "git ls-files " dir)
              (concat (in-emacs-d "bin/git-ls-files-long '") dir "'")))

(defun my/dired-list-git-ls-files-current-dir ()
  (interactive)
  (my/dired-list-git-ls-files (expand-file-name default-directory)))

(defun my/vc-visit-file-revision (file rev)
  "Visit revision REV of FILE in another window.
With prefix argument, uses the current window instead.
If the current file is named `F', the revision is named `F.~REV~'.
If `F.~REV~' already exists, use it instead of checking it out again."
  ;; based on `vc-revision-other-window'.
  (interactive
   (let ((file (expand-file-name
                (read-file-name
                 (if (buffer-file-name)
                     (format "File (%s): " (file-name-nondirectory
                                            (buffer-file-name)))
                   "File: ")))))
     (require 'vc)
     (unless (vc-backend file)
       (error "File %s is not under version control" file))
     (list file (vc-read-revision
                 "Revision to visit (default is working revision): "
                 (list file)))))
  (require 'vc)
  (unless (vc-backend file)
    (error "File %s is not under version control" file))
  (let ((revision (if (string-equal rev "")
                      (vc-working-revision file)
                    rev))
        (visit (if current-prefix-arg
                   'switch-to-buffer
                 'switch-to-buffer-other-window)))
    (funcall visit (vc-find-revision file revision))))

;; Projectile
(require 'projectile)
(setq projectile-indexing-method 'alien)
(projectile-global-mode)

(setq project-specifics-file (in-emacs-d "projects.el"))
(if (file-exists-p project-specifics-file)
    (load-file project-specifics-file))

;;
;; Various libraries
;;

(require 'dash)
(require 'helm-projectile)
(require 'align-by-current-symbol)
(require 'echo-keys)
(require 'goto-last-change)
(require 'track-mode)
(require 'wrap-region)
(require 'keep-formation)
(require 'lilypond-mode nil t)

;;

(wrap-region-add-wrappers
 '(("{-# " " #-}" "#" (haskell-mode))
   ("/* " " */" "/" (java-mode javascript-mode css-mode c-mode c++-mode))
   ("{- " " -}" "/" (haskell-mode))
   ("`" "`" nil (markdown-mode))))

(add-to-list 'wrap-region-except-modes 'magit-mode)
(wrap-region-global-mode)

;;
;; (read in Winston Churchill's voice):
;;
;;    When a cold sisyphean wind freezes the sweat over our foreheads, there will be
;;    only one sequence, 'Control-H Home' to bring us back and remind all keybindings,
;;    for all modes, at one place, to optimise the short-term actions for our long
;;    term goals.
;;

(defun my/visit-global-bindings-editor ()
  (interactive)
  (find-file (in-emacs-d "init.el"))
  (beginning-of-buffer)
  (search-forward ";; Global bindings")
  (goto-char (+ (point) 40))
  )
;;

;; Global bindings

(global-set-key [(control h) home]		'my/visit-global-bindings-editor)
(global-set-key (kbd "M-x")			'helm-M-x)

(global-set-key (kbd "C-;") (lambda () (interactive) (my/toggle-default-face-font-height)))
(global-set-key (kbd "C-v")			'yank)

;; Swap M-y and C-y
(global-set-key (kbd "M-y")			'yank)
(global-set-key (kbd "C-y")			'yank-pop)

(global-set-key (kbd "C-s-1")			'(lambda () (interactive) (workspace-goto ?1)))
(global-set-key (kbd "C-s-2")			'(lambda () (interactive) (workspace-goto ?2)))
(global-set-key (kbd "C-s-3")			'(lambda () (interactive) (workspace-goto ?3)))
(global-set-key (kbd "C-s-4")			'(lambda () (interactive) (workspace-goto ?4)))

(global-set-key (kbd "C-c <deletechar>")	'my/join-lines)

(define-key c-mode-map (kbd "C-c u")		'my/c-toggle-unused)
(define-key c-mode-map (kbd "C-c i")		'my/c-add-if)
(define-key c-mode-map (kbd "C-c f")		'my/c-add-for)
(define-key c-mode-map (kbd "C-c w")		'my/c-add-while)
(define-key c-mode-map (kbd "C-c s")		'my/c-add-switch)

(global-set-key [(control d)]			'highlight-symbol-prev)
(global-set-key [(control f)]			'highlight-symbol-next)

(global-set-key [(control b)]			'helm-projectile-switch-to-buffer)
(global-set-key [(control x) (control f)]	'helm-find-files)

(global-set-key (kbd "C-x <left>")		'diff-hl-previous-hunk)
(global-set-key (kbd "C-x <delete>")		'diff-hl-revert-hunk)
(global-set-key (kbd "C-x <right>")		'diff-hl-next-hunk)

(global-set-key (kbd "C-s-a")			'sp-beginning-of-sexp)
(global-set-key (kbd "C-s-d")			'sp-end-of-sexp)

(global-set-key (kbd "C-M-a")			'sp-backward-sexp)
(global-set-key (kbd "C-M-d")			'sp-forward-sexp)

(global-set-key (kbd "C-M-f")			'sp-down-sexp)
(global-set-key (kbd "C-M-b")			'sp-up-sexp)

(global-set-key (kbd "C-M-e")			'sp-backward-down-sexp)
(global-set-key (kbd "C-M-u")			'sp-backward-up-sexp)

(global-set-key (kbd "C-M-n")			'sp-next-sexp)
(global-set-key (kbd "C-M-p")			'sp-previous-sexp)
(global-set-key (kbd "C-M-k")			'sp-kill-sexp)
(global-set-key (kbd "C-M-w")			'sp-copy-sexp)

(global-set-key (kbd "M-<delete>")		'sp-splice-sexp)

(global-set-key (kbd "M-] ESC")			'projectile-project-buffers-other-buffer)
(global-set-key (kbd "M-] !")			'projectile-run-shell-command-in-root)
(global-set-key (kbd "M-] &")			'projectile-run-async-shell-command-in-root)

(global-set-key (kbd "M-] D")			'projectile-dired)
(global-set-key (kbd "M-] F")			'projectile-find-file-in-known-projects)
(global-set-key (kbd "M-] I")			'projectile-ibuffer)
(global-set-key (kbd "M-] P")			'projectile-test-project)
(global-set-key (kbd "M-] R")			'projectile-regenerate-tags)
(global-set-key (kbd "M-] S")			'projectile-save-project-buffers)
(global-set-key (kbd "M-] T")			'projectile-find-test-file)
(global-set-key (kbd "M-] a")			'projectile-find-other-file)
(global-set-key (kbd "M-] b")			'helm-projectile-switch-to-buffer)
(global-set-key (kbd "M-] c")			'projectile-compile-project)
(global-set-key (kbd "M-] d")			'helm-projectile-find-dir)
(global-set-key (kbd "M-] e")			'helm-projectile-recentf)
(global-set-key (kbd "M-] f")			'helm-projectile-find-file)
(global-set-key (kbd "M-] g")			'helm-projectile-find-file-dwim)
(global-set-key (kbd "M-] i")			'projectile-invalidate-cache)
(global-set-key (kbd "M-] j")			'projectile-find-tag)
(global-set-key (kbd "M-] k")			'projectile-kill-buffers)
(global-set-key (kbd "M-] l")			'projectile-find-file-in-directory)
(global-set-key (kbd "M-] m")			'projectile-commander)
(global-set-key (kbd "M-] o")			'projectile-multi-occur)
(global-set-key (kbd "M-] p")			'helm-projectile-switch-project)
(global-set-key (kbd "M-] r")			'projectile-replace)

(global-set-key (kbd "M-] t")			'projectile-toggle-between-implementation-and-test)
(global-set-key (kbd "M-] v")			'projectile-vc)
(global-set-key (kbd "M-] z")			'projectile-cache-current-file)

(global-set-key (kbd "M-] s g")			'projectile-grep)
(global-set-key (kbd "M-] s s")			'projectile-ag)
(global-set-key (kbd "M-] s f")			'my/copy-projectile-buffer-file-name-to-kill-ring)

(global-set-key (kbd "M-] 4 C-o")		'projectile-display-buffer)
(global-set-key (kbd "M-] 4 a")			'projectile-find-other-file-other-window)
(global-set-key (kbd "M-] 4 b")			'projectile-switch-to-buffer-other-window)
(global-set-key (kbd "M-] 4 d")			'projectile-find-dir-other-window)
(global-set-key (kbd "M-] 4 f")			'projectile-find-file-other-window)
(global-set-key (kbd "M-] 4 g")			'projectile-find-file-dwim-other-window)
(global-set-key (kbd "M-] 4 t")			'projectile-find-implementation-or-test-other-window)

(global-set-key [(control meta g)]		'my/kill-current-buffer)
(global-set-key [(meta g)]			'goto-line)

(global-set-key [(control f1)]			'ibuffer)
(global-set-key [(shift control f1)]		'recentf-open-files)

(global-set-key [(control f2)]			'dired-jump)

(global-set-key [(control prior)]		'sp-beginning-of-sexp)
(global-set-key [(control next)]		'sp-end-of-sexp)

(global-set-key [f3]				'my/spawn-terminal-in-current-project-root)

(global-set-key (kbd "C-x x")		        'new-frame)

(global-set-key (kbd "C-x v <up>")		'buf-move-up)
(global-set-key (kbd "C-x v <down>")		'buf-move-down)
(global-set-key (kbd "C-x v <right>")		'buf-move-right)
(global-set-key (kbd "C-x v <left>")		'buf-move-left)

(global-unset-key [(control meta r)])            ;; isearch-backward-regexp
(global-set-key   [(control meta r)]            'my/open-repository-root-dir)

(global-set-key (kbd "C-!")                     'delete-other-windows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; To quickly get into git context from editing context, this unbinds some
;; unused Emacs stuff, rebinds to Magit, and and reminds me to which git
;; shell commands it is equivalent.
;;
;; 'git status'
(global-unset-key	[(control n)]) ;; next-line
(global-set-key		[(control n)]  'magit-status)

;; 'git log HEAD --branches'
(global-unset-key	[(control q)]) ;; quoted-insert
(global-set-key		[(control q)]  'my/magit-log-branches)
;; [return]: To look at commits
;; P Q: push
;; r e: rebase interactive from here

;; 'git log' on the current file
(global-unset-key	(kbd "M-e"))   ;; forward-sentence
(global-set-key		(kbd "M-e")    'magit-log-buffer-file)

;; 'git branch'
(global-unset-key	(kbd "M-="))   ;; count-words-region
(global-set-key		(kbd "M-=")    'magit-branch-manager)
;; b c: create branch
;; b u: set upstream
;; k: delete branch
;; R: rename branch
;; r r: rebase

;; 'git diff' on uncommited changes
(global-unset-key	(kbd "M-a"))   ;; backward-sentence
(global-set-key		(kbd "M-a")    'my/magit-show-diff-current-head-working-tree)

;; quick amend
(global-set-key         (kbd "M-C-'")  'my/git-comment-amend-no-questions)

;; stuff that modify/fix the magit mode maps
(define-key             magit-status-mode-map   (kbd "M-i")  'magit-commit-amend)
(define-key             magit-mode-map       [C-tab] nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-unset-key	(kbd "M-'"))   ;; count-words-region
(global-unset-key	(kbd "M-c"))   ;; capitalize-word
(global-set-key         (kbd "M-c")    'helm-projectile-find-file-dwim)
(global-unset-key	(kbd "M-f"))   ;; forward-word
(global-set-key	        (kbd "M-f")    'my/sp-sexp-replace-from-kill-ring)
(global-unset-key	[(control p)]) ;; previous-line
(global-set-key		[(control p)]  'goto-last-change)
(global-unset-key	(kbd "C--"))   ;; negative-arugment
(global-unset-key	(kbd "C-/"))   ;; undo
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-/") 'yas-expand)
(global-unset-key	(kbd "C-\\"))  ;; toggle-input-mode
(global-set-key		(kbd "C-\\")   'sp-select-next-thing-exchange)
(global-set-key         (kbd "C-]")    'my/append-to-kill-ring)
(global-unset-key	(kbd "C-@"))   ;; set-mark-command
(global-unset-key	(kbd "C-_"))   ;; undo
(global-unset-key	(kbd "M-i"))   ;; tab-to-tab-stop
(global-set-key   	(kbd "M-i RET")  'replace-string)
(global-set-key   	(kbd "M-i -")  (lambda () (interactive) (my/replacer-save-str 'word)))
(global-set-key   	(kbd "M-i 0")  (lambda () (interactive) (my/replacer-save-str 'symbol)))
(global-set-key   	(kbd "M-i i")  'my/replacer-with-saved-str)
(global-unset-key	(kbd "M-k"))   ;; kill-sentence
(global-set-key		(kbd "M-k")    'highlight-symbol-query-replace)
(global-unset-key	(kbd "M-t"))   ;; transpose-words
(global-set-key		(kbd "M-t")    'query-replace)
(global-set-key		(kbd "C-S-t")  'transpose-words)
(global-unset-key	(kbd "M-v"))   ;; scroll-down-command
(global-set-key		(kbd "M-v")    'winner-undo)
(global-unset-key	(kbd "M-b"))   ;; backward-word
(global-set-key		(kbd "M-b")    'winner-redo)
(global-unset-key	(kbd "M-{"))   ;; backward-paragraph
(global-set-key		(kbd "M-{")    'sp-beginning-of-sexp)
(global-unset-key	(kbd "M-}"))   ;; forward-paragraph
(global-set-key		(kbd "M-}")    'sp-end-of-sexp)
(global-unset-key	(kbd "M-~"))   ;; not-modified
(global-set-key		(kbd "M-~")    'find-tag)
(global-unset-key	(kbd "M-SPC")) ;; just-one-space
(global-set-key         (kbd "M-SPC")  'my/dired-list-git-ls-files-current-dir)

(global-set-key		(kbd "S-<delete>") 'my/delete-region)
(global-set-key		(kbd "C-w") 'my/delete-region)

(define-key helm-find-files-map [return]	'my/helm-find-files-navigate-forward)
(define-key helm-find-files-map [backspace]	'my/helm-find-files-navigate-back)
(define-key helm-find-files-map [(control ?d)] (lambda () (interactive) (helm-select-nth-action 1)))

(defun my/git-commit-mode-hook ()
  (local-set-key [(control c) (v)] 'my/magit-show-diff-current-head)
)

(eval-after-load "magit"
  '(define-key magit-diff-mode-map (kbd "k") 'magit-revert-item))

(global-set-key [f4]			'next-error)
(global-set-key [(shift f4)]		'previous-error)
(global-set-key [(ctrl f4)]		'flycheck-first-error)
(global-set-key [(shift ctrl f4)]	'flycheck-list-errors)

(global-set-key [f5]			'switch-to-prev-buffer)
(global-set-key [(meta f5)]		'switch-to-next-buffer)
(global-set-key [(control f5)]		'ff-find-other-file)

(global-set-key [f6]			'helm-ls-git-ls)
(global-set-key [(shift f6)]		'helm-git-grep)
;; (global-set-key [f7]			'helm-find)

(global-set-key [(control tab)]		'other-window)
(global-set-key [(control z)]		'undo)

(global-set-key [(f9)]			'projectile-compile-project)

;; F9, Use to be compile, but who doesn't use project root
;; for invoking building commands these days?

(global-set-key [(meta f9)]		'recompile)
(global-set-key [(control f9)]		'git-grep)
(global-set-key [(shift control f9)]	'grep)

(global-set-key [f11]			'delete-window)
(global-set-key [f12]			'call-last-kbd-macro)

(defun my/c-cc-mode-hook-set-keys ()
  (local-set-key [return] 'newline-and-indent)
  (local-unset-key [(control d)])
  (local-unset-key [(meta e)])
  (local-unset-key [(meta a)])
)

(defun my/d-mode-hook-set-keys ()
  (local-set-key [return] 'newline-and-indent))

(defun my/grep-a-lot-setup-keys()
  "Define some key bindings for navigating multiple
grep search results buffers."
  (interactive)
  (global-set-key [(control h) left]		'grep-a-lot-goto-prev)
  (global-set-key [(control h) right]		'grep-a-lot-goto-next)
  (global-set-key [(control h) up]		'grep-a-lot-pop-stack)
  (global-set-key [(control h) down]		'grep-a-lot-clear-stack)
  (global-set-key [(control h) deletechar]	'grep-a-lot-restart-context)
  )

(my/grep-a-lot-setup-keys)

(global-set-key (kbd "M--")		'shrink-window)
(global-set-key (kbd "M-+")		'enlarge-window)

(global-set-key (kbd "M-<up>")		'windmove-up)
(global-set-key (kbd "M-<down>")	'windmove-down)
(global-set-key (kbd "M-<right>")	'windmove-right)
(global-set-key (kbd "M-<left>")	'windmove-left)

;; Haskell keys

(defun my/interactive-haskell-mode-keys ()
  (interactive)

  (define-key interactive-haskell-mode-map [f7]			'my/haskell-process-load-or-reload)
  (define-key interactive-haskell-mode-map [(control f12)]	'haskell-process-reload-devel-main)
  (define-key interactive-haskell-mode-map (kbd "C-`")		'haskell-interactive-bring)
  (define-key interactive-haskell-mode-map (kbd "C-c C-k")	'haskell-interactive-mode-clear)
  (define-key interactive-haskell-mode-map (kbd "C-c C-c")	'haskell-process-cabal-build)
  (define-key interactive-haskell-mode-map (kbd "C-c c")	'haskell-process-cabal)
  (define-key interactive-haskell-mode-map (kbd "M-.")		'haskell-mode-goto-loc)
  (define-key interactive-haskell-mode-map (kbd "C-c C-?")	'haskell-mode-find-uses)
  (define-key interactive-haskell-mode-map (kbd "C-c C-t")	'haskell-mode-show-type-at)
  (define-key interactive-haskell-mode-map (kbd "C-c t")	'haskell-mode-show-type-at)
  )

(defun my/haskell-mode-keys ()
  (interactive)
  (define-key haskell-mode-map (kbd "C-$")		'my/haskell-splice-with-dollar)
  (define-key haskell-mode-map (kbd "C-c i")		'hindent/reformat-decl)
  (define-key haskell-mode-map [f8]			'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "C-c C-u")		'haskell-insert-undefined)
  (define-key haskell-mode-map (kbd "C-c C-a")		'haskell-insert-doc)
  (define-key haskell-mode-map (kbd "C-<return>")	'haskell-simple-indent-newline-indent)
  (define-key haskell-mode-map (kbd "<space>")		'haskell-mode-contextual-space)
  (define-key haskell-mode-map (kbd "<delete>")         'keep-formation-delete-forward-char)
  (define-key haskell-mode-map (kbd "<backspace>")      'keep-formation-delete-backward-char)
  (keep-formation-mode)

  (define-key haskell-mode-map [(f4)]			'haskell-goto-next-error)
  (define-key haskell-mode-map [(shift f4)]		'haskell-goto-prev-error)
  (define-key haskell-mode-map [(ctrl f4)]		'my/haskell-goto-first-error)
  (define-key haskell-mode-map (kbd "C-x <up>")		'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "C-x <down>")	'haskell-navigate-imports-return)
  (define-key haskell-mode-map (kbd "C-,")        (lambda () (interactive) (haskell-move-nested-left 2)))
  (define-key haskell-mode-map (kbd "C-.")        (lambda () (interactive) (haskell-move-nested-right 2)))
  (define-key haskell-mode-map (kbd "C-x C-g .")	'isearch-forward-symbol-at-point)
  (define-key haskell-mode-map (kbd "C-x C-g _")	'isearch-forward-symbol)
  (define-key haskell-mode-map (kbd "C-x C-g w")	'isearch-forward-word)
  (define-key haskell-mode-map (kbd "C-c C-d")		'inferior-haskell-load-file)
  (define-key haskell-mode-map [(ctrl c) f5]		'haskell-mode-stylish-buffer)
;; (define-key haskell-mode-map (kbd "C-c C-c")    'my/run-ghc-compilation)
  )

(defun my/haskell-cabal-mode-keys ()
  (interactive)
  (define-key haskell-cabal-mode-map (kbd "C-`")	'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map [?\C-c ?\C-z]	'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c")	'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c")	'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k")	'haskell-interactive-mode-clear)
)

(defun my/markdown-mode-set-keys ()
  (interactive)

  (local-set-key (kbd "M-<up>")	        'windmove-up)
  (local-set-key (kbd "M-<down>")	'windmove-down)
  (local-set-key (kbd "M-<right>")	'windmove-right)
  (local-set-key (kbd "M-<left>")	'windmove-left)
  (local-set-key (kbd "C-M-<up>")       'markdown-move-up)
  (local-set-key (kbd "C-M-<down>")	'markdown-move-down)
  (local-set-key (kbd "C-M-<right>")	'markdown-demote)
  (local-set-key (kbd "C-M-<left>")	'markdown-promote)
  )

;; Instead of sp-up-sexp
(global-set-key [(control meta b)]		'helm-buffers-list)

;;
;; Mouse
;;

(global-set-key [(meta mouse-4)] 'drag-stuff-up)
(global-set-key [(meta mouse-5)] 'drag-stuff-down)

;;; init.el ends here

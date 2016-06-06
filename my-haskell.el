;;; package -- summary:
;;;
;;; Commentary:
;;;
;;; Based on stuff from https://github.com/Peaker/emacs.d/blob/master/chris-done-haskell.el
;;;

(require 'haskell)
(require 'company)
(require 'intero)
(require 'haskell-indentation)
(require 'haskell-font-lock)
(require 'haskell-mode)
(require 'haskell-process)
(require 'haskell-interactive-mode)
(require 'haskell-font-lock)

;;; Code:

(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.hsc\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
(require 'haskell-mode-autoloads)

(require 'hindent)

(defun my/haskell-sort-align-imports ()
  (interactive)
  (save-excursion
    (haskell-navigate-imports)
    (haskell-sort-imports)
    (haskell-align-imports)))

(defun my/haskell-process-all-types ()
  "List all types in a grep-mode buffer."
  (interactive)
  (let ((session (haskell-session)))
    (switch-to-buffer (get-buffer-create (format "*%s:all-types*"
                                                 (haskell-session-name (haskell-session)))))
    (setq haskell-session session)
    (cd (haskell-session-current-dir session))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((haskell-process-log nil))
        (insert (haskell-process-queue-sync-request (haskell-process) ":all-types")))
      (unless (eq major-mode  'compilation-mode)
        (compilation-mode)
        (setq compilation-error-regexp-alist
              haskell-compilation-error-regexp-alist)))))

(defun my/haskell-process-load-or-reload ()
  (interactive)
  (haskell-process-load-or-reload)
  )

(defun my/haskell-interactive-toggle-print-mode ()
  (interactive)
  (setq haskell-interactive-mode-eval-mode
        (intern
         (ido-completing-read "Eval result mode: "
                              '("fundamental-mode"
                                "haskell-mode"
                                "espresso-mode"
                                "ghc-core-mode"
                                "org-mode")))))

(defun my/haskell-insert-doc ()
  "Insert the documentation syntax."
  (interactive)
  (insert "-- | "))

(defun my/haskell-move-right ()
  (interactive)
  (haskell-move-nested 1))

(defun my/haskell-move-left ()
  (interactive)
  (haskell-move-nested -1))

(defun my/haskell-goto-first-error ()
  (interactive)
  (haskell-goto-error-overlay
   (first-overlay-in-if 'haskell-check-overlay-p
			(buffer-end 0) (buffer-end 1))))

(custom-set-variables
 '(haskell-process-args-cabal-repl
   '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng"))

 '(haskell-indent-spaces 4)
 '(haskell-indentation-ifte-offset 4)
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)
 '(haskell-indentation-starter-offset 4)
 '(haskell-indentation-where-post-offset 4)
 '(haskell-indentation-where-pre-offset 4)
 '(haskell-interactive-mode-eval-mode 'haskell-mode)
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-notify-p t)
 '(haskell-process-args-ghci '("-ferror-spans"))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-generate-tags nil)
 '(haskell-process-log t)
 '(haskell-process-path-ghci "ghci-ng")
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-suggest-haskell-docs-imports nil)
 '(haskell-process-suggest-hayoo-imports nil)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote stack-ghci))
 '(haskell-process-use-presentation-mode t)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save nil)
 '(hindent-style "chris-done")
 '(haskell-process-args-cabal-repl
   '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng"))
 '(haskell-process-args-stack-ghci
   '("--ghc-options=\"-ferror-spans\"" "--with-ghc=ghci-ng"))
 '(haskell-process-generate-tags nil)
 '(haskell-complete-module-preferred
   '("Data.ByteString"
     "Data.ByteString.Lazy"
     "Data.Conduit"
     "Data.Function"
     "Data.List"
     "Data.Map"
     "Data.Maybe"
     "Data.Monoid"
     "Data.Ord")))

(require 'align)
(add-to-list 'align-rules-list
	     '(haskell-types
	       (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
	       (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
	     '(haskell-assignment
	       (regexp . "\\(\\s-+\\)=\\s-+")
	       (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
	     '(haskell-arrows
	       (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
	       (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
	     '(haskell-left-arrows
	       (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
	       (modes quote (haskell-mode literate-haskell-mode))))

(defun my/haskell-cabal-mode-hook ()
  (setq indent-tabs-mode nil)
)
(add-hook 'haskell-cabal-mode-hook 'my/haskell-cabal-mode-hook)


(defun my/haskell-splice-with-dollar ()
  "Turn (...) to $ ..."
  (interactive)
  (insert "$ ")
  (goto-char (+ (point) 1))
  (sp-splice-sexp)
  )

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
(add-hook 'haskell-mode-hook 'intero-mode)
(add-hook 'haskell-mode-hook 'company-mode)


(provide 'my-haskell)
;;; my-haskell.el ends here

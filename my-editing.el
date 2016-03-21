;;; package -- summary:
;;;
;;; Commentary:
;;;
;;; Code:

(defun my/c-instantiate-vtable ()
  "---"
  (interactive)
  (let (orig pt tmp start end struct-name functions prefix var-name)
    (setq orig (point))
    (search-backward "{")
    (setq pt (point))
    (beginning-of-line)
    (setq tmp (buffer-substring-no-properties pt (point)))
    (setq functions '())

    (if (string-match "struct \\([a-zA-Z0-9_]+\\) " tmp)
	(let ()
	  (setq struct-name (match-string 1 tmp))
	  (setq start (point))
	  (search-forward "}")
	  (setq end (point))
	  (goto-char start)
	  (while (search-forward "(*" end t)
	    (let (funcname funcproto start-proto)
	      (setq tmp (point))
	      (if (search-forward ")(" end t)
		  (let (a)
		    (setq funcname (buffer-substring-no-properties tmp (- (point) 2)))
		    (save-excursion
		      (beginning-of-line)
		      (setq start-proto (point))
		      (search-forward ";" end)
		      (setq funcproto (buffer-substring-no-properties start-proto (- (point) 1)))
		      )
		    (push (list funcname funcproto) functions)
		    ))
	    )
	  )
	  (setq var-name (read-string "Variable: "))
	  (setq prefix (read-string "Prefix: "))

	  (delete-region start end)
	  (mapc (lambda (f)
	     (let ((funcname (nth 0 f))
		   (funcproto (nth 1 f)))
	       (setq funcproto (replace-regexp-in-string "([*]" (concat prefix "_") funcproto))
	       (setq funcproto (replace-regexp-in-string ")(" "(" funcproto))
	       (setq funcproto
		     (if (string-match "\\`[ \t\n\r]+" funcproto)
			 (replace-match "" t t funcproto) funcproto))
	       (insert funcproto)
	       (newline)
	       (insert "{")
	       (newline)
	       (insert "}")
	       (newline)
	       (newline)
	     ))
	     functions)
	  (insert "struct " struct-name " " var-name " = {")
	  (mapc (lambda (f)
	     (let ((funcname (nth 0 f))
		   (funcproto (nth 1 f)))
	       (newline-and-indent)
	       (insert "." funcname " = " prefix "_" funcname ",")
	     ))
	     functions)
	  (newline)
	  (insert "}")
	))
  )
)

(defun my/c-prototype-comment ()
  "---"
  (interactive)
  (let (orig pt funcname end params i)
    (setq orig (point))
    (setq params '())
    (search-forward "(")
    (setq pt (point))
    (save-excursion
      (search-backward " ")
      (setq funcname (buffer-substring-no-properties (+ (point) 1) (- pt 1))))
    (save-excursion
      (search-forward ")")
      (setq end (point)))
    (beginning-of-line)
    (save-excursion
      (catch 'foo
	(while (< (point) end)
	  (if (not (search-forward "," end t))
	      (if (not (search-forward ")" end t))
		  (throw 'foo t)))
	  (save-excursion
	    (backward-char)
	    (let ((start (point)))
	      (search-backward-regexp "[^a-zA-Z0-9_]")
	      (forward-char)
	      (push (buffer-substring-no-properties (point) start) params)
	      )
	    ))))
    (insert "\n")
    (insert "/**\n")
    (insert " * " funcname "()\n")
    (insert " *\n")
    (dolist (i (reverse params))
      (insert " * @param " i "\n"))
    (insert " *\n")
    (insert " * \n")
    (insert " *\n")
    (insert " * @returns\n")
    (insert " */\n")
  )
)

(defun my/c-toggle-unused ()
  "Adds or removes the UNUSED() on the identifier at point."
  (interactive)
  (let (s e)
    (save-excursion
      (save-excursion
        (setq s (search-backward-regexp "[^a-zA-Z0-9_\(\)]"))
        )
      (message (buffer-substring-no-properties (+ s 1) (+ s 8)))
      (if (string= (buffer-substring-no-properties (+ s 1) (+ s 8))
              "UNUSED(")
          (progn
            (delete-region (+ s 1) (+ s 8))
            (search-forward-regexp "[\)]")
            (goto-char (- (point) 1))
            (delete-char 1)
            )
          (progn
            (setq s (search-backward-regexp "[^a-zA-Z0-9_]"))
            (goto-char (+ s 1))
            (setq e (search-forward-regexp "[^a-zA-Z0-9_]"))
            (goto-char (+ s 1))
            (insert "UNUSED(")
            (goto-char (+ e 6))
            (insert ")")
            )
        )
      )
    )
  )

(defun my/c-add-block (keyword)
  (let (s e l)
    (setq l (string-width keyword))
	(progn
	  (save-excursion
	    (if (use-region-p)
	      (progn
		  (setq s (region-beginning))
		  (setq e (region-end)))
	      (progn
		  (setq s (point))
		  (setq e (point))))
	    (deactivate-mark)
	    (goto-char e)
	    (insert "\n}")
	    (if (not (use-region-p))
		(insert "\n"))
	    (goto-char s)
	    (insert (concat keyword " () {\n"))
	    )
	  (indent-region s (+ e l 8))
	  (goto-char (+ s l 2))
	  )
      )
    )

(defun my/c-add-if ()
  (interactive)
  (my/c-add-block "if"))

(defun my/c-add-while ()
  (interactive)
  (my/c-add-block "while"))

(defun my/c-add-for ()
  (interactive)
  (my/c-add-block "for"))

(defun my/c-add-switch ()
  (interactive)
  (my/c-add-block "switch"))

(defun my/align-c-function-parameters ()
  (interactive)
  (unless (region-active-p) (error "No region selected"))
  (align-regexp (region-beginning) (region-end) "[,(]\\(\\s-*\\)" 1 1 t)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\()[ \t]*;\\)" 1 1 t)
  )

(require 'thingatpt)

(defvar my/replacer-saved-str)
(defvar my/replacer-history nil)

(defun my/replacer-save-str (THING)
  "Remember the symbol at point as the THING to replace."

  (interactive)
  (let (s)
    (progn
      (setq s (thing-at-point THING))
      (setq my/replacer-saved-str s)
      (message (format "Saved replace string '%s'" s))
      ))
  )

(defun my/replacer-with-saved-str ()
  "Automatically replace the remembered string with the provided string,
   either in the marked region or the whole buffer"

  (interactive)
  (let (b e s)
    (progn
      (if (and transient-mark-mode mark-active)
	  (progn
	    (setq b (region-beginning))
	    (setq e (region-end))
	    (setq s "region")
	    )
	  (progn
	    (setq b (point-min))
	    (setq e (point-max))
	    (setq s "WHOLE BUFFER")
	    ))

      (let ((q (read-from-minibuffer
		(format "Replacing in %s, '%s' with: " s my/replacer-saved-str)
		nil nil nil
		my/replacer-history "" t)))
	(save-excursion
	  (goto-char b)
	  (while (and (re-search-forward my/replacer-saved-str nil t)
		      (< (point) e))
	    (replace-match q nil nil)
	    (setq e (+ e (- (length q) (length my/replacer-saved-str))))
	    ))
	))
  )
  )

(defun my/join-lines ()
  (interactive "")

  (end-of-line)
  (delete-char 1)
  (delete-horizontal-space)
  (insert " ")
  )

(provide 'my-editing)
;;; my-editing.el ends here

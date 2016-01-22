;;

(require 'hexrgb)
(defun my/far-color (hex)
  (let (components r g b)
    (setq components (hexrgb-hex-to-color-values hex))
    (setq r (mod (+ (nth 0 components) 128) 256))
    (setq g (mod (+ (nth 1 components) 128) 256))
    (setq b (mod (+ (nth 2 components) 128) 256))
    (hexrgb-color-values-to-hex (list r g b) 2)
  ))

;; Taken from: http://ergoemacs.org/emacs/elisp_eval_lisp_code.html
(defun my/xah-syntax-color-hex ()
  "Syntax color hex color spec such as 「#ff1100」 in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("\"\#[abcdef[:digit:]]\\{6\\}\""
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (substring (match-string-no-properties 0) 1 8)
	              :foreground (my/far-color (substring (match-string-no-properties 0) 1 8))))))))
  (font-lock-fontify-buffer)
  )


(require 'color)
(require 'rainbow-delimiters)

(defun my/gen-col-list (length s v &optional hval)
  (cl-flet ( (random-float () (/ (random 10000000000) 10000000000.0))
          (mod-float (f) (- f (ffloor f))) )
    (unless hval
      (setq hval (random-float)))
    (let ((golden-ratio-conjugate (/ (- (sqrt 5) 1) 2))
          (h hval)
          (current length)
          (ret-list '()))
      (while (> current 0)
        (setq ret-list
              (append ret-list
                      (list (apply 'color-rgb-to-hex (color-hsl-to-rgb h s v)))))
        (setq h (mod-float (+ h golden-ratio-conjugate)))
        (setq current (- current 1)))
      ret-list)))

(set-face-foreground 'rainbow-delimiters-depth-1-face "#ffffff" )
(set-face-foreground 'rainbow-delimiters-depth-2-face "#bbcc88" )
(set-face-foreground 'rainbow-delimiters-depth-3-face "#bb88cc" )
(set-face-foreground 'rainbow-delimiters-depth-4-face "#88ccbb" )
(set-face-foreground 'rainbow-delimiters-depth-5-face "#cc88bb" )
(set-face-foreground 'rainbow-delimiters-depth-6-face "#ccbb88" )
(set-face-foreground 'rainbow-delimiters-depth-7-face "#88bb88" )
(set-face-foreground 'rainbow-delimiters-depth-8-face "#8888bb" )
(set-face-foreground 'rainbow-delimiters-depth-9-face "#bbcc88" )

;;;

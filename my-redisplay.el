;;; package -- summary:
;;;
;;; Commentary:
;;;
;;; Code:

;; Overlay highlighting

(defface lawlist-active-region-face
  '((t (:background "#3c3c3c")))
  "Face for `lawlist-active-region-face`."
  :group 'init)

(defvar lawlist-redisplay-unhighlight-region-function
  (lambda (rol) (when (overlayp rol) (delete-overlay rol))))

(defvar lawlist-redisplay-highlight-region-function
  (lambda (start end window rol)
    (if (not (overlayp rol))
	(let ((nrol (make-overlay start end)))
	  (funcall lawlist-redisplay-unhighlight-region-function rol)
	  (overlay-put nrol 'window window)
	  (overlay-put nrol 'face 'lawlist-active-region-face)
	  (overlay-put nrol 'priority '(10000 . 100))
	  nrol)
      (unless (and (eq (overlay-buffer rol) (current-buffer))
		   (eq (overlay-start rol) start)
		   (eq (overlay-end rol) end))
	(move-overlay rol start end (current-buffer)))
      rol)))

(defun lawlist-redisplay--update-region-highlight (window)
  (with-current-buffer (window-buffer window)
    (let ((rol (window-parameter window 'internal-region-overlay)))
      (if (not (region-active-p))
	  (funcall lawlist-redisplay-unhighlight-region-function rol)
	(let* ((pt (window-point window))
	       (mark (mark))
	       (start (min pt mark))
	       (end   (max pt mark))
	       (new
		(funcall lawlist-redisplay-highlight-region-function
			 start end window rol)))
	  (unless (equal new rol)
	    (set-window-parameter window 'internal-region-overlay
				  new)))))))

(defun lawlist-redisplay--update-region-highlights (windows)
  (with-demoted-errors "lawlist-redisplay--update-region-highlights: %S"
    (if (null windows)
	(lawlist-redisplay--update-region-highlight (selected-window))
      (unless (listp windows) (setq windows (window-list-1 nil nil t)))
      (if highlight-nonselected-windows
	  (mapc #'lawlist-redisplay--update-region-highlight windows)
	(let ((msw (and (window-minibuffer-p) (minibuffer-selected-window))))
	  (dolist (w windows)
	    (if (or (eq w (selected-window)) (eq w msw))
		(lawlist-redisplay--update-region-highlight w)
	      (funcall lawlist-redisplay-unhighlight-region-function
		       (window-parameter w 'internal-region-overlay)))))))))

;; simple.el -- lines 4683 to 4684
(remove-function pre-redisplay-function #'redisplay--update-region-highlights)

(add-function :before pre-redisplay-function #'lawlist-redisplay--update-region-highlights)

(provide 'my-redisplay)
;;; my-redisplay.el ends here

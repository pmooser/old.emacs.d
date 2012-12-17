;; paul-utilities.el 

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; (defun indent-whole-buffer ()
;;   "Re-indent the whole buffer."
;;   (interactive)
;;   (save-excursion
;;     (mark-whole-buffer)
;;     (indent-region (region-beginning) (region-end))))

(defun toggle-comment-region-or-line ()
  "Toggle comment on either region or line."
  (interactive)
  (save-excursion
    (if mark-active
        (comment-or-uncomment-region (region-beginning) (region-end))
      (progn
        (back-to-indentation)
        (push-mark)
        (end-of-line)
        (comment-or-uncomment-region (mark) (point)))))
  (next-line))
    
;; rotate a list 
(defun rotate (elements n)
  (let* ((elements-length (length elements))
         (tail-length (mod n elements-length)))
    (if (zerop tail-length)
	elements
      (progn
	(append (last elements tail-length)
		(butlast elements tail-length))))))

;; window movement convenience tricks
(defun find-sublist-with-element (elem lst)
  (if (atom lst) nil
    (if (memq elem lst) lst
      (or (find-sublist-with-element elem (car lst))
          (find-sublist-with-element elem (cdr lst))))))

(defun select-window-right ()
  "Select the window to the right of the current window."
  (interactive)
  (let* ((window (selected-window))
         (neighbors (find-sublist-with-element window (window-tree)))
         (pos (position window neighbors)))
    (if (not (car neighbors))
        (select-window (or (nth (+ 1 pos) neighbors) (nth 2 neighbors))))))

(defun select-window-left ()
  "Select the window to the left of the current window."
  (interactive)
  (let* ((window (selected-window))
         (neighbors (find-sublist-with-element window (window-tree)))
         (neighblen (length neighbors))
         (pos (position window neighbors)))
    (if (not (car neighbors))
        (select-window 
         (cond ((equal pos 2) (car (last neighbors)))
               (t (nth (- pos 1) neighbors)))))))

(defun select-window-above ()
  "Select the window above the current window."
  (interactive)
  (other-window -1))

(defun select-window-below ()
  "Select the window below the current window."
  (interactive)
  (other-window 1))

;; non-destructive merge function
;; a and b are assumed to be sorted lists, and
;; p is a predicate used for comparison

(defun my-merge (a b p)

  (cond ((null a) b)
	((null b) a)
	;; else
	(t (let ((outlist '()))
	     
	     (while (and a b)
	       (if (funcall p (car a) (car b))
		   (progn
		     (push (car a) outlist)
		     (setq a (cdr a)))
		 (progn
		   (push (car b) outlist)
		   (setq b (cdr b)))))
		 
	     (if (not a)
		 (reverse (append (reverse b) outlist))
	       (reverse (append (reverse a) outlist)))))))

(provide 'paul-utilities)


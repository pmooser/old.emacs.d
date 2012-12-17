;; My own customizations for ediff

(require 'ediff)

(defun new-frame-with-focus ()
  (let ((frame (new-frame)))
    (select-frame-set-input-focus frame)
    frame))

(defun delete-frame-if-not-last ()
  (if (> (length (frame-list)) 1)
      (delete-frame)))

(add-hook 'ediff-before-setup-hook 'new-frame-with-focus)
(add-hook 'ediff-quit-hook 'delete-frame-if-not-last)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(provide 'paul-ediff)

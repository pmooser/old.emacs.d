;; My collection of advice for various functions

(defconst paul-indent-modes
  '(emacs-lisp-mode
    clojure-mode
    haskell-mode
    lisp-interaction-mode
    lisp-mode
    c-mode
    c++-mode
    objc-mode
    ruby-mode
    java-mode))

;; auto-indent when pasting in certain modes
(defadvice yank (after indent-yank activate)
  (if (member major-mode paul-indent-modes)
      (let ((transient-mark-mode nil))
	(indent-region (region-beginning) (region-end) nil))))

;; make sure focus changes when list-buffers is called
(defadvice list-buffers (after focus activate)
  (other-window 1))

(provide 'paul-advice)


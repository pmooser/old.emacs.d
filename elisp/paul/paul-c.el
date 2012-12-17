;; My custom C formatting style:

(defconst paul-c-style
  '((c-basic-offset             . 4)
    (c-tab-always-indent        . t)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (innamespace       . 0)
                                   (substatement-open . 0)
                                   (case-label        . 4)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t))
  "Paul C Style")

(c-add-style "paulstyle" paul-c-style)

(defun paul-c-mode-common-hook ()
  (c-set-style "paulstyle"))

(dolist (hook '(c-mode-common-hook
                java-mode-common-hook))
  (add-hook hook 'paul-c-mode-common-hook))

(provide 'paul-c)

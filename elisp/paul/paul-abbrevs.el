;; all my abbreviations
(setq default-abbrev-mode t)

(require 'paul-lisp-skeletons)

(define-abbrev-table 'lisp-mode-abbrev-table
  '(("defuns"  "" paul-defun-skel)
    ("lets"    "" paul-let-skel)
    ("let*s"   "" paul-let*-skel)
    ("lambdas" "" paul-lambda-skel)
    ("conds"   "" paul-cond-skel)))
  
(provide 'paul-abbrevs)



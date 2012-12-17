;; an alternative version:
(defconst default-ignored-directories
  '("." ".." "CVS" "cvs" "RCS" "rcs" ".svn" ".git"))

(defun subdirectories (dir &optional ignored-directories)
  (let ((ignored-directories (if ignored-directories
                                 ignored-directories
                               default-ignored-directories))
        (children (directory-files dir t))
	dirs
	child)
    (while children
      (setq child (pop children))
      (when (and (file-directory-p child)
                 (not (member (file-name-nondirectory child) ignored-directories)))
        (push child dirs)
        (setq children (append (directory-files child t) children))))
    dirs))

(provide 'subdirectories)



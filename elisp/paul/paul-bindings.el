;; Key bindings, with OS X specifics:
(global-set-key (kbd "M-`") 'ns-next-frame)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)

;; use command as META 
(setq ns-command-modifier (quote meta))

;; toggle comments on current line
(global-set-key (kbd "C-;") 'toggle-comment-region-or-line)

;; vi-style paren/brace matching
(global-set-key (kbd "C-]") 'goto-match-paren)

;; NOTE - I've disabled these because they conflict with some
;; standard paredit bindings, which are almost always in place:
;; window cycling
;;(global-set-key (kbd "<M-down>") 'select-window-below)
;;(global-set-key (kbd "<M-up>") 'select-window-above)

;; goto-line key
(global-set-key (kbd "M-g") 'goto-line)

;; UNBINDINGS:
;; Bind C-z to ANYTHING other than the annoying minimize
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-t") nil)

;; always indent after hitting newline
;;(global-set-key (kbd "<RET>") 'newline-and-indent)

;; use bs-show instead of list-buffers
(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show)

(provide 'paul-bindings)


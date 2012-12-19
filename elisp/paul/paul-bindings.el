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

;; define my own minor mode to override keybindings

;; (defvar my-keys-minor-mode-map 
;;   (make-keymap) "my-keys-minor-mode keymap.")

;; (define-key my-keys-minor-mode-map 
;;   (kbd "C-i") 'some-function)

;; (define-minor-mode my-keys-minor-mode
;;   "A minor mode so that my key settings override annoying major modes."
;;   t " my-keys" 'my-keys-minor-mode-map)

;; (my-keys-minor-mode 1)

;; (defun my-minibuffer-setup-hook ()
;;   (my-keys-minor-mode 0))

;; (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;; (defadvice load (after give-my-keybindings-priority)
;;   "Try to ensure that my keybindings always have priority."
;;   (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
;;       (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
;;         (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
;;         (add-to-list 'minor-mode-map-alist mykeys))))
;; (ad-activate 'load)

(provide 'paul-bindings)


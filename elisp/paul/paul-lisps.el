;; general setup for lisps

(require 'rainbow-delimiters)
(require 'paredit)

;; For some reason these 4 lines are critical to make
;; paredit work properly in the REPL - otherwise it will
;; not backspace properly over {} or []:
(modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
(modify-syntax-entry ?\} "){" lisp-mode-syntax-table)
(modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)

;; BEGIN non-standard paredit customizations:

(eval-after-load 'paredit
  '(progn
     ;;(define-key paredit-mode-map (kbd "<M-up>") nil)
     ;;(define-key paredit-mode-map (kbd "<M-down>") nil)
     ;; make ctrl navigation work as it normally does (outside of paredit):
     (define-key paredit-mode-map (kbd "C-<right>") 'forward-word)
     (define-key paredit-mode-map (kbd "C-<left>") 'backward-word)

     (define-key paredit-mode-map (kbd "C-<backspace>") 'paredit-backward-kill-word)
     (define-key paredit-mode-map (kbd "C-w") 'paredit-kill-region)
     (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
     (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)

     (define-key paredit-mode-map (kbd "RET") 'paredit-newline)))

;; END non-standard paredit customizations

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

;; setup for lisps in general

(defun do-lisps-setup ()
  (rainbow-delimiters-mode t)
  (idle-highlight-mode t)
  (show-paren-mode t)
  (paredit-mode +1))

(dolist (hook
         '(emacs-lisp-mode-hook
           lisp-mode-hook
           lisp-interaction-mode-hook
           scheme-mode-hook
           clojure-mode-hook
           nrepl-mode-hook
           nrepl-interaction-mode-hook
           slime-mode-hook
           slime-repl-mode-hook))
  (add-hook hook 'do-clojure-setup))

(provide 'paul-lisps)


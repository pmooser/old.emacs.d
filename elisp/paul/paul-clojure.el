(message "Setting up clojure ...")

;; nrepl

(require 'cider)

(setq cider-popup-stacktraces nil)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; NOTE - for some reason, ac-nrepl causes the first command executed in the repl
;; to take a significant amount of time. This never used to happen, before cider.

(require 'ac-nrepl)

(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; ;; auto-complete
(setq ac-dictionary-directories (list "~/elisp/other/auto-complete/dict"))
(require 'auto-complete-config)
(ac-config-default)

;; This is disabled for now, because having this set means hitting return while 
;; not connected to an nrepl session results in an error:
;; auto-complete normally uses RET to accept a completion,
;; but sometimes I don't want that, so this will disable it:
;;(define-key ac-completing-map [return] 'nrepl-return)

;; auto complete source using slime completions (works for clojure)

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

(require 'slime)
(setq slime-protocol-version 'ignore)
(slime-setup '(slime-repl slime-fancy))

;; basic setup
(require 'clojure-mode)

;; (defvar slime-override-map (make-keymap))

;; (define-minor-mode slime-override-mode
;;   "Fix SLIME REPL keybindings"
;;   nil " SLIME-override" slime-override-map)

;; (define-key slime-override-map (kbd "<C-return>") 'paredit-newline)
;; (define-key slime-override-map (kbd "{") 'paredit-open-curly)
;; (define-key slime-override-map (kbd "}") 'paredit-close-curly)
;; (define-key slime-override-map [delete] 'paredit-forward-delete)
;; (define-key slime-override-map [backspace] 'paredit-backward-delete)
;; (define-key slime-override-map (kbd "C-<backspace>") 'paredit-backward-kill-word)
;; (define-key slime-override-map (kbd "C-w") 'paredit-kill-region)
;; (define-key slime-override-map (kbd "M-[") 'paredit-wrap-square)
;; (define-key slime-override-map (kbd "M-{") 'paredit-wrap-curly)
;; (define-key slime-override-map (kbd "C-<right>") 'forward-word)
;; (define-key slime-override-map (kbd "C-<left>") 'backward-word)

;; (add-hook 'slime-repl-mode-hook
;;           (lambda ()
;;             (slime-override-mode t)
;;             (slime-redirect-inferior-output)
;;             (modify-syntax-entry ?\[ "(]")
;;             (modify-syntax-entry ?\] ")[")
;;             (modify-syntax-entry ?\{ "(}")
;;             (modify-syntax-entry ?\} "){")))

;; slime-repl-specific bindings that we need:
(define-key slime-repl-mode-map (kbd "{") 'paredit-open-curly)
(define-key slime-repl-mode-map (kbd "}") 'paredit-close-curly)
(define-key slime-repl-mode-map (kbd "M-(") 'paredit-wrap-round)
(define-key slime-repl-mode-map (kbd "M-[") 'paredit-wrap-square)
(define-key slime-repl-mode-map (kbd "M-{") 'paredit-wrap-curly)

;; these are not bound correctly in clojure-mode with paredit:
(define-key clojure-mode-map "{" 'paredit-open-curly)
(define-key clojure-mode-map "}" 'paredit-close-curly)

;; idle highlighting
(if (boundp 'idle-highlight-mode)
    (require 'idle-highlight-mode))

;; swank-clojure-1.4.0:
;; This references some seemingly-undefined tramp functions,
;; and this avoids that causing things to blow up:
;;(remove-hook 'slime-connected-hook 'clojure-slime-remote-file-name-hook)

;; Anything that starts with 'def' should get keyword highlighting:
(font-lock-add-keywords
 'clojure-mode
 `((,(concat "(\\(?:clojure.core/\\)?\\("
             "\\(def\\)\\(\\w*\\)"
             ;; Function declarations.
             "\\)\\>"
             ;; Any whitespace
             "[ \r\n\t]*"
             ;; Possibly type or metadata
             "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
             "\\(\\sw+\\)?")
    (1 font-lock-keyword-face))))

(require 'rainbow-delimiters)

;; If we add this to cider-repl-mode-hook, it causes everything to hang:
(dolist (hook '(clojure-mode-hook cider-mode-hook))
  (add-hook hook (lambda ()
                   (idle-highlight-mode t))))

(defun do-clojure-setup ()
  (rainbow-delimiters-mode t)
  (paredit-mode +1))

(dolist (hook
         '(clojure-mode-hook
           cider-mode-hook
           cider-repl-mode-hook
           slime-mode-hook
           slime-repl-mode-hook))
  (add-hook hook 'do-clojure-setup))

(provide 'paul-clojure)

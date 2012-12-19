;; make sure things in my bin directory can be found:
(defconst bin-dir (concat (getenv "HOME") "/bin"))
(setenv "PATH" (concat (getenv "PATH") ":" bin-dir))
(setq exec-path (append exec-path (list bin-dir)))

;; load my elisp directories:
(defconst elisp-dir (locate-user-emacs-file "elisp"))
(defconst pkgs-dir  (locate-user-emacs-file "pkgs"))

(load (concat elisp-dir "/subdirectories.el"))

(setq load-path
     (append (list elisp-dir pkgs-dir)
             (subdirectories elisp-dir)
             (subdirectories pkgs-dir)
             load-path))

;; start a server so we can open files in an emacs instance from the cmd line
(server-start)

;; enable ido mode
(ido-mode)

;; general appearance and behavior
(tool-bar-mode 0)
(blink-cursor-mode 0)
(setq default-truncate-lines t)
(setq inhibit-startup-message t)

(transient-mark-mode 1)

;; just warn if we try to open a symlink to a file under version control
(setq vc-follow-symlinks nil)

;; turn off anti-aliasing on macs
;; (if (boundp 'mac-allow-anti-aliasing)
;;     (setq mac-allow-anti-aliasing nil))

;; turn off annoying audible bell for versions less than 23
(setq visible-bell (version< emacs-version "23"))

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; org-mode
;; (lines from David O'Toole's Org tutorial)
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; always ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; backups and auto-saves
(defvar backup-dir "~/.emacs-backup/")

(make-directory backup-dir t)

;; backup settings
(setq version-control     t     ; numbered backups
      backup-by-copying   t     ; copy, don't clobber symlinks
      ;kept-new-versions   6
      ;kept-old-versions   2
      delete-old-versions t
      ;; put auto-save files in the backup dir
      auto-save-file-name-transforms `(("\\(?:[^/]*/\\)*\\(.*\\)" ,(concat backup-dir "\\1") t))
      backup-directory-alist `(("." . ,backup-dir)))

;; multiple cursors
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; snippets
(require 'yasnippet)

;; undo tree!
;; C-x u to visualize
(require 'undo-tree)
(global-undo-tree-mode)

;; my personal settings
(require 'paul-lisps)
(require 'paul-utilities)
(require 'paul-advice)
(require 'paul-erlang)
(require 'paul-clojure)
(require 'paul-scheme)
(require 'paul-haskell)
(require 'paul-c)
(require 'paul-bindings)

;; Visual characteristics:
(require 'font-lock)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)

;; mode selection by filename suffix
(setq auto-mode-alist
      (append '(("\\.m\\'"    . objc-mode)
                ("\\.h\\'"    . objc-mode)
                ("\\.erl\\'"  . erlang-mode)
                ("\\.ss$"     . scheme-mode)
                ("\\.scm$"    . scheme-mode)
                ("\\.sch$"    . scheme-mode)
                ("\\.hs\\'"   . haskell-mode)
                ("\\.rb\\'"   . ruby-mode)
                ("\\.rhtml$"  . html-mode))
              auto-mode-alist))

;; default frame characteristics
(setq default-frame-alist
      '((top    . 22)
        (left   . 640)
	(width  . 85)
        (height . 47)
	(cursor-color . "grey40")
	(cursor-type . box)
	(foreground-color . "white")
	(background-color . "black")))
	       
(setq initial-frame-alist '((top . 22) (left . 0)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-modes (quote (emacs-lisp-mode lisp-interaction-mode c-mode cc-mode c++-mode java-mode clojure-mode scala-mode scheme-mode ocaml-mode tuareg-mode perl-mode cperl-mode python-mode ruby-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode slime-repl-mode nrepl-mode nrepl-interaction-mode)))
 '(bs-default-configuration "files-and-scratch")
 '(bs-default-sort-name "by filename")
 '(ido-enable-flex-matching t)
 '(quack-pretty-lambda-p t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :background "LightGreen" :foreground "black"))))
 '(diff-indicator-added ((t (:inherit diff-added :background "black" :foreground "LightGreen"))))
 '(diff-indicator-removed ((t (:inherit diff-removed :background "black" :foreground "LightPink"))))
 '(diff-removed ((t (:inherit diff-changed :background "pink" :foreground "black"))))
 '(idle-highlight ((t (:inherit region :background "grey25"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "lightgreen"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "skyblue"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "pink"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#39F"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "grey" :foreground "red")))))

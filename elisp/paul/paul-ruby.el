;; general ruby requirements:

(autoload 'ruby-mode "ruby-mode" nil t)

;; for special folding
(require 'hideshow)

;; These should set up ruby with mmm-mode
(require 'mmm-mode)
(require 'mmm-auto)

(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)

(set-face-background 'mmm-output-submode-face  "DarkSlateGray")
(set-face-background 'mmm-code-submode-face    "DarkSlateBlue")
(set-face-background 'mmm-comment-submode-face "DarkOliveGreen")

(mmm-add-classes
 '((erb-code
    :submode ruby-mode
    :match-face (("<%#" . mmm-comment-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :front "<%[#=]?"
    :back "%>"
    :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
             (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
             (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @)))))

(add-hook 'html-mode-hook
          (lambda ()
            (local-set-key (kbd "<f8>") 'mmm-parse-buffer)
            (setq mmm-classes '(erb-code))
            (mmm-mode-on)))

;(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))

;(add-to-list 'hs-special-modes-alist ruby-hs)

(provide 'paul-ruby)

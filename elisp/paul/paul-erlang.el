;; erlang setup

(add-hook 'erlang-mode-hook
          (lambda ()
            (local-set-key (kbd "<RET>") 'newline-and-indent)))

(setq erlang-root-dir "/opt/local/lib/erlang")
(setq exec-path (cons "/opt/local/lib/erlang/bin" exec-path))
(require 'erlang-start)

;; --> distel
;;(add-to-list 'load-path "/Users/pmooser/elisp/distel/elisp")
(require 'distel)
(distel-setup)

;; avoid a hang-on-compile
(defvar inferior-erlang-prompt-timeout t)
;; default node name to emacs@localhost
(setq inferior-erlang-machine-options '("-sname" "emacs"))
;; tell distel to default to that node
(setq erl-nodename-cache
      (make-symbol
       (concat
        "emacs@"
        ;; Mac OS X uses "name.local" instead of "name", this should work
        ;; pretty much anywhere without having to muck with NetInfo
        ;; ... but I only tested it on Mac OS X.
        (car (split-string (shell-command-to-string "hostname"))))))

(provide 'paul-erlang)


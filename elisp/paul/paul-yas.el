;; snippets
(require 'yasnippet)
(require 's)

;; also load snippets from any directories found in
;; the SNIPPET-DIRS environment variable
(if (boundp 'my-snippet-dirs)
    (setq yas-snippet-dirs
          (append yas-snippet-dirs my-snippet-dirs)))

(yas-global-mode 1)

(provide 'paul-yas)

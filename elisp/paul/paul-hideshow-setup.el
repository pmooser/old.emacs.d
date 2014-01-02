; various face-related functions

;(make-face 'ellipsis-spacer-face)
;(make-face 'underliney)
;(set-face-foreground 'underliney "yellow")
;(set-face-background 'underliney "black")
;(set-face-italic-p   'underliney nil)
;(set-face-underline  'underliney t)

; This macro minimizes the nastiness we have to be directly
; exposed to when setting multiple strings and faces to be the
; ellipsis replacement

;; (defmacro replace-ellipsis (specs)
;;   "This macro is used to replace the ellipsis used when hiding text regions."
;;   `(vconcat
;;     ,@(mapcar
;;        (lambda (spec)
;;          (let ((offset (lsh (face-id (cdr spec)) 19)))
;;            `(quote ,(mapcar (lambda (c) (+ offset c)) (car spec)))))
;;        specs)))

;; (set-display-table-slot standard-display-table
;;                         'selective-display
;;                         (replace-ellipsis ((" " . ellipsis-spacer-face)
;;                                            ("{...}" . underliney))))

(make-face 'hs-face)
(set-face-foreground 'hs-face "yellow")

(setq hs-set-up-overlay
      (defun my-display-code-line-counts (ov)
        (when (eq 'code (overlay-get ov 'hs))
          (overlay-put ov 'display
                       (propertize
                        (format " < %d >"
                                (count-lines (overlay-start ov)
                                             (overlay-end ov)))
                        'face 'hs-face)))))

(setq hs-hide-comments-when-hiding-all nil)

(add-hook 'ruby-mode-hook 'hs-minor-mode)

(defun hs-ruby-end-of-block (arg)
  (ruby-end-of-block)
  (backward-line-nomark))

(setq ruby-hs '(ruby-mode
                "\\(def\\)"
                "\\(end\\)"
                "#"
                hs-ruby-end-of-block    
                nil))                   ; adj-beg-fn

(add-to-list 'hs-special-modes-alist ruby-hs)

(defun my-set-hs-keys ()
  ;;   (define-key hs-minor-mode-map (kbd "BLAH") 'hs-hide-block)
  ;;   (define-key hs-minor-mode-map (kbd "BLAH") 'hs-show-block)
  (define-key hs-minor-mode-map (kbd "C-c h") 'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "C-c s") 'hs-show-all)
  (define-key hs-minor-mode-map (kbd "C-c t") 'hs-toggle-hiding))

(add-hook 'hs-minor-mode-hook 'my-set-hs-keys)

(provide 'paul-hideshow-setup)

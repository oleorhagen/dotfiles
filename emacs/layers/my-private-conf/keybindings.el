;; Add a key for listing the current available snippets

;; Comment box binding on '<leader> o c b'
(spacemacs/declare-prefix
  "oc" "comment"
  "od" "describe yasnippet tables"
  "ow" "whitespace cleanup"
  "os" "split line")
(spacemacs/set-leader-keys
  "ocb" 'comment-box
  "od" 'yas-describe-tables
  "ow" 'whitespace-cleanup
  "os" 'my-split-line-on-spaces)

;; Global only run flycheck when prompted
(spacemacs/declare-prefix
  "eb" "flycheck-buffer")
(spacemacs/set-leader-keys
  "eb" '(lambda ()
          (interactive)
          ;; Enable flycheck if it is not enabled
          (flycheck-mode 1)
          (call-interactively #'flycheck-buffer)))



;; evaluate sexpressions with ee
(spacemacs/set-leader-keys
  "ee" 'eval-last-sexp
  "eb" 'eval-buffer)

;; (add-hook 'edebug-mode ()
;;           (define-key evil-motion-state-local-map (kbd "n") 'Info-next)
;;           (define-key evil-motion-state-local-map (kbd "H") 'Info-prev)
;;           (define-key evil-motion-state-local-map (kbd "L") 'Info-next)))

;; Add a key for listing the current available snippets
(spacemacs/set-leader-keys
  "os" 'yas-describe-tables)

;;; Make return follow link in info-mode
(add-hook 'Info-mode-hook
          (lambda ()
            (define-key evil-motion-state-local-map
                        (kbd "<return>") 'Info-follow-nearest-node)))

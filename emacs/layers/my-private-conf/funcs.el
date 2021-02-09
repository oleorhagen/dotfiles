
;;
;;; This is a simple snippet to replace the license year header with the current
;;; year.
;;

(defun update-license-year ()
  (interactive)
  (when (memq this-command '(save-buffer save-some-buffers))
    (beginning-of-buffer)
    (when (re-search-forward "^\\(#\\|;;\\|//\\)\\( +Copyright +\\)\\([0-9]\\{4\\}\\)" nil nil)
      (replace-match (format-time-string "\\1\\2%Y")))))

;; Add the hook to after-save

(add-hook 'after-save-hook
          (lambda ()
            (when (memq this-command '(save-buffer save-some-buffers))
              (save-excursion
                (beginning-of-buffer)
                (when (looking-at "^\\(#\\|;;\\|//\\)\\( +Copyright +\\)\\([0-9]\\{4\\}\\) +Northern.tech")
                  (update-license-year))))))



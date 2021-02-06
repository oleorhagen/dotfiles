
;;
;;; This is a simple snippet to replace the license year header with the current
;;; year.
;;

(defun update-license-year ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (when (re-search-forward "^\\(#\\|;;\\|//\\)\\( +Copyright +\\)\\([0-9]\\{4\\}\\)" nil nil)
      (replace-match (format-time-string "\\1\\2%Y")))))

;; Add the hook to after-save

(add-hook 'after-save-hook
          (lambda ()
            (save-excursion
              (beginning-of-buffer)
              (when (re-search-forward
                     "^\\(#\\|;;\\|//\\)\\( +Copyright +\\)\\([0-9]\\{4\\}\\) +Northern.tech"
                     nil nil)
                (update-license-year)))))


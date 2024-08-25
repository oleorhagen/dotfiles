;;
;;; This is a simple snippet to replace the license year header with the current
;;; year (EOL) - we no longer update the year of the License Header
;;

;; (defun update-license-year ()
;;   (save-excursion
;;     (when (memq this-command '(save-buffer save-some-buffers))
;;       (beginning-of-buffer)
;;       (when (re-search-forward "^\\(#\\|;;\\|//\\)\\( +Copyright +\\)\\([0-9]\\{4\\}\\)" nil nil)
;;         (replace-match (format-time-string "\\1\\2%Y"))))))

;; ;; Add the hook to before-save

;; (add-hook 'before-save-hook
;;           (lambda ()
;;             (when (memq this-command '(save-buffer save-some-buffers))
;;               (save-excursion
;;                 (beginning-of-buffer)
;;                 (when (looking-at "^\\(#\\|;;\\|//\\)\\( +Copyright +\\)\\([0-9]\\{4\\}\\) +Northern.tech")
;;                   (update-license-year))))))


(defun my-switch-to-prod-file ()
  "Switch to prod version of the k8s file if it exists"
  (interactive)
  (let ((current-buffer-name (buffer-file-name)))
    (cond
     ((string-match ".*/prod/.*" current-buffer-name) (find-file (string-replace "prod" "dev" current-buffer-name) ))
     ((string-match ".*/dev/.*" current-buffer-name) (find-file (string-replace "dev" "prod" current-buffer-name) )))))

(defun advice-projectile-toggle (orig-func &rest args)
  "Advice function for running another function when 'in-k8s-repo' is defined"
  (interactive)
  (if (boundp 'in-k8s-repo)
      (apply 'my-switch-to-prod-file nil)
    (apply orig-func args)))

;; "Adviced to change folders from prod <-> dev in k8s-flux repo (mimiro)"
(advice-add 'projectile-toggle-between-implementation-and-test :around
            #'advice-projectile-toggle)

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


(defun my-switch-to-js-css-file ()
  "Switch to css file if it exists"
  (interactive)
  (let ((current-buffer-name (buffer-file-name)))
    (cond
     ((string-match ".*\.js" current-buffer-name) (find-file (string-replace "js" "css" current-buffer-name) ))
     ((string-match ".*\.css" current-buffer-name) (find-file (string-replace "css" "js" current-buffer-name) )))))

(defun advice-projectile-toggle (orig-func &rest args)
  "Advice function for running another function when 'in-k8s-repo' is defined"
  (interactive)
  (cond
   ((boundp 'in-k8s-repo) (apply 'my-switch-to-prod-file nil))
   ((boundp 'in-js-project) (apply 'my-switch-to-prod-file nil))
   ((apply orig-func args))))

;; "Adviced to change folders from prod <-> dev in k8s-flux repo (mimiro)"
(advice-add 'projectile-toggle-between-implementation-and-test :around
            #'advice-projectile-toggle)


;; A simple function to copy the buffer from part I to II during AOC
(defun my-aoc-part-ii ()
  "Create part II from the part I buffer"
  (interactive)
  ;; Write the whole current buffer to part2.cpp
  (write-region nil nil "part2.cpp")
  (find-file "part2.cpp")
  (replace-string-in-region "part1" "part2" (point-min) (line-end-position))
  (save-buffer))

(defun my-split-line-on-spaces (begin end)
  (interactive "r")
  (replace-regexp "\\([^ ]+\\) " "\\1\n" nil begin end))

(defun my/create-branch-from-main (branch-name)
  "Fetch all remotes, checkout origin/main, and create a new branch.
BRANCH-NAME is prompted for interactively."
  (interactive "sBranch name: ")
  (message "Fetching all remotes...")
  (magit-git "fetch" "--all")
  (message "Checking out origin/main...")
  (magit-checkout "origin/main")
  (message "Creating branch %s..." branch-name)
  (magit-create-branch branch-name "origin/main")
  (magit-checkout branch-name)
  (message "Done!"))

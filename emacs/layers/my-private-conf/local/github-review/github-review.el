
;;
;; Simple functionality to help review pull-requests in Emacs
;;

;; TODO - Prettier interface to start a review
;; TODO - Don't start all the interactive LSP stuff when reviewing - highlighting only
;; TODO - Highlight the changes in some colour/shade


(defun github-review-checkout-pr (number)
  (interactive "n")
  (shell-command (format "gh pr checkout %s" number)))


(defun extract-files-in-commit (commit)
  (shell-command-to-string (format "git diff-tree --no-commit-id --name-only -r %s" commit)))

;; Open the files in the PR in a different buffer

;; (require 'persp) - Not needed I think !
(require 'magit)

;; Insert the keybinding 'v' to view a commit as a reviewer (!)

;; (transient-append-suffix 'magit-log "a"
;;   '("w" "Wip" magit-wip-log-current))

(defvar persp-review-name "github-review" "This is the name of the review perspective")

(transient-append-suffix 'magit-dispatch "_"
  '("v" "reView" review-commit))

(defun current-commit ()
  (magit-copy-section-value nil))

(defun open-files-in-new-perspective (files)
  (persp-switch "github-review")
  (defun iter-open (files)
    (unless (null files)
      (message "Opening %s for review" (expand-file-name (car files)))
      (find-file-noselect (car files))
      (iter-open (cdr files))))
  (iter-open files)
  (switch-to-buffer (car files)))

(defun do-review (number)
  (interactive "n")
  (github-review-checkout-pr number))

(defun review-commit ()
  (interactive)
  (let* ((commit (current-commit))
        (files (split-string (extract-files-in-commit commit))))
    (open-files-in-new-perspective files)))

(defun github-review-close ()
  (interactive)
  (persp-kill "github-review"))


(provide 'github-review)


;;
;; Simple functionality to help review pull-requests in Emacs
;;


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

(defvar persp-review-name "review" "This is the name of the review perspective")
(transient-append-suffix 'magit-dispatch "_"
  '("v" "reView" review-commit))

(defun current-commit ()
  (shell-commit-to-string (format "git rev-parse HEAD")))

(defun open-files-in-new-perspective (files)
  (interactive "b")
  (persp-switch "baz")
  (defun ))

(defun do-review-commit (commit)
  (let ((files (extract-files-in-commit commit)))
    (open-files-in-new-perspective files)))

(defun do-review (number)
  (interactive "n")
  (github-review-checkout-pr number))

(defun review-commit ()
  (interactive)
  (do-review)
  (let ((commit (current-commit))
        (files (extract-files-in-commit commit)))
    (open-files-in-new-perspective files)))

(defun review-close ()
  (interactive)
  (persp-kill 'persp-review-name))

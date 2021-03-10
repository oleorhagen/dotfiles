(require 'magit)


(transient-append-suffix 'magit-dispatch "A"
  '("t" "backports" my-magit-cherry-multiple))

(defun my-magit-cherry-multiple (name branches commits  &optional args)
  "Move COMMITS from the current branch onto another existing BRANCH.
Then move to existing BRANCH. If a conflict occurs, then you have
to fix that and finish the process manually."
  (interactive
   ;; Return the list of lists of commits and branches
   (list
    (read-string "Branch name: ")
    (magit-completing-read-multiple* "Cherry to branch: " (magit-list-branch-names)) ;; branches
    (magit-cherry-pick-read-args "Cherry-pick"))) ;; commits
  (let ((commits-new (car commits))
        (args-new (caadr commits)))
    (message commits-new)
    (message args-new)
    (dolist (branch branches)
      (message branch)
      (progn
        (magit-run-git
               "checkout" "-B" (format "cherry-%s-%s" name branch) branch "--no-track")
        (magit-run-git "cherry-pick" commits-new)
        (magit-run-git "push" "--dry-run" "-f" "--set-upstream" "oleorhagen" (format "cherry-%s-%s" name branch))
        (magit-call-process "gh" "pr" "create"
                            "--head"
                            ;; "--title" (format "cherry of %s" name)
                            "--base" branch
                            "--fill")
        ))))

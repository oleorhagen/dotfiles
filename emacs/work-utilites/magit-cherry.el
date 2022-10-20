(require 'magit)


(transient-append-suffix 'magit-dispatch "A"
  '("t" "backports" my-magit-cherry-to))

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
        (args-new (caadr commits))
        (body-description (magit-run-git "log -1 --format=\%s")))
    (message commits-new)
    (message args-new)
    (dolist (branch branches)
      (message branch)
      (progn
        (magit-run-git
               "checkout" "-B" (format "cherry-%s-%s" name branch) branch "--no-track")
        (magit-run-git "cherry-pick" commits-new)
        (magit-run-git "push" "-f" "--set-upstream" "oleorhagen" (format "cherry-%s-%s" name branch))
        (magit-call-process "gh" "pr" "create"
                            "--title" (format "\"[Cherry %s]: %s\"" (string-remove-prefix "mendersoftware/" branch) "Title")
                            "--base" (string-remove-prefix "mendersoftware/" branch)
                            "--body" "Cherry"
                            "--draft")))))



;; (defun )

(defun my-magit-cherry-to (target-branch commits  &optional args)
  "Create a branch on the target BRANCH, and then cherry-pick the
given commits to it. The BRANCH will be named as
<cherry-[target-cherry-pick-branch]>. If a conflict occurs you
will have to resolve this manually yourself."
  ;; Extract the arguments interactively
  (interactive
   (list
    ;; Helm (magit) get branch
    (magit-completing-read-multiple* "Cherry-pick to branch: " (magit-list-branch-names))
    ;; Get the commits
    (magit-cherry-pick-read-args "Cherry-pick: ")))
  (progn
    (magit-run-git
     "checkout" "-B" (format "cherry-%s" (car target-branch)) target-branch "--no-track")
    (magit-run-git "cherry-pick" commits)
    (magit-push-current (format "cherry-%s" (car target-branch)) target-branch)
    ;; TODO - Use forge to automatically create a PR
    (forge-create-pullreq source-branch target-branch)))

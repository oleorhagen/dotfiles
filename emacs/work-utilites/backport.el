
(require 'magit)

;; (defun magit-backport-to-branches (commits &optional revert)
(defun magit-backport-to-branches ()
  "magit-backport-to-branches backports the commits at point, or
  in region to the branches selected by Helm"
  (interactive)
  ;; (message arg)
  (magit-checkout))


(defun backport-commits (commits branches)
  "Backports the given commits to branches chosen by Helm (?) - for now: provided manually"
  (dolist (branch branches)
    (backport-commits-to-branch commits branch)))

(defun backport-commits-to-branch (commits branch)
  (magit-checkout branch)
  (magit-cherry-pick commits))

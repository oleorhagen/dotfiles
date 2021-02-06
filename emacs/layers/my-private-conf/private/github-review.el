
;;
;; Simple functionality to help review pull-requests in Emacs
;;


(defun github-review-checkout-pr (number)
  (interactive "n")
  (shell-command (format-string "gh pr checkout %s" number)))

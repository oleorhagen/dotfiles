
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple mode, which is simply a place-holder for loading git commit message specifics. ;;
;; As such, it is only a thin wrapper around text-mode.                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; TODO - Create a syntax highlighting table for this (?)

(define-derived-mode git-conventional-commit-message-mode
  text-mode "git-conventional-commits"
  "Major mode for writing git conventional commit messages.")

(provide 'git-conventional-commit-message-mode)

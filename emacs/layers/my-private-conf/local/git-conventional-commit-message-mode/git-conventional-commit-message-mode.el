
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple mode, which is simply a place-holder for loading git commit message specifics. ;;
;; As such, it is only a thin wrapper around text-mode.                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; INPROGRESS - Create a syntax highlighting table for this (?)

(defvar git-conventional-commit-message-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?+  "." st)
    (modify-syntax-entry ?-  "." st)
    (modify-syntax-entry ?%  "." st)
    (modify-syntax-entry ?&  "." st)
    (modify-syntax-entry ?|  "." st)
    (modify-syntax-entry ?^  "." st)
    (modify-syntax-entry ?!  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?<  "." st)
    (modify-syntax-entry ?>  "." st)
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?`  "\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    ;; TODO make _ a symbol constituent now that xemacs is gone
    (modify-syntax-entry ?_  "w" st)

    ;; Personal: # is a comment in git commit files
    (modify-syntax-entry ?# "<" st)

    st)
  "Syntax table for Git conventional commit mode.")


;; INPROGRESS - Fontify the buffer also

(defun git-conventional-commit-message--font-lock-keywords ()
  `(
    (,(rx "="
          (zero-or-more (syntax whitespace)) word-start
          (group (or "fix" "feat" "chore" "test" "refac" "perf"))
          word-end (zero-or-more (syntax whitespace)) line-end)
     (1 'font-lock-keyword-face))
    ))



(define-derived-mode git-conventional-commit-message-mode
  text-mode "git-conventional-commits"
  ;; TODO - maybe inherit from the existing git-commit mode map
  ;; :syntax-table git-conventional-commit-message-mode-syntax-table
  "Major mode for writing git conventional commit messages.")

(provide 'git-conventional-commit-message-mode)

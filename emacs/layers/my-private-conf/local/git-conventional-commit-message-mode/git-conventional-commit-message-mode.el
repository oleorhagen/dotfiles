
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple mode, which is simply a place-holder for loading git commit message specifics. ;;
;; As such, it is only a thin wrapper around text-mode.                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 'rx))


;;; INPROGRESS - Create a syntax highlighting table for this (?)

(defvar git-conventional-commit-mode-abbrev-table nil
  "Abbreviation table used in `nps-mode' buffers.")

(define-abbrev-table 'git-conventional-commit-mode-abbrev-table
  '())

(defvar nps-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\( "()" st)

    ;; - and _ are word constituents
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "w" st)

    ;; both single and double quotes makes strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?' "'" st)

    ;; add comments. lua-mode does something similar, so it shouldn't
    ;; bee *too* wrong.
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)

    ;; '==' as punctuation
    (modify-syntax-entry ?= ".")
    st)
  "Syntax table for Git conventional commit mode.")


;; INPROGRESS - Fontify the buffer also

(defconst git-conventional-commit-font-lock-keywords
  '("fix" "feat" "style" "chore"))

(defconst git-conventional-commit-font-lock-special-keywords
  '("All" "None"))

(defconst git-conventional-commit--font-lock-defaults
  (let ((keywords git-conventional-commit-font-lock-keywords)
        (special-words git-conventional-commit-font-lock-special-keywords)
        (types '("Changelog" "Ticket" "Signed-off-by:")))
    `(((,(rx-to-string `(: (or ,@keywords))) 0 font-lock-keyword-face)
       ("(\\([[:word:]]+\\))" 1 font-lock-function-name-face)
       (,(rx-to-string `(: (or ,@special-words))) 0 font-lock-warning-face)
       (,(rx-to-string `(: (or ,@types))) 0 font-lock-type-face)))))


;;;###autoload
(define-derived-mode git-conventional-commit-mode prog-mode "git-conventional-commits"
  "Major mode for writing git conventional commit messages."
  :abbrev-table git-conventional-commit-mode-abbrev-table
  (setq font-lock-defaults git-conventional-commit--font-lock-defaults)
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+[\t ]*")
  )


(provide 'git-conventional-commit-mode)

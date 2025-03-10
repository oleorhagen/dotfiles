;;
;; Interactive CLI Tool Wrapper for the rbuild CLI tool
;;
;; Builds command-line tools interactively by parsing --help output

(defun extract-options-from-help (help-output)
  "Extract command line options from HELP-OUTPUT."
  (let ((options '())
        (lines (split-string help-output "\n")))
    (dolist (line lines)
      ;; Match patterns like: -o, --option or --option=VALUE
      (when (string-match "\\(--?[a-zA-Z0-9][-a-zA-Z0-9]*\\)\\(=[A-Z]+\\)?\\|\\(-[a-zA-Z],\\s+\\)?\\(--[a-zA-Z0-9][-a-zA-Z0-9]*\\)" line)
        (let ((option (or (match-string 4 line) (match-string 1 line))))
          (when option
            (push option options)))))
    (reverse options)))


(defun extract-subcommands-from-help (help-output)
  "Extract subcommands from HELP-OUTPUT."
  (let ((subcommands '())
        (lines (split-string help-output "\n"))
        (in-commands-section nil))
    (dolist (line lines)
      ;; Look for sections typically containing commands
      (cond
       ((string-match "\\(Commands\\|Subcommands\\|Available commands\\):" line)
        (setq in-commands-section t))
       ((and in-commands-section
             (string-match "^\\s-*\\([a-zA-Z][-a-zA-Z0-9]*\\)\\s-+" line))
        (push (match-string 1 line) subcommands))))
    (reverse subcommands)))


(defun my-get-build-target-list ()
  (interactive)
  (process-lines "build" "--list-targets"))


(defun interactively-build-cli-command (cmd)
  "Build a command for CMD interactively based on its help output."
  (interactive "sCommand to wrap: ")
  (let* (
         (directory (read-directory-name "Run in directory: " default-directory))
         (default-directory directory)
         (help-output (shell-command-to-string (concat cmd " --help")))
         (options (extract-options-from-help help-output))
         (subcommands (extract-subcommands-from-help help-output))
         (selected-options '())
         (selected-subcommand nil)
         (additional-args nil))

    ;; First select a subcommand if available
    ;; (when subcommands
    ;;   (setq selected-subcommand
    ;;         (completing-read "Select subcommand (optional): "
    ;;                          (cons "" subcommands) nil t))
    ;;   (when (string= selected-subcommand "")
    ;;     (setq selected-subcommand nil)))

    ;; First select the target
    (setq selected-subcommand (concat "-t " (completing-read "Select the build target: " (my-get-build-target-list) nil t) ))

    ;; Select options
    (while (not (string= " " (setq option
                                   (completing-read "Add option (RET to finish): "
                                                    options nil nil))))
      (let ((value ""))
        ;; If option might need a value, prompt for it
        (when (string-match "^--" option)
          (setq value (read-string (format "Value for %s (leave empty for flag): " option))))

        (if (string= value "")
            (push option selected-options)
          (push (format "%s=%s" option value) selected-options))

        ;; Remove the selected option from options list
        (setq options (delete option options))))

    ;; Ask for any additional arguments
    ;; (setq additional-args
    ;;       (read-string "Additional arguments: "))

    ;; Build the final command
    (let ((final-command (concat cmd
                                 (if selected-subcommand
                                     (concat " " selected-subcommand)
                                   "")
                                 (if selected-options
                                     (concat " " (string-join (reverse selected-options) " "))
                                   "")
                                 (if (not (string= additional-args ""))
                                     (concat " " additional-args)
                                   ""))))

      ;; Preview and confirm
      (let ((confirm (read-string
                      (format "Execute command (edit if needed):\n%s\n: " final-command)
                      final-command)))
        (compile confirm)))))


;; Define a function to create specific wrappers
(defun create-cli-wrapper (command-name)
  "Create an interactive wrapper function for COMMAND-NAME."
  (let ((func-name (intern (concat "run-" command-name))))
    (fset func-name
          `(lambda ()
             ,(format "Build and run a %s command interactively." command-name)
             (interactive)
             (interactively-build-cli-command ,command-name)))
    func-name))


;; Now I need to make this work with rbuild


(create-cli-wrapper "build")

;; Example usage:
;; (create-cli-wrapper "git")
;; (global-set-key (kbd "C-c g") (create-cli-wrapper "git"))

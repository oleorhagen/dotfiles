;;
;; Interactive CLI Tool Wrapper for the rbuild CLI tool
;;
;; Builds command-line tools interactively by parsing --help output

(defvar cli-wrapper-history-file
  (expand-file-name "cli-wrapper.hist" user-emacs-directory)
  "File to save CLI wrapper command history.")

;; History variables
(defvar cli-wrapper-history-alist '()
  "Alist of command histories for different tools, format: ((command . history-list) ...)")

(defvar cli-wrapper-history-size 50
  "Maximum number of history entries to keep per command.")

(defun cli-wrapper-add-to-history (command cmd-string)
  "Add CMD-STRING to the history for COMMAND."
  (let* ((history-entry (assoc command cli-wrapper-history-alist))
         (history-list (if history-entry
                           (cdr history-entry)
                         '())))
    ;; Remove duplicates
    (setq history-list (delete cmd-string history-list))
    ;; Add to front
    (push cmd-string history-list)
    ;; Truncate if needed
    (when (> (length history-list) cli-wrapper-history-size)
      (setq history-list (seq-take history-list cli-wrapper-history-size)))

    ;; Update or add history entry
    (if history-entry
        (setcdr history-entry history-list)
      (push (cons command history-list) cli-wrapper-history-alist))))

(defun cli-wrapper-get-history (command)
  "Get command history list for COMMAND."
  (let ((history-entry (assoc command cli-wrapper-history-alist)))
    (if history-entry
        (cdr history-entry)
      '())))


;; Function to explicitly view and manage command history
(defun cli-wrapper-view-history (cmd)
  "View and optionally execute a command from CMD history."
  (interactive "sCommand history to view: ")
  (let* ((history (cli-wrapper-get-history cmd))
         (selected (completing-read (format "%s history: " cmd) history nil t)))
    (when selected
      (when (y-or-n-p "Execute this command? ")
        (compile selected)))))


;; Function to save history between Emacs sessions
(defun cli-wrapper-save-history ()
  "Save CLI wrapper command history to a file."
  (interactive)
  (with-temp-file cli-wrapper-history-file
    (insert ";; CLI Wrapper History - Auto-generated\n")
    (insert ";; " (current-time-string) "\n\n")
    (prin1 `(setq cli-wrapper-history-alist ',cli-wrapper-history-alist) (current-buffer)))
  (message "CLI wrapper history saved to %s" cli-wrapper-history-file))


;; Function to load history
(defun cli-wrapper-load-history ()
  "Load CLI wrapper command history from a file."
  (interactive)
  (when (file-exists-p cli-wrapper-history-file)
    (condition-case err
        (load-file cli-wrapper-history-file)
      (error (message "Error loading CLI wrapper history: %s" err)))
    (message "CLI wrapper history loaded from %s" cli-wrapper-history-file)))



;; TODO - Mark the commands which has an option, instead of manually sorting it out

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

(defun extract-options-with-details-from-help (help-output)
  "Extract command line options from HELP-OUTPUT with details about each option.
Returns a list of plists with properties :name, :description, and :needs-value."
  (let ((options '())
        (lines (split-string help-output "\n"))
        (current-line 0)
        (total-lines (length (split-string help-output "\n"))))

    (while (< current-line total-lines)
      (let* ((line (nth current-line lines))
             (next-line (when (< (1+ current-line) total-lines)
                          (nth (1+ current-line) lines)))
             (option nil)
             (description "")
             (needs-value nil))

        ;; Match different option patterns
        (cond
         ;; Match pattern: -o, --option[=VALUE]
         ((string-match "\\s-+\\(-[a-zA-Z],\\s+\\)?\\(--[a-zA-Z0-9][-a-zA-Z0-9]*\\)\\(\\[?=[A-Z]+\\]?\\)?" line)
          (setq option (match-string 2 line))
          (when (match-string 3 line)
            (setq needs-value t))
          (setq description (replace-regexp-in-string "^\\s-+\\(\\(-[a-zA-Z],\\s+\\)?--[^\\s-]+\\([^\\s-]?=[^\\s-]+\\)?\\)\\s-*" "" line)))

         ;; Match pattern: --option (simple long flag)
         ((string-match "\\s-+\\(--[a-zA-Z0-9][-a-zA-Z0-9]*\\)\\s-+" line)
          (setq option (match-string 1 line))
          (setq description (replace-regexp-in-string "^\\s-+--[^\\s-]+\\s-*" "" line)))

         ;; Match pattern: -o (simple short flag)
         ((string-match "\\s-+\\(-[a-zA-Z]\\)\\s-+" line)
          (setq option (match-string 1 line))
          (setq description (replace-regexp-in-string "^\\s-+-[^\\s-]+\\s-*" "" line))))

        ;; If we found an option
        (when option
          ;; Look for additional description in next line if it's indented
          (when (and next-line (string-match "^\\s-+\\s-+\\([^-].*\\)" next-line))
            (setq description (concat description " " (match-string 1 next-line)))
            (setq current-line (1+ current-line)))

          ;; Check description for value indicators
          (when (or (string-match "<[^>]+>" description)
                    (string-match "\\bARG\\b" description)
                    (string-match "\\bVALUE\\b" description)
                    (string-match "\\bFILE\\b" description)
                    (string-match "\\bPATH\\b" description)
                    (string-match "\\bDIR\\b" description))
            (setq needs-value t))

          ;; Add to options list
          (push (list :name option
                      :description description
                      :needs-value needs-value)
                options)))

      (setq current-line (1+ current-line)))

    (nreverse options)))


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

(defun format-option-for-display (option-plist)
  "Format an option for display in completion."
  (let ((name (plist-get option-plist :name))
        (desc (plist-get option-plist :description))
        (needs-value (plist-get option-plist :needs-value)))
    (format "%s%s - %s"
            name
            (if needs-value "=VALUE" "")
            (if (> (length desc) 50)
                (concat (substring desc 0 47) "...")
              desc))))

(defun option-plist-from-display (display option-plists)
  "Get the original option plist from a display string."
  (let* ((name (car (split-string display " - ")))
         (base-name (car (split-string name "="))))
    (cl-find base-name option-plists :key (lambda (plist) (plist-get plist :name)) :test 'string=)))


(defun interactively-build-cli-command (cmd)
  "Build a command for CMD interactively based on its help output."
  (interactive "sCommand to wrap: ")
  (let* (
         ;; (directory (read-directory-name "Run in directory: " default-directory))
         (directory "~/src/main")
         (default-directory directory)
         (cmd-history (cli-wrapper-get-history cmd))
         (use-history (and cmd-history
                           (y-or-n-p "Use command history? ")))
         (help-output (shell-command-to-string (concat cmd " --help")))
         (options (extract-options-from-help help-output)) ;; TODO - remove once the plist is working
         (option-plists (extract-options-with-details-from-help help-output))
         (option-displays (mapcar #'format-option-for-display option-plists))
         (subcommands (extract-subcommands-from-help help-output))
         (selected-options '())
         (selected-subcommand nil)
         (additional-args nil))

    (if use-history
        ;; Choose from history
        (setq final-command
              (completing-read (format "%s command: " cmd)
                               cmd-history nil nil))

      ;;
      ;;; Else build the command
      ;;

      ;; First select the target
      (setq selected-subcommand
            (concat "-t "
                    (completing-read "Select the build target: " (my-get-build-target-list) nil t)))

      ;; Select options
      ;;; TODO - Could possibly move towards using completing-read-multiple ?
      (let ((option-display ""))
        (while (not (string= "done" (setq option-display
                                          (completing-read "Add option (RET to finish): "
                                                           `("done" ,@option-displays ) nil nil))))
          (let* ((option-plist (option-plist-from-display option-display option-plists))
                 (option-name (plist-get option-plist :name))
                 (needs-value (plist-get option-plist :needs-value))
                 (value ""))

            ;; Remove selected option from available options
            (setq option-displays
                  (delete option-display option-displays))

            ;; If option needs a value, prompt for it
            (when needs-value
              (setq value (read-string
                           (format "Value for %s: " option-name))))

            ;; Add to selected options
            (if (and needs-value (not (string= value "")))
                (push (format "%s=%s" option-name value) selected-options)
              (push option-name selected-options)))))


      ;; Ask for any additional arguments
      ;; (setq additional-args
      ;;       (read-string "Additional arguments: "))

      ;; Build the final command
      (let ((final-command (concat "rbuild"
                                   " -i buildmachine" ;; Where to remote build
                                   " -p ~/src/main" ;; Directory on remote builder
                                   (if selected-subcommand
                                       (concat " " selected-subcommand)
                                     "")
                                   (if selected-options
                                       (concat " -o " (string-join (reverse selected-options) " -o "))
                                     "")
                                   (if (not (string= additional-args ""))
                                       (concat " " additional-args)
                                     ""))))

        ;; Preview and confirm
        (let ((confirm (read-string
                        (format "Execute command (edit if needed):\n%s\n: " final-command)
                        final-command)))

          ;; Add to history
          (cli-wrapper-add-to-history cmd confirm)

          confirm)))))


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

(defun my-cisco-main-compile-command-advice (orig-fun &optional command comint)
  "Advice to make compile interactive."
  (interactive)
  ;; TODO - Add history
  (interactively-build-cli-command "build")) ;; require-match


;; Load history on initialization if available
(cli-wrapper-load-history)

(advice-add 'compilation-read-command :around #'my-cisco-main-compile-command-advice)

;; Example usage:
;; (create-cli-wrapper "git")
;; (global-set-key (kbd "C-c g") (create-cli-wrapper "git"))

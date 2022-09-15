
;;
;;; Have a simply utility function translate a buffer from markdown to JIRA
;;; markdown syntax, using pandoc
;;


(defun translate-buffer-to-jira-markdown ()
  (interactive)
  ;; Send stdin from the buffer to pandoc
  (call-process-region
   (point-min) ;; region-start
   (point-max) ;; region-end
   "pandoc"
   t ;; Delete buffer contents
   t ;; Insert into the current buffer
   t ;; Display output in buffer as it is inserted
   "--from=markdown+pipe_tables" "--to=jira"))

;; TODO - should probs be added to my private layer...

;; Add a shortcut to spacemacs user prefix o
(spacemacs/declare-prefix "oj" "jira-md")
(spacemacs/set-leader-keys
  "oj" 'translate-buffer-to-jira-markdown)

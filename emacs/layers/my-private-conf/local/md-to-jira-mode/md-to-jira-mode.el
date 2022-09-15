;;; INPROGRESS - bind to the kill-buffer-hook only for the given buffer (!)

;;;###autoload
(define-minor-mode md-to-jira-mode
  "Utility mode for never writing Jira markdown (yuk) again"
  :lighter " JIRA(MD)"
  :global nil

  ;;; Make the kill-buffer-hook buffer local
  (make-local-variable 'kill-buffer-hook)

  (add-hook 'kill-buffer-hook 'replace-buffer-contents-to-jira-markdown-on-save)
  )



;;;###autoload
(defun replace-buffer-contents-to-jira-markdown-on-save()
  "A utility function which should be called as a hook when closing the tridactyl interaction buffer.

And then transform the written markdown to Jira markdown using pandoc, and pandoc-mode."
  (interactive)
  ;;; Generate the Jira markdown from the markdown contents of the current buffer
  (pandoc-run-pandoc)


  ;;; Replace the contents of the current buffer with the Jira markdown
  (replace-buffer-contents pandoc--output-buffer-name)

  )

(provide 'md-to-jira-mode)


;; Add a hook for users to latch onto
(defvar stutter-mode-hook nil)

;; Define a custom mode-map
(defvar stutter-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Stutter minor mode")

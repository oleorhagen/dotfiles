
;; TODO - how to use this properly (?)
;;
            ;;; Should I add it to the major mode ?
;;
            ;;; Or custom keybindings
;;
            ;;; One option is to 'evilify' the logview-key map
;;
            ;;; Another one is: Manually overriding the map
            ;;; Using (evil-define-key)
;;

;; Use magit as inspiration
;;
;; (kbd "?") - should show all the keys

;; (evilified-state-evilify-map logview-mode-map
;;   :mode logview-mode
;;   :bindings
;;   ;; "j" 'logview-next-entry
;;   ;; "k" 'logview-previous-entry
;;   ;; "l" nil ;; <- Don't want to override this key (and some others)
;;   ;; "L" nil
;;   )



(defun log/change-keymap ()
  (interactive)

  ;; Add the leader key to the logview-mode-map
  (with-eval-after-load 'logview
    (define-key logview-mode-map
                (kbd dotspacemacs-leader-key) spacemacs-default-map))

              ;;; TODO - Add a hydra mode for this
              ;;; Define keymaps for evil emulation
  (with-eval-after-load 'logview
    (define-key logview-mode-map "/" 'evil-search-forward)
    (define-key logview-mode-map ":" 'evil-ex)
    ;; (define-key logview-mode-map "h" 'evil-backward-char)
    (define-key logview-mode-map "j" 'evil-next-visual-line)
    (define-key logview-mode-map "k" 'evil-previous-visual-line)
    ;; (define-key logview-mode-map "l" 'evil-forward-char)
    ;; (define-key logview-mode-map "n" 'evil-search-next)
    ;; (define-key logview-mode-map "N" 'evil-search-previous)
    ;; (define-key logview-mode-map "v" 'evil-visual-char)
    ;; (define-key logview-mode-map "V" 'evil-visual-line)
    ;; (define-key logview-mode-map "gg" 'evil-goto-first-line)
    (define-key logview-mode-map "G" 'evil-goto-line)
    (define-key logview-mode-map (kbd "C-f") 'evil-scroll-page-down)
    (define-key logview-mode-map (kbd "C-b") 'evil-scroll-page-up)
    (define-key logview-mode-map (kbd "C-e") 'evil-scroll-line-down)
    (define-key logview-mode-map (kbd "C-y") 'evil-scroll-line-up)
    (define-key logview-mode-map (kbd "C-d") 'evil-scroll-down)
    (define-key logview-mode-map (kbd "C-u") 'evil-scroll-up)
    (define-key logview-mode-map (kbd "C-o") 'evil-jump-backward)
    ))

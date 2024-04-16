;;; packages.el --- log layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author:  <olepor@olepor>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `log-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `log/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `log/pre-init-PACKAGE' and/or
;;   `log/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst log-packages
  '(

    ;; We only require logview-mode
    (logview :location (recipe
                        :fetcher github
                        :repo "doublep/logview"))

    ))


;;
;;; TODO - Unix log does not pass our levels... Fix!
;;

;; (defvar logview-unix-with-level
;;   '("MENDER" . ((format . "TIMESTAMP THREAD NAME: TIMESTAMP level=LEVEL MESSAGE")
;;                 (levels . "RFC 5424 lowercase"))))

;;
;;; TODO - The UX should be so that all filter creation and edits is under one key
;;;        as opposed to now, that it's under a,t,m etc
;;

(defvar logview-unix-with-level
  '("MENDER" . ((format . "TIMESTAMP THREAD NAME: IGNORED lvl=LEVEL")
                (levels . "RFC 5424 lowercase"))))

(defvar logview-mender-logs-format
  '("MENDER-LOGS" . ((format . "TIMESTAMP [LEVEL]: >> THREAD-NAME")
                     (levels . "RFC 5424"))
    ))


;; TODO - In progress = not tested
;; (defvar logview-unix-with-level
  ;; '("PYTEST" . ((format . "[THREAD] IGNORED NAME")
                ;; No levels set for the pytest module)))

;;(add-to-list 'logview-additional-submodes logview-unix-with-level)

;;
;;; TODO - Maybe add a logview transient mode for spacemacs (?)
;;

;;
;;; TODO - Should be able to switch between the evilified keymap and logviews keymap (!)
;;

(defun log/init-logview ()
  (use-package logview
    :defer t
    :config (progn

              ;;(add-to-list 'logview-additional-submodes logview-unix-with-level t)

              ;; (evil-set-initial-state 'logview-mode 'emacs)

              ;; (evil-define-key 'normal logview-mode-map (kbd "j") 'logview-next-entry)
              ;; (evil-define-key 'normal logview-mode-map (kbd "k") 'logview-previous-entry)
              )))

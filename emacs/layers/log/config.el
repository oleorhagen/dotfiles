
(defvar log/format-unix-with-level
  '("MENDER" . ((format . "TIMESTAMP THREAD NAME: IGNORED lvl=LEVEL")
                (levels . "RFC 5424 lowercase"))))

(defvar log/format-mender-logs-format
  '("MENDER-LOGS" . ((format . "TIMESTAMP [LEVEL]: >> THREAD-NAME")
                     (levels . "RFC 5424"))
    ))

;; TODO - Create a coloured point-marker

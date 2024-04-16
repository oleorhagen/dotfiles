

(spacemacs/declare-prefix
  "f" "filter"
  "l" "Show all levels"
  "n" "narrow"
  "t" "Edit threads"
  "v" "Views")

(spacemacs/set-leader-keys

  "fe" 'logview-edit-filters
  "fn" 'logview-add-include-name-filter
  "fN" 'logview-add-exclude-name-filter
  "ft" 'logview-add-include-thread-filter
  "fT" 'logview-add-exclude-thread-filter
  "fr" 'logview-reset-all-filters-restrictions-and-hidings
  "l" 'logview-show-all-levels

  ;; Narrow
  "nt" 'logview-narrow-to-thread
  "t" 'logview-edit-threads
  "w" 'logview-widen

  ;; TODO - Views
  "vs" 'logview-save-filters-as-view-for-submode
  "ve" 'logview-edit-submode-views
  "vd" 'logview-delete-submode-views
  "vn" 'logview-set-navigation-view
  "vh" 'logview-highlight-view-entries
  )

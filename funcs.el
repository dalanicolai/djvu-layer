;; (when (configuration-layer/package-used-p 'djvu)

(defun djvu-advise-image-toggle (orig-func &rest args)
  (djvu-image-toggle))

(defun djvu-fast-search (regexp)
  (interactive "sSearch (regexp): ")
  (when djvu-image-mode
    (djvu-image-toggle))
  (spacemacs/djvu-search-forward regexp))

(defun spacemacs/djvu-search-forward (query)
  "Search forward for match for REGEXP.

Search case-sensitivity is determined by the value of the variable
`case-fold-search', which see.

The command `djvu-search-forward-continue' continues to search forward."
  (interactive "sQuery: ")
  (setq djvu-last-search-re query)
  (while (not (or (search-forward query nil t)
                  (eq (djvu-ref page) (djvu-ref pagemax))))
    (djvu-goto-page (let ((page (djvu-ref page))
                          (return 1))
                      (while (and (not (= return 0)) (< page (+ (djvu-ref pagemax) 1)))
                        (setq return (call-process-shell-command
                                      (format "djvused %s -e 'select %s; print-pure-txt' | grep -i '%s'"
                                              (shell-quote-argument buffer-file-name)
                                              page
                                              query)))
                        (setq page (1+ page)))
                      (print page)))))

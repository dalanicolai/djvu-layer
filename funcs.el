(defun djvu-toggle-semi-continuous-scrolling ()
  (interactive)
  (setq djvu-semi-continuous-scrolling (if djvu-semi-continuous-scrolling
                                       nil
                                     t))
  (message "Djvu alternative scrolling %s" (if djvu-semi-continuous-scrolling
                                               "enabled"
                                             "disabled")))

(defun spacemacs/djvu-advise-image-toggle (orig-func &rest args)
  (djvu-image-toggle))

;; djvu-continuous of djvu.el does not work with djvu3.el
(defun spacemacs/djvu-scroll-up-or-next-page ()
  (interactive)
  (if (not djvu-semi-continuous-scrolling)
      (if djvu-image-mode
          (djvu-image-scroll-up)
        (evil-next-visual-line))
    (scroll-up-line 5)
    (when (= (window-vscroll) 0)
      (djvu-next-page 1))))

(defun spacemacs/djvu-scroll-down-or-previous-page ()
  (interactive)
  (if (not djvu-semi-continuous-scrolling)
      (if djvu-image-mode
          (djvu-image-scroll-down)
        (evil-previous-visual-line))
    (if (not (= (window-vscroll) 0))
        (scroll-down-line 5)
      (djvu-prev-page 1)
      (scroll-up-command))))

(defun spacemacs/djvu-fast-search (regexp)
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

(defun djvu-occur-next-entry-and-follow ()
  (interactive)
  (evil-next-visual-line)
  (call-interactively 'djvu-occur-show-entry))

(defun djvu-occur-previous-entry-and-follow ()
  (interactive)
  (evil-previous-visual-line)
  (call-interactively 'djvu-occur-show-entry))

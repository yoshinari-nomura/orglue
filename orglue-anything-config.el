(require 'orglue)

(defvar anything-c-source-org-projects
  '((name . "Org Projects")
    (candidates . orglue-get-org-project-names)
    (migemo)
    (action
     ("Pop To Org Node" .
      (lambda (candidate)
        (let (marker)
          ;; re-get text-property
          (setq candidate
                (car (member candidate (orglue-get-org-project-names))))
          (when (and candidate
                     (setq marker (get-text-property 0 'org-marker candidate)))
            (org-goto-marker-or-bmk marker))))))
    ))

(provide 'orglue-anything-config)

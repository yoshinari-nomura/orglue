;;; orglue-octopress.el -- Compose octopress articles using org-mode.

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;; Created: 2011-08-28
;; Revised: 

;;; Commentay:

;; Bassic settings:
;;
;; (setq orglue-octopress-staging-org-source  "~/prj/private/octopress/org_source")
;; (setq orglue-octopress-staging-source      "~/prj/private/octopress/source")
;; (setq orglue-octopress-staging-destination "~/prj/private/octopress/public")
;; (setq orglue-octopress-setup-file          "~/sys/lib/org-sty/octpress.org")
;; (orglue-octopress-setup-publish-project)
;;
;; M-x orglue-octopress
;;
;; Note:
;;  In octopress/_config.yml, you must set the permelink attribute:
;;    permalink: /blog/:year-:month-:day-:title.html
;;

;;; Code:

(require 'ctable)
(require 'orglue)

(defvar orglue-octopress-staging-org-source  "~/prj/private/octopress/org_source")
(defvar orglue-octopress-staging-source      "~/prj/private/octopress/source")
(defvar orglue-octopress-staging-destination "~/prj/private/octopress/public")
(defvar orglue-octopress-setup-file          "~/sys/lib/org-sty/octpress.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draft

(defun orglue-octopress-new-post (&optional title date)
  (interactive "sPermalink Text: ")
  (let ((date (or date (org-read-date))))
    (find-file (expand-file-name
                (orglue-octopress-new-post-file-name title date)
                (concat orglue-octopress-staging-org-source  "/blog")))
    (orglue-octopress-insert-export-options-template title date nil orglue-octopress-setup-file)))

(defun orglue-octopress-insert-export-options-template (title date category setupfile)
  (save-excursion
    (insert (format (concat
                     "#+TITLE: %s\n"
                     "#+DATE: %s\n"
                     "#+CATEGORY: %s\n"
                     "#+SETUPFILE: %s\n\n* ")
                    title date (or category "") setupfile)))
  (re-search-forward "#\\+TITLE: " nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Publishing

(defun orglue-octopress-setup-publish-project ()
  (let* ((org-top   orglue-octopress-staging-org-source)
         (jkl-top   orglue-octopress-staging-source)
         (jkl-posts (concat orglue-octopress-staging-source "/_posts"))
         (org-posts (concat orglue-octopress-staging-org-source "/blog")))
    (orglue-update-publish-project-alist
     'org-publish-project-alist
     `(
       ("jkl" :components ("org-to-jkl-posts" "org-to-jkl" "org-to-jkl-static"))
       ("org-to-jkl-posts"
        :base-directory ,org-posts
        :publishing-directory ,jkl-posts
        :base-extension "org"
        :recursive nil
        :exclude "/[^0-9][^/]+\\.org$" ;; XXXX
        :publishing-function orglue-publish-org-to-html-with-yaml
        :table-of-contents nil
        :headline-levels 4 
        :body-only t
        )
       ("org-to-jkl"
        :base-directory ,org-top
        :publishing-directory ,jkl-top
        :base-extension "org"
        :exclude "[0-9]+-[0-9]+-[0-9]+.*\\.org$"
        :recursive t
        :publishing-function org-publish-org-to-html
        :table-of-contents nil
        :headline-levels 4 
        :body-only t
        )
       ("org-to-jkl-static"
        :base-directory ,org-top
        :publishing-directory ,jkl-top
        :base-extension ".*"
        :exclude "^\\(attic\\|top\\|options\\|.*\\.org\\)$"
        :recursive t
        :publishing-function org-publish-attachment
        )
       ))))

(add-hook 'orglue-before-export-hook  'orglue-octopress-setup-publish-project)

;;
;; Add YAML front matter to HTML.
;;

(defun orglue-publish-org-to-html-with-yaml (plist filename pub-dir)
  "Publish an org file to HTML with YAML front matter for Jekyll."
  (let ((org-export-html-final-hook 'orglue-export-html-add-yaml-front-matter)
        (org-exporting-blog-property (orglue-get-property-from-org-file filename)))
    (org-publish-org-to-html plist filename pub-dir)))

(defun orglue-export-html-add-yaml-front-matter ()
  (save-excursion
    (goto-char (point-min))
    (insert "---\n")
    (insert "layout: post\n")
    (insert (format "date: %s\n"  (nth 0 org-exporting-blog-property)))
    (insert (format "categories: %s\n" (nth 1 org-exporting-blog-property)))
    (insert (format "title: %s\n" (nth 2 org-exporting-blog-property)))
    (insert "comments: true\n")
    (insert "---\n")))

;;
;; Summary
;;

(defun orglue-octopress (&optional title)
  (interactive)
  (let ((buf (get-buffer-create "Octpress"))
        (cp))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "   %s\n\n" (or title "Octopress")))
      (save-excursion
        (setq cp
              (ctbl:create-table-component-region
               :width nil :height nil
               :model
               (make-ctbl:model
                :column-model
                (list (make-ctbl:cmodel
                       :title "Date" :sorter 'ctbl:sort-string-lessp
                       :min-width 10 :align 'left)
                      (make-ctbl:cmodel
                       :title "Category" :align 'left
                       :sorter 'ctbl:sort-string-lessp)
                      (make-ctbl:cmodel
                       :title "Title" :align 'left
                       :min-width 40
                       :max-width 40))
                :data
                (cons '("-" "-" "*** Add New Entry ***" nil)
                      (mapcar
                       'orglue-get-property-from-org-file
                       (directory-files
                        (expand-file-name "blog" orglue-octopress-staging-org-source) t "^[0-9].*\\.org$"))))))))
    (ctbl:cp-add-click-hook cp (lambda ()
                                 (if (setq filename (nth 3 (ctbl:cp-get-selected-data-row cp)))
                                     (find-file filename)
                                   (call-interactively 'orglue-octopress-new-post))))
    (switch-to-buffer (ctbl:cp-get-buffer cp))))

;;
;; Helpers
;;

(defun orglue-octopress-sanitize-title (title)
  (replace-regexp-in-string "[\t ]+" "-" (downcase title)))

(defun orglue-octopress-new-post-file-name (title &optional date)
  (let ((time (if (stringp date) (org-read-date nil t date) date)))
    (format
     (format-time-string "%Y-%m-%d-%%s.org" time)
     (orglue-octopress-sanitize-title title))))

(provide 'orglue-octopress)

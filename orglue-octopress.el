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
;; (setq orglue-octopress-setup-file          "~/sys/lib/org-sty/octopress.org")
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

(mapc (lambda (x) (add-to-list 'org-export-inbuffer-options-extra x))
      '(
        ("OCTOPRESS_LAYOUT"     :octopress-layout)
        ("OCTOPRESS_PUBLISHED"  :octopress-published)
        ;; worthless?
        ;; ("OCTOPRESS_PERMALINK"  :octopress-permalink)
        ;; ("OCTOPRESS_CATEGORIES" :octopress-categories)
        ;; ("OCTOPRESS_CATEGORY"   :octopress-category)
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draft

(defun orglue-octopress-new-post (&optional title date)
  (interactive "sPermalink Text: ")
  (let ((date (or date (org-read-date))))
    (find-file (expand-file-name
                (orglue-octopress-new-post-file-name title date)
                (concat orglue-octopress-staging-org-source  "/blog")))
    (orglue-octopress-insert-export-options-template title date nil orglue-octopress-setup-file "true")))

(defun orglue-octopress-insert-export-options-template (title date category setupfile published)
  (save-excursion
    (insert (format (concat
                     "#+TITLE: %s\n"
                     "#+DATE: %s\n"
                     "#+CATEGORY: %s\n"
                     "#+SETUPFILE: %s\n"
                     "#+OCTOPRESS_PUBLISHED: %s\n\n *"
                     )
                    title date (or category "") setupfile published)))
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
    (insert (format "published: %s\n" (nth 3 org-exporting-blog-property)))
    (insert "comments: true\n")
    (insert "---\n")))

;;
;; Summary
;;

(defvar orglue-blog-summary-mode-map  nil
  "Keymap for `orglue-blog-summary-mode'.")

(defvar orglue-blog-summary-mode-hook nil)

(defun orglue-merge-keymap (keymap1 keymap2)
  (append keymap1
          (delq nil
                (mapcar
                 (lambda (x)
                   (if (or (not (consp x))
                           (assoc (car x) keymap1))
                       nil x))
                 keymap2))))

(if orglue-blog-summary-mode-map
    ()
  (setq orglue-blog-summary-mode-map (make-sparse-keymap))
  (define-key orglue-blog-summary-mode-map "w" 'orglue-octopress-new-post)
  (define-key orglue-blog-summary-mode-map "d" 'orglue-octopress-delete-post)
  (setq orglue-blog-summary-mode-map
        (orglue-merge-keymap orglue-blog-summary-mode-map ctbl:table-mode-map)))

(defun orglue-octopress-delete-post ()
  "Delete existing post."
  (interactive))

(defun orglue-blog-summary-command-list (symbols &optional keymap)
  (let (symbol keysym keystr docstr summary-list)
    (while (setq symbol (car symbols))
      (setq keysym (where-is-internal symbol (or keymap (current-local-map)) nil)
            keystr (if keysym (mapconcat 'key-description keysym ",") "No keybind")
            docstr (documentation symbol))
      (if docstr
          (setq summary-list (cons (format "%10s ... %s (%s)"
                                           keystr
                                           (car (split-string docstr "\n"))
                                           symbol)
                                   summary-list)))
      (setq symbols (cdr symbols)))
    summary-list))

(defun orglue-blog-summary-mode ()
  "Major mode for listing and controlling org-mode based blog articles.

\\{orglue-blog-summary-mode-map}"
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map orglue-blog-summary-mode-map)
  (setq major-mode 'orglue-blog-summary-mode
        mode-name  "Orglue-Blog")
  (setq buffer-undo-list t
        buffer-read-only t)
  (run-hooks 'orglue-blog-summary-mode-hook))

(defun orglue-blog-summary-header (&optional title)
  (concat
   (format "%s\n" (or title "Octopress"))
   (mapconcat
    'identity
    (orglue-blog-summary-command-list
     (remove-duplicates
      (mapcar 'cdr (cdr orglue-blog-summary-mode-map)))
     orglue-blog-summary-mode-map)
    "\n")
   "\n\n\n"))

(defun orglue-octopress (&optional title)
  (interactive)
  (let ((buf (get-buffer-create "Octpress"))
        (cp)
        (param (copy-ctbl:param ctbl:default-rendering-param)))
    (switch-to-buffer buf)
    (setq buffer-read-only nil)
    (setf (ctbl:param-fixed-header param) t)
    (erase-buffer)
    (insert (orglue-blog-summary-header title))
    (save-excursion
      (setq cp
            (ctbl:create-table-component-region
             :param param
             :width  nil
             :height nil
             :keymap orglue-blog-summary-mode-map
             :model
             (make-ctbl:model
              :data   (orglue-blog-scan)
              :sort-state '(2 1)
              :column-model
              (list (make-ctbl:cmodel
                     :title "Date"
                     :sorter 'ctbl:sort-string-lessp
                     :min-width 10
                     :align 'left)
                    (make-ctbl:cmodel
                     :title "Category"
                     :align 'left
                     :sorter 'ctbl:sort-string-lessp)
                    (make-ctbl:cmodel
                     :title "Title"
                     :align 'left
                     :min-width 40
                     :max-width 140))))))
    (ctbl:cp-add-click-hook
     cp
     (lambda ()
       (find-file (nth 4 (ctbl:cp-get-selected-data-row cp)))))
    (orglue-blog-summary-mode)
    (ctbl:navi-goto-cell
     (ctbl:find-first-cell (ctbl:component-dest cp)))
    ))

(defun orglue-blog-scan ()
  (mapcar
   'orglue-get-property-from-org-file
   (directory-files
    (expand-file-name "blog" orglue-octopress-staging-org-source) t "^[0-9].*\\.org$")))


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

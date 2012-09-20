;;; orglue.el -- more functionality to org-mode.

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;; Created: 2012-08-28
;; Revised: 

;;; Commentay:

;;; Code:
;;;; Require
(require 'org)
(require 'org-publish)
(require 'org-mac-link-grabber) ;; found in org-mode/contrib
(require 'epic) ;; https://github.com/yoshinari-nomura/epic

;;;; OMLG (Org Mac Link Grabber) Everywhere
(defun orglue-normalize-webpage-url (url-string)
  "Make clean URL; for example:
  Removing strings... from http://www.amazon.co.jp/strings.../dp/ASIN"
  (save-match-data
    (cond
     ;; make plain link to amazon.
     ((string-match "^\\(https?://www\\.amazon\\.[^/]+/\\).*\\(dp/.\\{10\\}\\).*" url-string)
      (concat (match-string 1 url-string) (match-string 2 url-string)))
     (t
      url-string))))

(defun orglue-normalize-webpage-title (title-string)
  "Nomalize title string of web pages; for example:
  Remove ``Amazon.com: '' from URL title"
  (save-match-data
    (cond
     ((string-match "^Amazon\\.\\(co\\.jp\\|com\\)[：:] *\\(.*\\)" title-string)
      (match-string 2 title-string))
     (t
      title-string))))

(defun orglue-decompose-org-bracket-link (link-string)
  (save-match-data
    (if (string-match org-bracket-link-regexp link-string)
        ;; 1: url, 3: anchor string
        (list (orglue-normalize-webpage-title (or (match-string 3 link-string) ""))
              (orglue-normalize-webpage-url   (or (match-string 1 link-string) "")))
      (list "" ""))))

(defun orglue-decompose-last-org-bracket-link ()
  (interactive)
  (if (save-excursion (re-search-backward org-bracket-link-regexp nil t))
      (progn
        (insert
         (apply 'format "%s\n%s\n" (orglue-decompose-org-bracket-link (match-string 0))))
        (delete-region (match-beginning 0) (match-end 0)))
    (message "No bracket-link found.")))

(defadvice omlg-grab-link (after omlg-grab-link-advice)
  (unless (eq major-mode 'org-mode)
    (orglue-decompose-last-org-bracket-link)))

(ad-activate 'omlg-grab-link)

;;;; Indent
(defun orglue-indent-rigidly-to-current-level (start end arg)
  "If called with C-u prefix (= arg 4) in org-mode buffer,
indent to fit the current outline level. Otherwise, do ``indent-rigidly''."
  (interactive "r\np")
  (indent-rigidly 
   start
   end
   (if (and (eq major-mode 'org-mode)
            (not (org-before-first-heading-p))
            (= arg 4))
       (- (1+ (nth 0 (org-heading-components)))
          (orglue-indent-base-column start end))
     arg)))

(global-set-key "\C-x\C-i" 'orglue-indent-rigidly-to-current-level)

(defun orglue-indent-base-column (start end)
  (let ((base-indent 1000))
    (save-match-data
      (save-excursion
        (goto-char start)
        (unless (bolp) (forward-line 1))
        (while (< (point) end)
          (unless (looking-at "^[ \t]*$")
            (setq base-indent (min base-indent (current-indentation))))
          (forward-line 1)))
      (if (= base-indent 1000) 0 base-indent))))

(defadvice org-indent-line (around org-indent-line-advice)
  (let ((org-in-item-p-orig (symbol-function 'org-in-item-p)))
    (flet ((org-in-item-p ()
                          (or (funcall org-in-item-p-orig)
                              (save-excursion 
                                (beginning-of-line 0)
                                (funcall org-in-item-p-orig)))))
      ad-do-it)))

(ad-activate 'org-indent-line)

;;;; Easy setup for publish-project to HTML
(defun orglue-update-publish-project-alist (project-alist-var projects)
  (let ((project-names (mapcar 'car projects))
        (project-alist (symbol-value project-alist-var)))
    ;; remove old projects from project-alist
    (mapc
     (lambda (prj)
       (setq project-alist
             (delete (assoc prj project-alist) project-alist)))
     project-names)
    ;; add new projects to project-alist
    (set project-alist-var (append projects project-alist))))

(defun orglue-publish-setup-current-project ()
  "+ Set standard directory layout for a project :: 
  + org/       ... project top (any arbitrary name is OK)
    + *.org    ... org files
    + dat/     ... static attachments linked from *.org
    + dyn/     ... dinamically generated files from *.org
    + pub/     ... files only needed on publishing
      + css/   ... style sheets
      + top/   ... top-level dot files like a .htaccess
    + options/ ... org-mode options (not copied on publishing)
  + html/      ... destination to publish.

+ oprations to publish :: 
  1) find-file TOP/org/index.org
  2) M-x orglue-publish-setup-current-project to register the TOP.
  3) C-c C-e P to publish files to TOP/html/

+ Note :: 
  All org/*.org files will be converted into html/*.html files.
  this is a non-recursive operation due to the css linking problem.
  http://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html

  All other files in org/ will be copied into html/ preserving
  their relaive positons in the directory tree.

  Directories named ``attic'' will be ignored.

  You must make the necessary directories beforehand.
  
+ Clean up your published files :: 

  Since every file in html/ can be reproduced from the other files,
  you can clean up html/ like:
  : rm -rf html ; mkdir html

  In addition, in case their cache files seem to be harmful:
  : rm -f  ~/.org-timestamps
"
  (interactive)
  (let* ((src (directory-file-name (file-truename default-directory)))
         (top (directory-file-name (file-name-directory src)))
         (dst (expand-file-name "html" top)))
    ;; current-file: /foo/project/org/index.org
    ;;   top => /foo/project
    ;;   src => /foo/project/org
    ;;   dst => /foo/project/html
    (orglue-update-publish-project-alist
     'org-publish-project-alist
     `(
       ("current" :components
        ("current-org" "current-static" "current-static-top")
        )
       ("current-org" ;; SRC/*.org -> DST/html/
        :base-directory ,src
        :base-extension "org"
        :publishing-directory ,dst
        :recursive nil
        :publishing-function org-publish-org-to-html
        )
       ("current-static" ;; SRC/**/* -> DST/html/
        :base-directory ,src
        :base-extension ".*"
        :exclude "^\\(attic\\|top\\|options\\|.*\\.org\\)$"
        :publishing-directory ,dst
        :recursive t
        :publishing-function org-publish-attachment
        )
       ("current-static-top" ;; SRC/pub/top -> DST/html/
        :base-directory ,(concat src "/pub/top")
        :base-extension ".*"
        :include (".htaccess")
        :publishing-directory ,dst
        :recursive nil
        :publishing-function org-publish-attachment
        )
       ))
    (message "PUBLISH %s => %s" src dst)))

(defadvice org-publish-current-project
  (around org-publish-current-project-advice)
  "open published file in browser after ``org-publish-current-project''."
  ad-do-it
  (org-open-file (expand-file-name
                  (concat (file-name-sans-extension (file-name-nondirectory (buffer-file-name))) ".html")
                  (plist-get (cdr (assoc (car (org-publish-get-project-from-filename (buffer-file-name)))
                                         org-publish-project-alist))
                             :publishing-directory))))

(ad-activate 'org-publish-current-project)

(defvar orglue-before-export-hook nil
  "Hook for org-export attached by advice.
This hook is useful for settingup org-publish-project-alist before ``org-publish-current-project''")

(defadvice org-export (before org-export-advice)
  "setup org-publish-project-alist before ``org-publish-current-project'' etc."
  (run-hooks 'orglue-before-export-hook)
  (if (not (org-publish-get-project-from-filename (buffer-file-name)))
      (orglue-publish-setup-current-project)))

(ad-activate 'org-export)

;;;; Table
(defun org-table-renumber ()
  (interactive)
  (let* ((col (org-table-current-column))
         (val (org-table-get nil nil))
         (num (string-to-number val))
         (fmt (format "%%0%dd" (length val)))
         (count 0))
    (save-excursion
      (while (org-at-table-p)
        (org-table-goto-column col)
        (setq val (org-table-get nil nil))
        ;; if val is in the form of NUMBER, re-number it.
        (if (string-match "^[0-9]+$" val)
            (progn (org-table-put nil nil (format fmt (+ num count)) t)
                   (setq count (1+ count))))
        (beginning-of-line 2)  ;; down 1 line
        ))
    (message "Replaced %d number(s)." count)))

;;;; Evernote

;; Add link type.
;; C-cC-o (org-open-at-point) works on evernote:// links.

(org-add-link-type "evernote" 'orglue-evernote-open)

(defun orglue-evernote-open (path)
  (browse-url (concat "evernote:" path)))

(defun orglue-evernote-insert-selected-note-as-org-links ()
  "Capture selected notes in Evernote, and insert org-style links."
  (interactive)
  (insert (orglue-zipup-to-org-links
           (epic-selected-note-uris)
           (epic-selected-note-titles))))

(defun orglue-evernote-create-note-from-org-buffer ()
  (interactive)
  (let* ((htmlize-output-type 'font)
         (org-export-html-xml-declaration nil)
         (org-file (buffer-file-name))
         (org-plist (org-combine-plists
                     (org-default-export-plist)
                     (org-infile-export-plist)))
         (org-title (or (plist-get org-plist :title) "")))
    (message "%s"
             (epic-create-note-from-html-string
              (org-export-region-as-html (point-min) (point-max) nil 'string)
              org-title
              nil
              nil
              org-file
              ))))

;;;; DnD to Org buffer
(define-key global-map [ns-drag-file] 'orglue-ns-insert-file)
(define-key global-map [ns-drag-text] 'orglue-ns-insert-text)

(defun orglue-ns-insert-file ()
  (interactive)
  (let ((file (file-truename (car ns-input-file))))
    (setq ns-input-file (cdr ns-input-file))
    (if (eq major-mode 'org-mode)
        (insert (orglue-convert-file-to-org-link file))
      (dnd-handle-one-url
       (get-buffer-window)
       'copy
       (concat "file://" file)))))

(defun orglue-ns-insert-text ()
  (interactive)
  (dnd-insert-text
   (get-buffer-window)
   'copy
   (if (eq major-mode 'org-mode)
       (orglue-convert-text-to-org-link ns-input-text)
     ns-input-text)))

;;;; Import images with some modification
(defvar orglue-image-store-directory "dat/img")
(defvar orglue-image-store-resize '(("jpg" . "800x600")))

(defun orglue-screencapture-and-insert ()
  (interactive)
  (let ((directory (file-name-directory (buffer-file-name)))
        (filename (format-time-string "img/screencapture-%Y%m%dT%H%M%S.png")))
    (call-process "screencapture"
                  nil nil nil "-i" "-P" (expand-file-name filename directory))
    (insert (format "[[file:%s]]" filename))
    (org-display-inline-images
     nil
     (point-min) (point-max))))

(defun orglue-modify-path (path &optional dest-directory new-suffix)
  (let* ((dir  (file-name-directory path))
         (file (file-name-nondirectory path))
         (node (file-name-sans-extension file))
         (ext  (file-name-extension file)))
    (expand-file-name
     (format "%s.%s" node (or new-suffix ext))
     (or dest-directory dir))))

(defun orglue-confirm-files (src dst &optional overwrite)
  (let* ((src (expand-file-name src))
         (dst (expand-file-name dst))
         (dir (file-name-directory dst)))
    (unless (file-exists-p src)
      (error "Source file does not exist (%s)" src))
    (unless (file-accessible-directory-p dir)
      (error "Destination directory is not accessible (%s)" dir))
    (when (file-equal-p src dst)
      (error "Source and destination are identical (%s)" src))
    (when (and (file-exists-p dst) (not overwrite))
      (error "Destination file already exists (%s)" dst)))
  t)

(defun orglue-import-image (path &optional dest-directory new-suffix geometry)
  (let* ((new-path (orglue-modify-path path dest-directory new-suffix))
         (default-opt '("-strip"))
         (geom-opt (and geometry (list "-geometry" geometry))))
    (orglue-confirm-files path new-path)
    (with-output-to-string
      (with-current-buffer standard-output
        (apply 'call-process "convert" nil '(t t) nil
               (append default-opt geom-opt (list path new-path)))))))

;;;; Misc functions for manipulation of org structure

(defvar orglue-org-project-file "~/prj/private/org/TODO.org")

(defun orglue-get-org-project-names (&optional org-file-path match)
  (let ((org-file-path (or org-file-path orglue-org-project-file))
        (match (or match "LEVEL=2+PROJECT")))
    (org-map-entries (lambda ()
                       (let ((string (org-link-display-format (nth 4 (org-heading-components)))))
                         (put-text-property 0 1 'org-marker
                                            (move-marker (make-marker) (point) (current-buffer)) string)
                         string))
                     match (list  org-file-path))))

(defun orglue-make-link-to-project-top (&optional pom)
  (let ((project-top (org-entry-get (or pom (point)) "PROJECT_TOP")))
    (if project-top
        (org-make-link-string (concat "file:" project-top) "TOP")
      "   ")))

(defun orglue-goto-id (id-string)
  "ID-STRING is like: E47A9659-AC45-45B8-B4BD-520AD180A2B3"
  (interactive)
  (goto-char
   (org-find-entry-with-id id-string))
  (message (org-get-heading t t)))

(defun orglue-headline-string ()
  (interactive)
  (message "head: %s\n"
           (plist-get (org-fix-agenda-info
                       (text-properties-at 0 (org-current-line-string))) 'txt)))

(defun orglue-octopress-get-date-from-filename (filename)
  (let ((fn (file-name-nondirectory filename)))
    (if (string-match "^[0-9]+-[0-9]+-[0-9]+" fn)
        (match-string 0 fn)
      (format-time-string "%Y-%m-%d %H:%m" (current-time)))))

(defun orglue-get-property-from-org-file (filename)
  (with-temp-buffer
    (let ((default-directory (file-name-directory filename))
          plist title category date)
      (insert-file filename)
      (org-mode)
      (setq plist (org-infile-export-plist)
            title (or (plist-get plist :title) "Untitled")
            published (or (plist-get plist :octopress-published) "true")
            category (org-get-category)
            date (orglue-octopress-get-date-from-filename filename))
      (list date (if (string= category "???") "" category) title published filename))))


;;;; Org-suitable link comporser

(defun orglue-convert-file-to-org-link (path)
  (let* ((path (file-truename path))
         (dir  (file-name-directory path))
         (file (file-name-nondirectory path))
         (node (file-name-sans-extension file))
         (ext  (downcase (file-name-extension file))))
    (if (boundp 'orglue-image-store-directory)
        (progn
          (orglue-import-image
           path
           orglue-image-store-directory 
           ext
           (cdr (assoc ext orglue-image-store-resize)))
          (concat "#+CAPTION: \n"
                  (format "#+ATTR_HTML: alt=\"%s\"\n" node )
                  (format "[[file:%s/%s.%s]]\n" orglue-image-store-directory node ext node)))
      (format "[[file://%s][%s]]\n" path node))))

(defun orglue-convert-text-to-org-link (text)
  (cond
   ((string-match "^evernote:" ns-input-text)
    (orglue-zipup-to-org-links (split-string text " ") (epic-selected-note-titles)))
   (t
    text)))

(defun orglue-zipup-to-org-links (uris titles)
  "Take two lists and zip up them to be org-style links like:
    [[URI1][TITLE1]] LF [[URI2][TITLE2]]..."
  (let ((result ""))
    (while (and (car uris) (car titles))
      (setq result
            (concat result (format "[[%s][%s]]\n" (car uris) (car titles))))
      (setq uris   (cdr uris))
      (setq titles (cdr titles)))
    result))

(provide 'orglue)

;;; Copyright Notice:

;; Copyright (C) 2011, 2012 Yoshinari Nomura.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; orglue.el ends here

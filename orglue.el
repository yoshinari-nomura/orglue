;;; orglue.el --- more functionality to org-mode.

;; Copyright (C) 2011, 2012 Yoshinari Nomura.
;; All rights reserved.

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;; Created: 2012-08-28
;; Version: 1.0
;; Package-Requires: ((org "8.1") (epic "0.2") (org-mac-link "1.2"))
;; Keywords: org

;;; Commentary:
;;
;;; Code:
;;

;;;; Require

(require 'org)

(when (featurep 'ns)
  (require 'org-mac-link) ;; found in org-mode/contrib
  (require 'epic)         ;; https://github.com/yoshinari-nomura/epic
  )

;;;; Org Mac Link Grabber Everywhere

(defun orglue-normalize-webpage-url (url-string)
  "Make clean URL; for example:
  Removing ``strings...'' from http://www.amazon.co.jp/``strings...''/dp/ASIN"
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

(when (fboundp 'org-mac-grab-link)
  (defadvice org-mac-grab-link (after org-mac-grab-link-advice)
    (unless (eq major-mode 'org-mode)
      (orglue-decompose-last-org-bracket-link)))
  (ad-activate 'org-mac-grab-link))

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
;; Org-mode becomes to recognize evernote:// links
(eval-after-load 'org
  '(if (and (boundp 'org-link-protocols)
            (not (assoc "evernote" org-link-protocols)))
       (org-add-link-type "evernote" 'orglue-org-evernote-note-open)))

;; C-cC-o (org-open-at-point) works on evernote:// links.
(defun orglue-org-evernote-note-open (path)
  (browse-url (concat "evernote:" path)))

(defalias 'orglue-evernote-insert-selected-note-as-org-links
  'epic-insert-selected-note-as-org-links)

(defalias 'orglue-evernote-create-note-from-org-buffer
  'epic-create-note-from-org-buffer)

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
(defvar orglue-image-store-resize '(("jpg" . "480x480>")))

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
      (unless (yes-or-no-p "Destination file already exists. Overwrite? ")
        (error "Destination file already exists (%s)" dst)))
    t))

(defun orglue-import-image (path &optional dest-directory new-suffix geometry)
  (let* ((new-path (orglue-modify-path path dest-directory new-suffix))
         (default-opt '("-strip"))
         (geom-opt (and geometry (list "-resize" geometry))))
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
    (org-map-entries
     (lambda ()
       (let ((string (org-link-display-format (nth 4 (org-heading-components)))))
         (put-text-property
          0 1 'org-marker
          (move-marker (make-marker) (point) (current-buffer)) string)
         string))
     match (list  org-file-path))))

(defun orglue-make-link-to-project-top (&optional pom)
  (let ((project-top (org-entry-get (or pom (point)) "PROJECT_TOP")))
    (if project-top
        (org-make-link-string (concat "file:" project-top) "TOP")
      "   ")))

(defun orglue-headline-string ()
  (interactive)
  (message "head: %s\n"
           (plist-get (org-fix-agenda-info
                       (text-properties-at 0 (org-current-line-string))) 'txt)))


;;;; Org-suitable link comporser

(defun orglue-convert-file-to-org-link (path)
  (let* ((path (file-truename path))
         (dir  (file-name-directory path))
         (file (file-name-nondirectory path))
         (node (file-name-sans-extension file))
         (ext  (downcase (or (file-name-extension file) ""))))
    (if (and (boundp 'orglue-image-store-directory)
             (assoc ext orglue-image-store-resize))
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
;;
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

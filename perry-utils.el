;;; perry-utils.el --- perry sitebuilder utility functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Alex - QWxleA

;; Author: Alex - QWxleA <QWxleA@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun perry--org-kwds ()
  "parse the buffer and return a cons list of (property . value)
from lines like:
#+PROPERTY: value"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (keyword) (cons (org-element-property :key keyword)
                            (org-element-property :value keyword)))))

(defun perry--org-kwd (KEYWORD)
  "get the value of a KEYWORD in the form of #+KEYWORD: value"
  (cdr (assoc KEYWORD (my/org-kwds))))

(defun perry--get-title ()
  "Get the title of org file."
  (or (perry--org-kwd "TITLE")
      (file-name-sans-extension (buffer-name))))

(defun perry--get-descr ()
  "Get the title of org file."
  (or (perry--org-kwd "DESCRIPTION")
      "no DESCRIPTION"))

(defun perry--get-category (org-file)
  "Get the keyword(s) of org file."
  (or (perry--org-kwd "CATEGORY")
      "no CATEGORY"))

(defun perry--nice-date (perry--info)
  (or (replace-regexp-in-string "[<>]" "" (perry--org-kwd "DATE"))
      (perry--org-kwd "DATE")))

(defun perry--get-template-path (perry--info)
  "Returns a list containing the template-path."
  (let ((theme--info (cdadr (plist-get perry--info :theme))))
    (list (expand-file-name (file-name-as-directory "templates")
                            (expand-file-name
                             (plist-get theme--info :loc)
                             (plist-get perry--info :themedir))))))

(defun perry--get-template (template perry--info)
  "Returns TEMPLATE from theme directory."
  (concat  (car (perry--get-template-path perry--info)) (concat template ".mustache")))

;;http://ergoemacs.org/emacs/elisp_read_file_content.html#8221
(defun perry-file-to-string (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun perry--string-to-file (string file &optional mode)
  "Write STRING into FILE, only when FILE is writable. If MODE is a valid major
mode, format the string with MODE's format settings."
  (when (file-writable-p file)
    (with-temp-buffer
      (insert string)
      (set-buffer-file-coding-system 'utf-8-unix)
      (when (and mode (functionp mode))
        (funcall mode)
        (flush-lines "^[ \\t]*$" (point-min) (point-max))
        (delete-trailing-whitespace (point-min) (point-max))
        (indent-region (point-min) (point-max)))
      (write-region (point-min) (point-max) file))))

;;FIXME - remove setq
(defun perry--get-new-file-loc (file perry--info)
  "Returns the location of a to be exported post or page. If needed it will create a directory"
  (let* ((dirbeforefile (cadr (reverse (split-string file "/"))))
         (newfname (concat (file-name-base file) ".html"))
         (exportloc (expand-file-name (plist-get perry--info :exportslug) (plist-get perry--info :docroot))))
    (if (equal dirbeforefile "pages")
        (setq exportfile (expand-file-name newfname exportloc) )
      (unless (file-directory-p (expand-file-name dirbeforefile exportloc))
        (make-directory (expand-file-name dirbeforefile exportloc) t))
      (setq exportfile (expand-file-name newfname (expand-file-name dirbeforefile exportloc))))))

(defun perry--get-file-path ()
  "Return path to the file currently selected in a tabulated list"
  (save-excursion
    (move-beginning-of-line nil)
    (get-text-property (point) 'help-echo)));help-echo is also the file path

(defun perry--get-new-url (file perry--info)
  "return new-url for FILE. PERRY--INFO is used todo."
  (concat (file-name-as-directory "posts")
          (concat (file-name-base file) ".html")))

(provide 'perry-utils)
;;; perry-utils.el ends here

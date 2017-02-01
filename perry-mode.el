;;; perry-mode.el --- abbrev and snippets        -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Alex - QWxleA

;; Author: Alex - QWxleA <QWxleA@gmail.com>
;; Keywords: convenience, tools

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

;;FIXME - unite list -posts and -pages
(defun perry--list-all-posts (perry--info)
  "todo pery--info see org-mode"
  (let* ((perry--files
          (file-expand-wildcards (expand-file-name "*.org"  (expand-file-name "posts" (plist-get perry--info :docroot)))))
         entries)
    (dolist (file perry--files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((title (propertize (perry--get-title) 'help-echo file))
              (author (or (perry--org-kwd "AUTHOR")
                          user-full-name "Unknown Author"))
              (description  (or (perry--org-kwd "DESCRIPTION")
                                "No DESCRIPTION"))
              (category (perry--get-category file)))
          (push (list nil (vector title author description category)) entries))))
    (reverse entries)))

(defun perry--list-all-pages (perry--info)
  "todo"
  (let* ((perry--files
          (file-expand-wildcards (expand-file-name "*.org" (expand-file-name "pages" (plist-get perry--info :docroot)))))
         entries)
    (dolist (file perry--files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((title (propertize (perry--get-title) 'help-echo file))
              (description  (or (perry--org-kwd "DESCRIPTION")
                                "No DESCRIPTION")))
          (push (list nil (vector title description)) entries))))
    (reverse entries)))

(defun perry-mode-edit ()
  "Edit selected post or page"
  (interactive)
  (find-file (perry--get-file-path)))

(defun perry-mode-preview (perry--info)
  "Preview the current post / page."
  (interactive)
  (let* ((file (perry--get-file-path))
         (httpd-port (plist-get perry--info :web-server-port))
         (parentdir (cadr (reverse (split-string file "/"))))
         (curpage (concat (file-name-base file) ".html")))
    (perry-publish-file file (perry--get-new-file-loc file perry--info) perry--info)
    (httpd-serve-directory (concat (file-name-as-directory (plist-get perry--info :docroot)) (plist-get perry--info :exportslug)))
    (unless (equal parentdir "pages")
      (setq curpage (concat (file-name-as-directory parentdir) curpage)))
    (browse-url (format "http://%s:%d/%s" "localhost" httpd-port curpage))))

(defun perry--delete-post (perry--info)
  (let* ((file (perry--get-file-path)) 
         (htmlpage (perry--get-new-file-loc file perry--info)))
    (when (y-or-n-p (message "Deleting: %s and %s, are you sure?"
                             (file-name-nondirectory file)
                             (file-name-nondirectory htmlpage))) 
      (if (file-exists-p file)
          (delete-file file))
      (if (file-exists-p htmlpage)
          (delete-file htmlpage))
      (perry--populate-list perry--info))))

(defun perry--insert-options-template (&optional title category tags description)
  "Insert a template into current buffer with information for exporting.
TITLE: the title of this post
CATEGORY: the category(-ies) of this post, should be separated by comma and space
TAGS: the tags of this post, should be separated by comma and space
DESCRIPTION: the description of this post, it will be displayed in RSS feed
Note that this function does not verify the input parameters, it is users'
responsibility to guarantee these parameters are valid." 
  (if (not (bolp)) (newline))
  (insert (format
           "#+TITLE:       %s
#+AUTHOR:      %s
#+EMAIL:       %s
#+DATE:        %s
#+CATEGORY:    %s
#+TAGS:        %s
#+LANGUAGE:    %s
#+OPTIONS:     H:%d num:%s toc:%s \\n:%s ::%s |:%s ^:%s -:%s f:%s *:%s <:%s
#+DESCRIPTION: %s\n\n"
           (if (string= title "") (buffer-name) title)
           (user-full-name)
           user-mail-address
           (format-time-string (substring (car org-time-stamp-formats) 1 -1)) 
           (if (string= category "") "<TODO: insert your category here>" category)
           (if (string= tags "") "<TODO: insert your tags here>" tags)
           org-export-default-language
           org-export-headline-levels
           nil ;; org-export-with-section-numbers
           nil ;; org-export-with-toc
           org-export-preserve-breaks 
           org-export-with-fixed-width
           org-export-with-tables
           nil ;; org-export-with-sub-superscripts
           nil ;; org-export-with-special-strings
           org-export-with-footnotes
           org-export-with-emphasize
           org-export-with-timestamps
           (if (string= description "") "<TODO: insert your description here>" description))))

(defun perry-new-post (perry--info)
  "Wrapper function to create a new blog-post, calls perry--setup-post"
  ;;(call-interactively 'perry--setup-post "makesomethingup" (plist-get perry--info :default-category))
  (call-interactively 'perry--setup-post))

(defun perry--setup-post (&optional title category tags description)
  "Setup a new post.
TITLE: the title of this post
CATEGORY: the category(-ies) of this post, should be separated by comma and space
TAGS: the tags of this post, should be separated by comma and space
DESCRIPTION: the description of this post, it will be displayed in RSS feed
Note that this function does not verify the category and filename, it is users'
responsibility to guarantee the two parameters are valid."
  (interactive (let* ((title (read-string "Title: " "new-post" nil))
                      (category (read-string "Category: " (plist-get perry--info :default-category) nil))
                      (tags (read-string "Tags (comma seperated)" (plist-get perry--info :default-tags) nil))
                      (description (read-string "Description: ")))
                 (list title category tags description)))
  (if (string= category "")
      (setq category (plist-get perry--info :default-category)))
  (if (string= title "")
      (setq title "new-post.org"))
  (setq filename (replace-regexp-in-string " " "-" title))
  (unless (string-suffix-p ".org" filename)
    (setq filename (concat filename ".org")))
  (setq path (expand-file-name filename (expand-file-name "posts" (plist-get perry--info :docroot))))
  (switch-to-buffer (find-file path))
  (unless (file-exists-p path)
    (perry--insert-options-template title category tags description) 
    (save-buffer))
  (with-current-buffer (format "*perry-manager [%s]*" perry-curproj)
    (perry--populate-list perry--info)))

(defun perry-mode-toggle-pp (perry--info)
  (interactive)
  "Toggle between showing posts or pages"
  (if (eq perry--mode-toggle t)
      (setq perry--mode-toggle nil)
    (setq perry--mode-toggle t))
  (perry--populate-list perry--info))

(defun perry--populate-list (perry--info)
  "Populate the list with either posts or pages, depending on perry-mode-toggle
perry-info is used to pass around the current projects configuration"
  (if (eq perry--mode-toggle t)
      (progn
        (setq tabulated-list-format [("title" 30 nil)
                                     ("author" 20 t)
                                     ("description" 50 t)
                                     ("category" 10 t)]
              tabulated-list-entries (perry--list-all-posts perry--info)))
    (setq tabulated-list-format [("title" 30 nil)
                                 ("description" 60 t)]
          tabulated-list-entries (perry--list-all-pages perry--info)))
  (tabulated-list-init-header)
  (tabulated-list-print t))

;;FIXME, why doI need arg?
(defun perry-switch-blog (arg)
  "Switch over to other defined blog"
  (interactive
   (list
    (completing-read "Choose one: " (mapcar #'car perry--rc))))
  (setq perry-curproj arg)
  (perry-manager perry-curproj))

(define-derived-mode perry-mode tabulated-list-mode "perry-mode"
  "mode for managing blog posts and pages with perry"
  (setq perry--info (cdr (assoc perry-curproj perry--rc)))
  (perry--populate-list perry--info)

  (define-key perry-mode-map (kbd "s") (lambda () (interactive) (perry-mode-toggle-pp perry--info))) 
  (define-key perry-mode-map (kbd "S") 'perry-switch-blog)
  (define-key perry-mode-map (kbd "e") 'perry-mode-edit)
  (define-key perry-mode-map (kbd "<RET>") 'perry-mode-edit)
  (define-key perry-mode-map (kbd "<down-mouse-1>") 'perry-mode-edit)
  (define-key perry-mode-map (kbd "d") (lambda () (interactive) (perry--delete-post perry--info)))
  (define-key perry-mode-map (kbd "p") (lambda () (interactive) (perry-mode-preview perry--info)))
  (define-key perry-mode-map (kbd "P") (lambda () (interactive) (perry--render-site perry--info)))
  (define-key perry-mode-map (kbd "+") (lambda () (interactive) (perry-new-post perry--info)))
  (define-key perry-mode-map (kbd "a") (lambda () (interactive) (perry-new-post perry--info))))

(provide 'perry-mode)
;;; perry-mode.el ends here

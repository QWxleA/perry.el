;;; perry-export.el --- export todo                  -*- lexical-binding: t; -*-

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
(require 'mustache)

(defun perry--sitenav (curtitle perry--info)
  "CURTITLE is the title of the current page, used to highlight the menu-item."
  (let* ((site--info (cdadr (plist-get perry--info :site))))
    (with-temp-buffer
      (dolist (item (cadr (plist-get perry--info :menu)))
        (message "menu: %s/%s" (cdr item) (car item))
        (insert (mustache-render
                 (perry-file-to-string
                  (perry--get-template "nav-item" perry--info))
                 (ht 
                  ("active" (string= curtitle (cdr item)))
                  ("site.baseurl" (plist-get site--info :baseurl))
                  ("link.url" (if (string-prefix-p "/" (car item))
                                  (car item)
                                (concat "/" (car item))))
                  ("link-title" (cdr item))
                  ("link.text" (cdr item)))))) 
      (buffer-string))))

(defun perry--render-item (file content curtemplate perry--info)
  "Render FILE, CONTENT contains a hash used by mustache-render and CURTEMPLATE.
Returns a string containing the rendered page" 
  (let* ((mustache-partial-paths (perry--get-template-path perry--info))
         (site--info (cdadr (plist-get perry--info :site)))
         (tmp (with-temp-buffer
                ;; the index does not exists as a real file
                (if (file-readable-p file)
                    (progn
                      (insert-file-contents file) 
                      (setq title (or (perry--org-kwd "TITLE") (plist-get site--info :title)))
                      (setq descr (or (perry--org-kwd "DESCRIPTION") (plist-get site--info :description)))
                      (setq keywords (or (perry--org-kwd "KEYWORDS") (plist-get site--info :keywords))))
                  (setq title (plist-get site--info :title))
                  (setq descr (plist-get site--info :description))
                  (setq keywords (plist-get site--info :keywords)))))
         (context (ht
                   ;; these items are used in head.mustache
                   ("page.title" title)
                   ("page.description" descr)
                   ("page.keywords" keywords)
                   ("page.url" (perry--get-new-url file perry--info))
                   ("site.author.name" (plist-get site--info :author.name))
                   ("site.author.job" (plist-get site--info :author.job))
                   ("site.author.bio" (plist-get site--info :author.bio))
                   ("site.keywords" (plist-get site--info :keywords))
                   ("site.baseurl" (plist-get site--info :baseurl))
                   ("project.name" (plist-get site--info :project.name))
                   ("project.description" (plist-get site--info :project.description))
                   ("site.title" (plist-get site--info :title))
                   ("content" content)
                   ("sitenav" (perry--sitenav title perry--info))
                   )
                  ))
    (mustache-render (perry-file-to-string
                      (perry--get-template curtemplate perry--info)) context)))

(defun perry--render-sitelist (perry--info)
  "Create a list of all posts, usinf TODO template.
Returns a string containing the sitelist page" 
  (let* ((file (concat (file-name-as-directory (plist-get perry--info :docroot)) "index.html"))
         (perry--files
          (file-expand-wildcards (expand-file-name "*.org"  (expand-file-name "posts" (plist-get perry--info :docroot))))) 
         (content (with-temp-buffer
                    (dolist (file perry--files)
                      (with-temp-buffer
                        (insert-file-contents file)
                        (setq title (perry--get-title))
                        (setq descr (perry--get-descr))
                        (setq url (perry--get-new-url file perry--info))
                        (setq date (perry--nice-date perry--info))
                        ) 
                      (setq context (ht ("post-date" date)
                                        ("post-url" url)
                                        ("post-title" title)
                                        ("post-description" descr)))
                      (insert (mustache-render
                               (perry-file-to-string
                                (perry--get-template "postlistentry" perry--info)) context)))
                    (buffer-string)))
         )
    (perry--render-item file content "postlistpage" perry--info)))

(defun perry--publish-sitelist (perry--info)
  "Publish sitelist to :exportslug/index.html" 
  (let* ((sitemap-file (expand-file-name "index.html"
                                         (expand-file-name (plist-get perry--info :exportslug)
                                                           (plist-get perry--info :docroot))))
         (newsitelist (perry--render-sitelist perry--info)))
    (perry--string-to-file newsitelist sitemap-file)))

(defun perry--publish-archive (perry--info)
  "Publish sitelist to :exportslug/index.html" 
  (let* ((archivefile (expand-file-name "index.html"
                                        (expand-file-name (plist-get perry--info :exportslug)
                                                          (plist-get perry--info :docroot)))) 
         (perry--files
          (file-expand-wildcards (expand-file-name "*.org"  (expand-file-name "posts" (plist-get perry--info :docroot)))))
         (content (with-temp-buffer
                    (dolist (file perry--files)
                      (with-temp-buffer
                        (insert-file-contents file)
                        (setq title (perry--get-title))
                        (setq descr (perry--get-descr))
                        (setq url (perry--get-new-url file perry--info))
                        (setq date (perry--nice-date perry--info))
                        ) 
                      (setq context (ht ("post-date" date)
                                        ("post-url" url)
                                        ("post-title" title)
                                        ("post-description" descr)))
                      (insert (mustache-render
                               (perry-file-to-string
                                (perry--get-template "postlistentry" perry--info)) context)))
                    (buffer-string)))
         (newarchive (perry--render-item "tmp" content "postlistpage" perry--info)))
    (perry--string-to-file newarchive archivefile)))

(defun perry--publish-type (perry--info perry--type) 
  "Render all post types, either posts or pages."
  (let* ((perry--files
          (file-expand-wildcards (expand-file-name "*.org"  (expand-file-name perry--type (plist-get perry--info :docroot))))))
    (dolist (file perry--files) 
      (perry-publish-file file (perry--get-new-file-loc file perry--info) perry--info))))

(defun perry--copy-resources (perry--info)
  "Copy theme resources to export folder."
  (let* ((theme--info (cdadr (plist-get perry--info :theme)))
         (sourceloc (expand-file-name (file-name-as-directory (plist-get theme--info :loc)) (plist-get perry--info :themedir)))
         (exportloc (expand-file-name (plist-get perry--info :exportslug) (plist-get perry--info :docroot))))
    (dolist (res (cadr (plist-get theme--info :res)))
      (copy-directory (expand-file-name res sourceloc) exportloc)
      (message "Copying %s to %s" res exportloc))))

(defun perry--render-site (perry--info)
  "todo - add render-fromtpage here"
  (perry--copy-resources perry--info)
  (perry--publish-type perry--info "posts")
  (perry--publish-type perry--info "pages")
  (perry--publish-archive perry--info))

(defun perry-publish-file (file newfile perry--info)
  "Converts FILE.org to NEWFILE.html and saves it in :exportslug.
pages are hardcoded to be exported to /:docroot/:exportslug
all other post-types go in their own folders." 
  (let* ((content (with-temp-buffer
                    (insert-file-contents file)
                    (org-export-as 'html nil nil t nil))) 
         (newcontents (perry--render-item file content "container" perry--info)))
    (perry--string-to-file newcontents newfile)))

(provide 'perry-export)
;;; perry-export.el ends here

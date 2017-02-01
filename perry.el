;;; perry.el --- perry org-mode based blogging engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Alex Poslavsky

;; Author: Alex 'QWxleA' Poslavsky <qwxlea@gmail.com>
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

;; perry is a static site generator based on Emacs, Git and Org mode.

;; 1. Sources:   https://github.com/qwxlea/perry-org
;; 2. Documents: http://qwxlea.github.io/perry-org

;;; Code:

(require 'perry-mode)
(require 'perry-export)
(require 'perry-utils)

(require 'mustache)
(require 'ox-html)
(require 'simple-httpd)

;;; Configuration - export to perry-config.el
(defvar perry--mode-toggle t
  "toggles between pages (nil) and posts(t) in perry-manager")

(defvar perry--default-site "documentation"
  "todo - add initial value to perry--rc")1
(setq perry-curproj "documentation")

;;FIXME rename var!
(setq perry--rc '(("documentation"
                   :docroot "~/Projects/perry.el/Documentation"
                   :exportslug "_export" ;in docroot
                   :post_slug "blog" ;;fixme not used?
                   :web-server-port 8080 
                   :themedir "~/Projects/perry.el/themes" ; in perry-root
                   :theme '("Thinkspace/Org"
                            :loc "thinkspace" ; all relative to themedir
                            :res '("css" "img" "icons")) 
                   :menu '(("install.html" . "Installation"))
                   :site '("Site-info"
                           :title  "Perry Documentation"
                           :description "Perry, a static site converter, build using elisp and orgmode"
                           :baseurl "http://localhost:8080/" ;must end in a slash!
                           :keywords "blog, static site, elisp, Emacs"
                           :default-tags "configuration"                  
                           :default-category "Emacs"
                           :project.name "Perry.el"
                           :project.description "is a static site converter, written in elisp, using org-mode and mustache to create content" 
                           :author.name "Alex 'QWxleA' Poslavsky"
                           :author.email "qwxlea@gmail"))
                  ("AP Blog"
                   :docroot "~/Documents/BLOG/"
                   :exportslug "_export" ;in docroot
                   :post_slug "blog" ;;fixme not used?
                   :web-server-port 8080 
                   :themedir "~/Projects/perry.el/themes" ; in perry-root
                   :theme '("Thinkspace/Org"
                            :loc "thinkspace" ; all relative to themedir
                            :res '("css" "img")) 
                   :menu '(("blog.html" . "Blog")
                           ("about.html" . "About"))
                   :site '("Site-info"
                           :title  "Perry Documentation"
                           :description "Perry, a static site converter, build using elisp and orgmode"
                           :baseurl "http://localhost:8080/" ;must end in a slash!
                           :keywords "blog, static site, elisp, emacs"
                           :default-tags "configuration"                  
                           :default-category "emacs"
                           :author.name "Alex 'QWxleA' Poslavsky"
                           :author.bio "Perry.el is a static site converter, written in elisp, using org-mode and mustache to create content" 
                           :author.job "Emacs user, elisp student and coffee aficionado" 
                           :author.email "qwxlea@gmail"))
                  ("default"
                   :web-server-docroot "~/Documents/BLOG/export"
                   :web-server-port 8080
                   :docroot "~/Documents/BLOG"
                   :themedir "~/Documents/BLOG/templates"
                   :menu '(("/" . "Home")
                           ("/blog.html" . "Blog")
                           ("/about.html" . "About"))
                   )))

;;;###autoload
(defun perry-manager (&optional perry-curproj)
  "List posts or pages in the current project (PERRY_CURPROJ)."
  (interactive (let* ((p perry--default-site))
                 (list p)))
  (switch-to-buffer (get-buffer-create (format "*perry-manager [%s]*" perry-curproj)))
  (perry-mode))

(provide 'perry)
;;; perry-org.el ends here

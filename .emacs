(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; define my packages
(defvar my-packages
  '(better-defaults
    cider
    clojure-mode
    company
    docker-compose-mode
    dockerfile-mode
    dracula-theme
    exec-path-from-shell
    fill-column-indicator
    magit
    markdown-mode
    neotree
    parinfer
    racer
    rust-mode))

;; install my packages
(mapc
   #'(lambda (package)
       (unless (package-installed-p package)
         (package-install package)))
   my-packages)

(require 'better-defaults)

;; appearance
(setq-default fill-column 80)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq neo-theme 'arrow)
(load-theme 'dracula t)
(setq inhibit-startup-screen t)

;; Org
(global-set-key "\C-ca" 'org-agenda)
(with-eval-after-load 'org
  (setq org-log-done 'time)
  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE")))
  (add-hook 'org-mode-hook #'visual-line-mode))

;; Org work and publishing
(set-variable 'org-agenda-files (list "~/Documents/Work"))
(load-file "~/Documents/Work/publish.el")
(setq org-publish-project-alist
      (append '()
        (alist-entries "~/Documents/Work" "~/Documents/publish_Work")))

;; OS X Stuff
(setq default-input-method "MacOSX")
(setq mac-command-modifier 'meta
      mac-option-modifier nil)
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; git
(global-set-key (kbd "C-x g") 'magit-status)

;; general source code
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'fci-mode)

;; markdown
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'fci-mode)

;; clj/cljs
(add-hook 'clojure-mode-hook #'parinfer-mode)
(add-hook 'clojurescript-mode-hook #'parinfer-mode)

;; rust
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)

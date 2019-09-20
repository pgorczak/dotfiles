(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; set to nil or t
(defvar +clojure nil)
(defvar +docker nil)
(defvar +macos nil)
;; General Python support: pip install --user flake8
;; Pipenv support: add python-language-server[all] as dev-dependency
;; Use ctrl+x p p to activate pipenv + lsp
(defvar +python nil)
;; Catkin ws support: pip install --user python-language-server[all]
;; Use ctrl+x p r to activate catkin ws + lsp
(defvar +ros nil)
;; Requires setup
;; https://github.com/racer-rust/racer#installation
(defvar +rust nil)
;; Requires synced work folder
(defvar +work nil)

;; define my packages
(defvar my-packages
  (append
    '(better-defaults
      company
      company-lsp
      dracula-theme
      drag-stuff
      fill-column-indicator
      flycheck
      flymd
      lsp-mode
      lsp-ui
      magit
      markdown-mode
      multi-term
      multiple-cursors
      neotree)
    (when +clojure
      '(cider
        clojure-mode
        parinfer))
    (when +macos
      '(exec-path-from-shell))
    (when +docker
      '(docker-compose-mode
        dockerfile-mode))
    (when +python
      '(pipenv))
    (when +ros
      '(cmake-mode))
    (when +rust
      '(racer
        rust-mode))))

;; install my packages
;; http://wikemacs.org/wiki/Package.el#Install_packages_automatically_on_startup

(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(require 'better-defaults)

;; appearance
(setq-default fill-column 79)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq neo-theme 'arrow)
(load-theme 'dracula t)
(setq inhibit-startup-screen t)
;; make default frame accommodate 3-digit line nums and fci
(when (display-graphic-p)
  (add-to-list 'default-frame-alist (cons 'width 85)))

;; toggle filetree
(global-set-key (kbd "<C-tab>") 'neotree-toggle)

;; multi line editing
(require 'multiple-cursors)
(global-set-key (kbd "C-|") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; comment/uncomment
(global-set-key (kbd "C-#") 'comment-line)

;; drag lines and regions
(with-eval-after-load 'drag-stuff
  (define-key drag-stuff-mode-map (kbd "<C-up>") 'drag-stuff-up)
  (define-key drag-stuff-mode-map (kbd "<C-down>") 'drag-stuff-down))

;; Terminal
(global-set-key (kbd "\C-x t") 'multi-term)

;; Language server protocol (LSP)
;; Hooks defined per language below
(require 'lsp-mode)
(set-variable 'lsp-enable-snippet nil)
(set-variable 'lsp-prefer-flymake nil)

;; Org
(global-set-key "\C-ca" 'org-agenda)
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'visual-line-mode))

;; Org work and publishing
(when +work
  (set-variable 'org-agenda-files (list "~/Documents/Work"))
  (load-file "~/Documents/Work/publish.el")
  (setq org-publish-project-alist
        (append '()
          (alist-entries "~/Documents/Work" "~/Documents/publish_Work"))))

;; OS X Stuff
(when +macos
  (setq default-input-method "MacOSX")
  (setq mac-command-modifier 'meta
        mac-option-modifier nil)
  (require 'exec-path-from-shell)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; git
(global-set-key (kbd "C-x g") 'magit-status)

;; general source code
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'drag-stuff-mode)

;; markdown
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'fci-mode)

;; js
(setq js-expr-indent-offset 2)
(setq js-indent-level 2)

;; clj/cljs
(when +clojure
  (add-hook 'clojure-mode-hook #'parinfer-mode)
  (add-hook 'clojurescript-mode-hook #'parinfer-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode))

;; python
(when +python
  (setq flycheck-python-flake8-executable "~/.local/bin/flake8")
  (add-hook 'python-mode-hook #'flycheck-mode)
  (add-hook 'python-mode-hook (lambda () (set-fill-column 79)))
  ;; LSP + pipenv
  (setq-default lsp-pyls-plugins-pycodestyle-enabled nil)
  (setq-default lsp-pyls-plugins-pydocstyle-enabled nil)
  (setq-default lsp-pyls-plugins-pylint-enabled nil)
  (setq-default lsp-pyls-plugins-rope-completion-enabled nil)
  (setq-default lsp-pyls-plugins-mccabe-enabled nil)
  (global-set-key (kbd "C-x p p") 'pipenv-mode)
  (add-hook 'pipenv-mode-hook (lambda () (pipenv-activate) (lsp)))
)

;; ROS
(when +ros
  (add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.xacro\\'" . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.urdf\\'" . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.sdf\\'" . xml-mode))
  ; catkin ws mode
  (defun catkin-root ()
    (with-temp-buffer
      (call-process "catkin" nil t nil "locate" "-d")
      (goto-char (point-min))
      (buffer-substring (point) (line-end-position))))
  (defun catkin-pkg-root ()
    (with-temp-buffer
      (call-process "bash" nil t nil "-c" "catkin locate $(catkin list --unformatted --this)")
      (goto-char (point-min))
      (buffer-substring (point) (line-end-position))))
  (defun catkin-env ()
    (setq env '())
    (with-temp-buffer
      (call-process "bash" nil t nil "-c"
                    ;;;"source $(catkin locate -d)/setup.bash; env | egrep 'ROS|PYTHON|^PATH|^PKG_CONFIG_PATH'")
                    "source $(catkin locate -d)/setup.bash; env")
      (goto-char (point-min))
      (while (not (eobp))
        (setq env
              (cons (buffer-substring (point) (line-end-position)) env))
        (forward-line 1)))
    env)
  (defun catkin-setup ()
    "Activate catkin env and LSP"
    (interactive)
    (let ((env (catkin-env))
          ;; Replaces the entire env with the result of setup.bash
          (process-environment env)
          (pkg-root (catkin-pkg-root)))
      (push pkg-root (lsp-session-folders (lsp-session)))
      (lsp)))
  (global-set-key (kbd "C-x p r") 'catkin-setup)
)

;; rust
(when +rust
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (require 'rust-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))

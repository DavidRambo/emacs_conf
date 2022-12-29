; Following along with System Crafter's Emacs From Scratch series.
(setq inhibit-startup-message t)

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Add some fringe

(menu-bar-mode -1)      ; Disable the menu bar

;; (setq visible-bell t)   ; Visible bell
(setq ring-bell-function 'ignore)  ; Disable error bell

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

;; Establish sources for packages.
;; When installing a package and it is not found, try calling list-packages
;; because that will update the list of packages.
(setq package-archive-priorities
      '(("org" . 15)
        ("elpa" . 10)
        ("melpa" . 5)))

(setq package-archives  '(("melpa" . "https://melpa.org/packages/")
                          ("org" . "https://orgmode.org/elpa/")
                          ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package general
  :config
  ;; leader-key definer
  (general-create-definer dr/leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; Fonts
(cond ((eq system-type 'gnu/linux)
       (set-face-attribute 'default nil :font "MesloLGSDZ Nerd Font" :height 140)
       (set-face-attribute 'variable-pitch nil
			   :font "Source Sans Pro" :height 140))
      ((eq system-type 'darwin)
       (set-face-attribute 'default nil :font "SauceCodePro Nerd Font Mono" :height 140)
       (set-face-attribute 'variable-pitch nil
			   :font "Source Sans Pro" :height 150)))

(use-package doom-themes
  :init (load-theme 'doom-one t))  ; t is to avoid prompt to load theme

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20)))

(use-package all-the-icons
  :ensure t)

;; Depending on the source for ivy, swiper may not be included.
;(use-package swiper
;  :ensure t)

;; Completion
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-partial)
         ;; ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ;; ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-wrap t)
  (setq +ivy-buffer-preview t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-virtual-buffer t)
  ;; Set minibuffer height per command
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 8))
  ;; (dr/leader-key
  ;;  "," '(+ivy/switch-workspace-buffer :which-key "switch workspace buffers")
  ;;  "<" '(ivy-switch-buffer :which-key "switch buffers")))

;; Short descriptions next to ivy choices.
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ; Change function used by current mapping:
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; helpful bindings
(dr/leader-key
  "h" '(:ignore t :which-key "help")
  "hf" '(counsel-describe-function :which-key "describe function")
  "hv" '(counsel-describe-variable :which-key "describe variable"))

;; Which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

;; Display column number in modeline
(column-number-mode)

;; Line numbers
(setq display-line-numbers-type 'relative)

(defun dr/display-line-numbers-hook ()
  (display-line-numbers-mode 1)
  )
(add-hook 'prog-mode-hook 'dr/display-line-numbers-hook)
(add-hook 'text-mode-hook 'dr/display-line-numbers-hook)

;; Disable line numbers for some modes
;; The first method worked at first, and it still works within my Doom emacs
;; config. But now (12/29/2022) the second method (dolist) works, which did
;; not initially work. After restarting, the second did not work, but running
;; the first and restarting org-mode did.
(defun dr/disable-line-numbers-hook ()
  (display-line-numbers-mode 0)
  )
(add-hook 'org-mode-hook 'dr/disable-line-numbers-hook)

; Now this works...?
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Turn on rainbow-delimiters for every programming mode.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)  ; turn off and use evil-collection below instead
  (setq evil-want-C-u-scroll t)  ; rebind C-u from universal-argument to scroll up
  (setq evil-want-C-i-jump t)  ; C-i to jump forward (inverse of C-o)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-fine-undo t
        undo-limit 80000000)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package undo-fu)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; (use-package evil-snipe
;;   :after evil
;;   :config
;;   (evil-snipe-mode t)
;;   :hook
;;   (magit-mode . 'turn-off-evil-snipe-override-mode)
;;   :custom
;;   (evil-snipe-scope 'visible))


(use-package avy)
(define-key evil-normal-state-map (kbd "s") 'avy-goto-char-2-below)
(define-key evil-normal-state-map (kbd "S") 'avy-goto-char-2-above)

;; hydra for repetition
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("x" nil "finished" :exit t))

(dr/leader-key
 "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :init
  ;; The below variable settings are from DOOM emacs
  (setq projectile-auto-discover nil
        projectile-globally-ignored-files '(".DS_STORE" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
  ;;       projectile-kill-buffers-filter 'kill-only-files
        projectile-ignored-projects '("/~"))

  (when (file-directory-p "~/repos/")
    (setq projectile-project-search-path '("~/repos")))
  ;; (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-switch-project-action #'projectile-find-file))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode)
  (setq ivy-initial-inputs-alist nil))
;; Now press alt-o for actions on highlighted selection during projectile action

;; Projectile bindings
(dr/leader-key
 "p" '(:ignore t :which-key "project")
 "pa" '(projectile-add-known-project :which-key "add project")
 "pF" '(counsel-projectile-rg :which-key "ripgrep in files")
 "pp" '(projectile-switch-project :which-key "Switch to project")
 "pf" '(counsel-projectile-find-file :which-key "Find file")
 "pd" '(projectile-find-dir :whick-key "Find project directory")
 "pb" '(counsel-projectile-switch-to-buffer :which-key "Switch to project buffer"))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  ;; custom function to display diff in separate window
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; magit bindings
(dr/leader-key
 "g" '(:ignore t :which-key "git")
 "gg" '(magit-status :which-key "magit status"))

;; Org Mode
(defun dr/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (auto-fill-mode 0)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode)
  (lambda () (display-line-numbers-mode 0)))

(defun dr/org-font-setup ()
  ;; Set headings face sizes.
  (dolist (face '((org-level-1 . 1.2)
    (org-level-2 . 1.1)
    (org-level-3 . 1.05)
    (org-level-4 . 1.0)
    (org-level-5 . 1.0)
    (org-level-6 . 1.0)
    (org-level-7 . 1.0)
    (org-level-8 . 1.0)))
  (set-face-attribute (car face) nil :weight 'regular :height (cdr face))))

(require 'org-indent)

(use-package org
  :defer t
  :hook 
    (org-mode . dr/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t
	org-src-fontify-natively t
	org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)

  (setq org-agenda-files 
	'("~/notes/tasks.org"))

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "ACTIVE(a)" "DONE(d!)")
	  ))

  ;; Custom agenda views
  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-deadline-warning-days 7)))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))
	    (todo "ACTIVE"
		  ((org-agenda-overriding-header "Active Tasks")))))
	  ("n" "Next Tasks"
	   ((todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))))
	  ("a" "Active Tasks"
	   ((todo "ACTIVE"
		  ((org-agenda-overriding-header "Active Tasks")))))
	  ))

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (dr/org-font-setup)
  )

;; Org-mode bindings
;; (use-package evil-org
;;   :ensure t
;;   :after org
;;   :hook ((org-mode . (lambda () evil-org-mode))
;; 	 ;; (org-agenda-mode . evil-org-mode)
;; 	 )
;;   :config
;;   ;; (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys)
;;   )

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "◌" "⁖" "◿")))

;; Ensure fixed-pitch faces for select org-mode areas.
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Get rid of the background on column views
(set-face-attribute 'org-column nil :background nil)
(set-face-attribute 'org-column-title nil :background nil)

;; Set maximum width for org-mode display
(defun dr/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . dr/org-mode-visual-fill))

;; Keybindings
(dr/leader-key
 ;; buffers
 "b" '(:ignore t :which-key "buffer")
 "bb" '(counsel-switch-buffer :which-key "switch buffer")
 "," '(counsel-switch-buffer :which-key "switch buffer")
 "bk" '(kill-current-buffer :which-key "Kill current buffer")
 "bn" 'evil-next-buffer
 "b]" 'evil-next-buffer
 "bp" 'evil-prev-buffer
 "b[" 'evil-prev-buffer
 ;; files
 "f" '(:ignore t :which-key "file")
 "fs" '(save-buffer :which-key "save file")
 "ff" '(find-file :which-key "find file")
 "." '(find-file :which-key "find file")
 ;; org-mode
 "o" '(:ignore t :which-key "org")
 "oa" '(org-agenda :which-key "org-agenda")
 ;;search
 "s" '(:ignore t :which-key "search")
 "sb" '(swiper :which-key "search buffer")
 ;; toggles
 "t"  '(:ignore t :which-key "toggle")
 "tc" '(comment-line :which-key "comment line")
 "tn" '(org-toggle-narrow-to-subtree :which-key "Narrow subtree")
 "tt" '(counsel-load-theme :which-key "choose theme")
 ;; windows
 "w" '(:ignore t :which-key "window")
 "wb" 'balance-windows
 "wc" '(delete-window :which-key "close window")
 "wo" '(delete-other-windows :which-key "delete other windows")
 "wn" 'evil-window-left
 "wi" 'evil-window-right
 "wu" 'evil-window-up
 "we" 'evil-window-down
 "wm" 'maximize-window
 "ws" 'split-window-below
 "wv" 'split-window-right)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-org undo-fu evil-snipe visual-fill-column org-superstar org-bullets evil-magit magit which-key use-package rainbow-delimiters ivy-rich hydra helpful general evil-collection doom-themes doom-modeline counsel-projectile command-log-mode all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

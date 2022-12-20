; Following along with System Crafter's Emacs From Scratch series.
(setq inhibit-startup-message t)

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Add some fringe

(menu-bar-mode -1)      ; Disable the menu bar

(setq visible-bell t)   ; Visible bell

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
    :global-prefix "C-SPC")
  ;; Example use of the general definer:
  (dr/leader-key
   "t"  '(:ignore t :which-key "toggle")
   "tt" '(counsel-load-theme :which-key "choose theme")))

; Font
(set-face-attribute 'default nil :font "MesloLGSDZ Nerd Font" :height 140)

(use-package doom-themes
  :init (load-theme 'doom-one t))  ; t is to avoid prompt to load theme

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

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
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
)
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
 "hf" '(counsel-describe-function :which-key "describe function")
 "hv" '(counsel-describe-variable :which-key "describe variable"))

;; Which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Turn on rainbow-delimiters for every programming mode.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)  ; turn off and use evil-collection below instead
  (setq evil-want-C-u-scroll t)  ; rebind C-u from universal-argument to scroll up
  (setq evil-want-C-i-jump t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

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
  :config (counsel-projectile-mode))
;; Now press alt-o for actions on highlighted selection during projectile action

;; Projectile bindings
(dr/leader-key
 "p" '(:ignore t :which-key "project+")
 "pa" '(projectile-add-known-project :which-key "add project")
 "pF" '(counsel-projectile-rg :which-key "ripgrep in files")
 "pp" '(counsel-projectile :which-key "Switch to project")
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
 "g" '(:ignore t :which-key "git+")
 "gg" '(magit-status :which-key "magit status"))

;; Keybindings
(dr/leader-key
 ;; buffers
 "b" '(:ignore t :which-key "buffer+")
 "bk" '(kill-current-buffer :which-key "Kill current buffer")
 "bn" 'evil-next-buffer
 "b]" 'evil-next-buffer
 "bp" 'evil-prev-buffer
 "b[" 'evil-prev-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-magit magit which-key use-package rainbow-delimiters ivy-rich hydra helpful general evil-collection doom-themes doom-modeline counsel-projectile command-log-mode all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

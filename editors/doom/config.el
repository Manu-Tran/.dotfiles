;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Keybinds
(load! "bindings")
(setq auth-sources '("~/.authinfo.gpg"))

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Emmanuel Tran (Manu)"
      user-mail-address "emmanuel.tran@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:

; (setq doom-font (font-spec :family "monospace" :size 12))
;; (setq doom-font (font-spec :family "JetBrains Mono" :size 12))
;(setq doom-font (font-spec :family "JetBrains Mono" :size 11))
;;(setq doom-font (font-spec :family "Fira Code" :size 14))

(menu-bar-mode t)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(setq doom-modeline-window-width-limit fill-column)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type 'relative)
;; (setq display-line-numbers-type 'nil)
(setq display-line-numbers 'relative)
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-current-absolute t)

;; Keeps the cursor within x lines from the top/bottom
(setq scroll-margin 7)

;; Completion Stuff =============================================================

;; Delay for completion
(setq company-idle-delay 10
      company-minimum-prefix-length 3)
;;
;; (setq lsp-java-format-settings-url "file://Users/emmanuel.tran/dd/eclipse-java-google-style-format.xml")
;; (setq lsp-java-format-settings-profile "GoogleStyle")
;; (setq lsp-ui-sideline-delay 10.0)
;; (setq lsp-ui-doc-delay 1.5)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 3.0)
(setq lsp-on-idle-hook nil)
(setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:+UseStringDeduplication" "-XX:MaxMetaspaceSize=256m" "-Xms2048m" "-Xmx2048m" "-Dlog.level=ERROR"))
 ;;"-cp \"~/dd/logs-backend-2/target/*\""
(setq lsp-java-completion-max-results 20)
(setq lsp-inhibit-message t)
(setq lsp-after-apply-edits-hook nil)
(setq lsp-before-save-edits nil)
(setq lsp-log-io t)
(setq lsp-io-messages-max 10)
(setq lsp-java-autobuild-enabled nil)
(setq lsp-java-max-concurrent-builds 64)
(setq lsp-use-workspace-root-for-server-default-directory t)
(setq lsp-ui-doc-show-with-mouse nil)
(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-lens-enable nil)

;; Always open in an already open window
(setq display-buffer-base-action '(display-buffer-use-some-window))

(after! projectile-mode
  (setq projectile-project-root-files-bottom-up (cons "pom.xml" projectile-project-root-files-bottom-up)))

(setq lsp-enable-file-watchers nil)

;; (add-hook 'code-review-mode-hook
;;           (lambda ()
;;             ;; include *Code-Review* buffer into current workspace
;;             (persp-add-buffer (current-buffer))))

;; (setq lsp-java-vmargs '("-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication"))
;; (setq lsp-java-jdt-download-url  "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz")
;; (setq debug-on-error t)



;; Language of the grammar checking
;; (setq langtool-default-language "fr-FR")

;; Fill Column At 80th character
;;(require 'fill-column-indicator)

(setq fci-rule-width 3)
;; (setq fci-rule-color "darkblue")

;; (defun sanityinc/fci-enabled-p () (symbol-value 'fci-mode))

;; (defvar sanityinc/fci-mode-suppressed nil)
;; (make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

;; (defadvice popup-create (before suppress-fci-mode activate)
;;  "Suspend fci-mode while popups are visible"
;;  (let ((fci-enabled (sanityinc/fci-enabled-p)))
;;    (when fci-enabled
;;      (setq sanityinc/fci-mode-suppressed fci-enabled)
;;      (turn-off-fci-mode))))

;;(defadvice popup-delete (after restore-fci-mode activate)
  ;;"Restore fci-mode when all popups have closed"
  ;;(when (and sanityinc/fci-mode-suppressed
             ;;(null popup-instances))
    ;;(setq sanityinc/fci-mode-suppressed nil)
   ;; (turn-on-fci-mode)))


;; (add-hook! 'prog-mode 'fci-mode)
;; (add-hook! 'python-mode-local-vars-hook #'fci-mode)
;; (defun my-flycheck-python-setup ()
;;   (flycheck-add-next-checker 'lsp 'python-flake8))

;; These MODE-local-vars-hook hooks are a Doom thing. They're executed after
;; MODE-hook, on hack-local-variables-hook. Although `lsp!` is attached to
;; python-mode-local-vars-hook, it should occur earlier than my-flycheck-setup
;; this way:
;;
;; Hook Stuff ==================================================================
;; (add-hook 'lsp-after-initialize-hook (lambda
;;                                        ()
;;                                        (flycheck-add-next-checker 'lsp 'python-flake8)))
;;                                        (eval-after-load 'eglot-java
;;
;; (eval-after-load 'eglot-java
;;   (progn
;;     (require 'eglot-java)
;;     '(eglot-java-init)))

(setq lsp-ui-sideline-enable t)
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-ui-sideline-show-symbol nil)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-delay 0.5)
(setq lsp-prefer-flymake nil)
(setq lsp-ui-peek-enable nil)
(setq lsp-enable-file-watchers nil)
(setq lsp-log-max nil)
(setq lsp-enable-links nil)
(setq lsp-eldoc-enable-hover nil)
(setq lsp-enable-symbol-highlighting nil)
(after! dap-mode
        (dap-register-debug-template
        "Java Attach"
        (list :name "Java Attach"
                :type "java"
                :request "attach"
                :hostName "localhost"
                :port 5005)))

(setq python-shell-exec-path '("/Users/emmanueltran/.pyenv/shims/"))

;; (add-hook 'python-mode-hook 'conda-env-autoactivate-mode)
;; (conda-env-activate 'base)


; (let ((langs '("american" "francais")))
;       (setq lang-ring (make-ring (length langs)))
;       (dolist (elem langs) (ring-insert lang-ring elem)))

; (defun cycle-ispell-languages ()
;   (interactive)
;   (let ((lang (ring-ref lang-ring -1)))
;     (ring-insert lang-ring lang)
;     (ispell-change-dictionary lang)))

(setq projectile-project-search-path '("~/Programmation" "~/dd"))
(setq +workspaces-switch-project-function 'magit-status)
;; Datadog Linting =============================================================

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Stop lagging after saving
(setq lsp-before-save-edits nil)

;; Indent with spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; UTF-8 EVERYWHERE!!!
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Run Prettier on save
;; (add-hook 'before-save-hook 'prettier-before-save)

;; Configure Prettier to match the CLI settings
(setq prettier-js-args '(
  "--print-width" "80"
  "--tab-width" "4"
  "--single-quote" "true"
))

;; Key stuff ====================================================================

(setq which-key-idle-delay 0.4)

;; Mac Option Enabler
(setq ns-alternate-modifier 'none)
(setq mac-option-modifier 'none)

;; Calendar stuff ==============================================================

(load! "calendar.el")
(load! "advent-of-code.el")
(load! "exercism.el")

;; Evil stuff ==================================================================

(require 'evil-replace-with-register)
(setq evil-replace-with-register-key (kbd "gr"))
(evil-replace-with-register-install)
(evil-ex-define-cmd "q[uit]" 'kill-current-buffer)
(evil-ex-define-cmd "wq" 'doom/save-and-kill-buffer)

;; (advice-add #'evil-quit :around #'my-evil-quit)

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

(define-key evil-normal-state-map (kbd "gj") 'evil-next-line)
(define-key evil-normal-state-map (kbd "gk") 'evil-previous-line)

;; Org stuff ====================================================================

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Workflow configuration
;; (load! "orgconfig")

;; Protobuf mode
(require 'protobuf-mode)

;; (require 'ob-ipython)

;;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(setq yas-snippet-dirs (append yas-snippet-dirs '("~/.doom.d/my-snippets")))
(setq magit-todos-keywords-list '("TODO(manut)" "FIXME(manut)" "REVIEW(manut)" "HACK(manut)" "DEPRECATED(manut)" "BUG(manut)" "XXX(manut)"))

;; Don't show tags for magit because it is slow on dogweb (too many tags)
(after! magit
  (remove-hook! 'magit-status-headers-hook #'magit-insert-tags-header)
  (remove-hook! 'magit-status-sections-hook #'magit-insert-unpushed-to-upstream-or-recent #'magit-insert-unpulled-from-upstream)
)

(defun tkj/org-file-of-the-day()
  (interactive)
  (let ((daily-name (format-time-string "%Y/%Y-%m-%d")))
    (find-file
     (expand-file-name
      (concat "~/org/daily/" daily-name ".org")))))


;;(org-babel-do-load-languages
;; 'org-babel-load-languages
;; '((emacs-lisp . t)
;;   (julia . t)
;;   (python . t)
;;   (ipython . t)
;;   (jupyter . t)))
;; (setq ob-async-no-async-languages-alist '("ipython"))

(defvar hexcolour-keywords
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property (match-beginning 0)
                            (match-end 0)
                            'face (list :background
                                        (match-string-no-properties 0)))))))
(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))

;; Select window between frames
(after! ace-window
  (setq aw-scope 'global))

;; Allow for python to count _ as part of a word
(add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'python-mode-hook #'tree-sitter-hl-mode)
(after! python-mode
  (setq flycheck-python-pycompile-executable "/Users/emmanueltran/.pyenv/shims/python3"))



;; Language specific config

;;(use-package! mvn
;;  :config
;;  (defun re-seq (regexp string)
;;    "Get a list of all regexp matches in a string"
;;    (save-match-data
;;      (let ((pos 0)
;;            matches)
;;        (while (string-match regexp string pos)
;;          (push (match-string 1 string) matches)
;;          (setq pos (match-end 0)))
;;        matches)))
;;
;;  (defun extract-project-list ()
;;    (interactive)
;;    (with-temp-buffer
;;      (insert-file-contents (concat (lsp--suggest-project-root) "pom.xml"))
;;      (keep-lines "\\(<module>\\|<name>\\)" (point-min) (point-max))
;;      (setq java-modules-list (re-seq "<module>\\(.*\\)<\/module>" (buffer-string)))
;;      (if (null java-modules-list)
;;          (re-seq "<name>logs-backend-\\(.*\\)<\/name>" (buffer-string))
;;        java-modules-list)
;;      )
;;    )
;;
;;  (defun mvn-spotless ()
;;    (interactive)
;;    (mvn (concat "spotless:apply -pl "
;;                 (completing-read "Enter project name:" (extract-project-list) nil t))))
;;  )

;; (use-package! kubernetes
;;   :ensure t
;;   :commands (kubernetes-overview)
;;   :init
;;   (map! :leader :desc "Kubernetes" "k" #'kubernetes-overview))


;; ;; If you want to pull in the Evil compatibility package.
;; (use-package! kubernetes-evil
;;   :ensure t
;;   :after kubernetes)

;; (use-package! jupyter
;;   :defer)
;; (use-package! ob-async
;;   :defer)
;; (use-package! conda
;;   :defer
;;   :init
;;   (setq conda-anaconda-home (expand-file-name "~/opt/miniconda3"))
;;   (setq conda-env-home-directory (expand-file-name "~/opt/miniconda3")))
;; (load! "~/Programmation/emacs-jupyter/jupyter-client.el")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
;; (use-package nlinum-relative
;;   :config
;;   (nlinum-relative-setup-evil)
;;   (add-hook 'prog-mode-hook 'nlinum-relative-mode)
;; )


; (use-package! org-dropbox
;   :init
;   :config
;   (setq org-dropbox-note-dir "~/org/sync")
;   (setq org-dropbox-datetree-file "~/org/sync/reference.org")
;   )

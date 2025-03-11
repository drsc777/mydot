;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "abby"
      user-mail-address "abbyzyl777@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/notes/org/")
(setq org-roam-directory (file-truename "~/notes/roam/"))

;; ==========================================================
;; Org Agenda 配置
;; ==========================================================

;; 设置agenda文件列表 - 确保包含所有相关文件
(setq org-agenda-files '("~/notes/" 
                         "~/notes/org/"
                         "~/notes/s25.org"
                         "~/notes/todo.org"))

;; 设置agenda显示参数
(setq org-agenda-span 14                     ;; 显示两周的内容
      org-agenda-start-on-weekday nil        ;; 从今天开始显示
      org-agenda-start-day "-0d"             ;; 从今天开始
      org-agenda-todo-ignore-scheduled nil   ;; 不忽略计划任务
      org-agenda-todo-ignore-deadlines nil   ;; 不忽略截止任务
      org-agenda-todo-ignore-timestamp nil   ;; 不忽略时间戳任务
      org-agenda-skip-scheduled-if-done t    ;; 跳过已完成的计划任务
      org-agenda-skip-deadline-if-done t     ;; 跳过已完成的截止任务
      org-agenda-include-deadlines t         ;; 包含截止日期
      org-agenda-include-diary nil           ;; 不包含日记
      org-agenda-text-search-extra-files '(agenda-archives) ;; 搜索存档文件
      org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s") ;; 自定义显示格式
                                 (todo . " %i %-12:c")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))

;; 设置agenda自定义视图
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-agenda-span 7)
                     (org-agenda-start-day "-0d")
                     (org-agenda-start-on-weekday nil)))
          (alltodo ""
                  ((org-agenda-overriding-header "所有任务")
                   (org-agenda-sorting-strategy '(priority-down todo-state-down deadline-up))))))

        ("n" "Agenda and all TODOs"
         ((agenda "" nil)
          (alltodo "" nil))
         nil)
        
        ("s" "School Tasks"
         ((agenda "" 
                  ((org-agenda-span 14)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-prefix-format "  %?-12t% s")))
          (tags-todo "school"
                    ((org-agenda-overriding-header "School Tasks")
                     (org-agenda-sorting-strategy '(priority-down deadline-up)))))
         nil)))

;; 确保在agenda中包含今天的任务
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-start-with-clockreport-mode nil)

;; 在Doom中正确启用org-agenda模块
(after! org
  ;; 设置基本参数
  (setq org-log-done 'time                   ;; 完成任务时记录时间
        org-log-into-drawer t                ;; 记录到抽屉中
        org-agenda-window-setup 'current-window ;; 在当前窗口打开agenda
        org-agenda-restore-windows-after-quit t) ;; 退出后恢复窗口配置
  
  ;; 确保能够正确识别待办任务
  (setq org-agenda-todo-keyword-format "%-12s") ;; 确保足够空间显示状态
)

;; 添加agenda快捷键绑定
(map! :leader
      (:prefix "o"
       :desc "Agenda" "a" #'org-agenda
       :desc "Todo list" "t" #'org-todo-list))

;; 常规agenda快捷键
(global-set-key (kbd "C-c a") 'org-agenda)

;; ==========================================================

(setq org-roam-file-extensions '("org"))

;; (use-package! pangu-spacing
;;   :ensure t
;;   :config
;;   (global-pangu-spacing-mode 1))

;; (cl-defmethod org-roam-node-type ((node org-roam-node))
;;   "Return the TYPE of NODE."
;;   (condition-case nil
;;       (file-name-nondirectory
;;        (directory-file-name
;;         (file-name-directory
;;          (file-relative-name (org-roam-node-file node) org-roam-directory))))
;;     (error "")))
;;
(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(setq org-roam-capture-templates
      '(("m" "main" plain "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new (file+head "reference/${title}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new (file+head "articles/${title}.org"
                            "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t
         :unnarrowed t)
        ("d" "default" plain "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/notes/roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package! org-roam-ui
  :after org-roam
  :hook (org-roam-mode . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package vterm
  :ensure t
  :commands vterm)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; 自动折叠大纲设置
(after! org
  (setq org-startup-folded t)  ;; 默认折叠所有标题
  
  ;; TODO状态设置
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)")))
  
  ;; Org Babel支持
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (emacs-lisp . t)))

  ;; 允许执行代码块时不询问
  (setq org-confirm-babel-evaluate nil)
)

;; 启用super-save自动保存
(use-package! super-save
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; GitHub自动同步笔记功能
(defun sync-org-notes ()
  (when (eq major-mode 'org-mode)
    (let ((default-directory "~/notes/"))
      (when (file-exists-p ".git")
        (shell-command "git add . && git commit -m 'Auto-sync notes' && git push origin main || true")))))

(add-hook 'after-save-hook 'sync-org-notes)

;; org-journal配置
(use-package! org-journal
  :config
  (setq org-journal-dir "~/notes/journal/"
        org-journal-date-format "%Y-%m-%d"))

;; 设置org-capture快速记录
(after! org
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/notes/org/inbox.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("n" "Note" entry (file+headline "~/notes/org/inbox.org" "Notes")
           "* %? :NOTE:\n  %i\n  %a"))))

;; 拼写检查配置
(after! ispell
  ;; 配置ispell程序路径
  (setq ispell-program-name "/opt/homebrew/bin/ispell")
  ;; 如果您想使用aspell替代ispell，请使用下面的配置
  ;; (setq ispell-program-name "/opt/homebrew/bin/aspell")
  ;; (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))

  ;; 配置不应该被检查的面部（如代码块等）
  (add-to-list 'ispell-skip-region-alist '("\\$\\$" . "\\$\\$"))
  (add-to-list 'ispell-skip-region-alist '("\\\\begin{code}" . "\\\\end{code}"))
  (add-to-list 'ispell-skip-region-alist '("\\\\begin{minted}" . "\\\\end{minted}"))
  (add-to-list 'ispell-skip-region-alist '("\\\\begin{verbatim}" . "\\\\end{verbatim}")))

;; 如果你想支持中文拼写检查
;; (setq ispell-dictionary "en_US,zh_CN")

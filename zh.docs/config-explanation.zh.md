# 配置文件详解

这个文档详细解释了各个工具的配置文件内容，帮助你理解每个设置的作用。

## Doom Emacs 配置

### config.el

主要配置文件，包含个人信息、主题、功能设置等：

```elisp
;; 个人信息
(setq user-full-name "abby"
      user-mail-address "abbyzyl777@gmail.com")

;; 主题设置
(setq doom-theme 'doom-gruvbox)  ;; 使用Gruvbox主题

;; 行号显示
(setq display-line-numbers-type t)

;; 笔记目录设置
(setq org-directory "~/notes/org/")
(setq org-roam-directory (file-truename "~/notes/roam/"))
(setq org-roam-file-extensions '("org"))

;; Org Roam 显示模板
(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

;; 笔记捕获模板
(setq org-roam-capture-templates
      '(("m" "main" plain "%?" ...)  ;; 主要笔记
        ("r" "reference" plain "%?" ...) ;; 参考资料笔记
        ("a" "article" plain "%?" ...) ;; 文章笔记
        ("d" "default" plain "%?" ...))) ;; 默认笔记

;; Org Roam 配置
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/notes/roam/"))
  :bind (...)
  :config ...)

;; Org Roam UI 配置
(use-package! org-roam-ui
  :after org-roam
  :hook (org-roam-mode . org-roam-ui-mode)
  :config ...)

;; 终端支持
(use-package vterm
  :ensure t
  :commands vterm)

;; 自动折叠
(after! org
  (setq org-startup-folded t))

;; TODO状态设置
(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)"))))

;; Org Babel支持
(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (emacs-lisp . t))))

;; 自动保存
(use-package! super-save
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; GitHub自动同步
(defun sync-org-notes ()
  (when (eq major-mode 'org-mode)
    (let ((default-directory "~/notes/"))
      (when (file-exists-p ".git")
        (shell-command "git add . && git commit -m 'Auto-sync notes' && git push origin main || true")))))

(add-hook 'after-save-hook 'sync-org-notes)

;; 日志配置
(use-package! org-journal
  :config
  (setq org-journal-dir "~/notes/journal/"
        org-journal-date-format "%Y-%m-%d"))

;; 快速记录设置
(after! org
  (setq org-capture-templates
        '(...)))
```

### init.el

控制Doom Emacs加载的模块：

- 启用了UI相关模块：doom, doom-dashboard, modeline等
- 启用了编辑器功能：evil, file-templates, fold等
- 启用了语言支持：emacs-lisp, markdown, org等
- 启用了工具：magit, lookup等

### packages.el

管理额外安装的包：

```elisp
(package! super-save)  ;; 自动保存功能
(package! org-journal)  ;; 日志功能
(package! org-roam-ui  ;; Org Roam UI
  :recipe (:host github :repo "org-roam/org-roam-ui"))
(package! vterm)  ;; 终端支持
```

## NeoVim 配置

### init.lua

主配置文件，设置了：

```lua
-- 插件管理器设置
-- 使用Packer管理插件

-- 插件列表
-- LSP支持（语言服务器协议）
-- Copilot AI补全
-- Telescope文件搜索
-- 自动保存
-- 文件树
-- 代码高亮
-- 状态栏
-- Git集成
-- Tmux集成
-- 自动配对括号
-- 注释插件

-- 基本设置
vim.opt.number = true  -- 显示行号
vim.opt.relativenumber = true  -- 相对行号
vim.opt.expandtab = true  -- 用空格代替Tab
vim.opt.shiftwidth = 2  -- 缩进宽度
vim.opt.tabstop = 2  -- Tab宽度
-- 更多UI和编辑器设置...

-- 快捷键设置
-- 设置Leader键为空格
-- Telescope快捷键
-- NvimTree快捷键
-- 窗口导航快捷键
-- 缓冲区导航
-- Copilot设置

-- 插件配置
-- 自动保存配置
-- LSP配置
-- NvimTree设置
-- Treesitter设置
-- 状态栏设置
-- Git集成设置
-- 自动配对设置
-- 注释插件设置
```

## Tmux 配置

### .tmux.conf

```bash
# 前缀键设置为Ctrl+a
unbind C-b
set -g prefix C-a
bind C-a send-prefix

# 启用鼠标支持
set -g mouse on

# 状态栏配置
set -g status-style 'bg=#333333 fg=#5eacd3'
# 状态栏显示内容...

# 窗口索引从1开始
set -g base-index 1
setw -g pane-base-index 1

# 窗口分割快捷键
bind | split-window -h -c "#{pane_current_path}"
bind - split-window -v -c "#{pane_current_path}"

# Vim式导航
bind -n C-h select-pane -L
bind -n C-j select-pane -D
bind -n C-k select-pane -U
bind -n C-l select-pane -R

# 终端设置
set -g default-terminal "screen-256color"
set -ga terminal-overrides ",*256col*:Tc"

# 插件设置
set -g @plugin 'tmux-plugins/tpm'
set -g @plugin 'tmux-plugins/tmux-resurrect'  # 会话恢复
set -g @plugin 'tmux-plugins/tmux-continuum'  # 自动保存

# 自动保存设置
set -g @continuum-restore 'on'
set -g @continuum-save-interval '10'
```

## 实用脚本

### sync-cursor-nvim.sh

```bash
#!/bin/bash
# 监控指定项目目录的文件变化，并通知Cursor AI更新
# 用法: sync-cursor-nvim.sh <项目目录>

# 监控特定目录的文件变化
PROJECT_DIR=$1

# 使用fswatch监控文件变更并激活Cursor
fswatch -o "$PROJECT_DIR" | xargs -n1 -I{} osascript -e 'tell application "Cursor" to activate' &
```

### sync-notes.sh

```bash
#!/bin/bash
# 自动将笔记同步到Git仓库

# 配置笔记目录
NOTES_DIR=~/notes

# 检查目录是否存在
# 如果不存在，创建目录结构
# 如果不是Git仓库，初始化Git

# 添加并提交更改
git add .
git commit -m "Auto-sync notes: $(date +'%Y-%m-%d %H:%M:%S')"

# 尝试推送到远程仓库
git push origin main || git push origin master
```

## Aerospace 配置

### config.toml

Aerospace是macOS的窗口管理器，配置定义了：

- 工作区设置
- 窗口布局规则
- 键盘快捷键
- 应用程序行为规则
- UI设置 
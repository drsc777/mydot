# Abby的开发环境配置 (dotfiles)
# Abby's Development Environment Configuration (dotfiles)

这个仓库包含了我的完整开发环境配置，基于无鼠标操作理念，专注于高效编程体验。

This repository contains my complete development environment configuration, based on a mouse-free philosophy, focused on efficient programming experience.

## 快速安装 | Quick Installation

```bash
# 克隆仓库 | Clone repository
git clone https://github.com/drsc777/mydot.git
cd mydot

# 运行安装脚本 | Run installation script
./install.sh    # 中文版 | Chinese version
./install_en.sh # 英文版 | English version
```

## 环境概览 | Environment Overview

我的开发环境由以下组件组成：

My development environment consists of the following components:

- **Doom Emacs** - 笔记管理和Org Mode | Note management and Org Mode
- **NeoVim** - 主要编程工具 | Primary coding tool
- **Cursor AI** - AI辅助编程 | AI-assisted programming
- **Tmux** - 终端会话管理 | Terminal session management
- **Aerospace** - 窗口管理器（macOS）| Window manager (macOS)

## 目录结构 | Directory Structure

```
mydot/
├── doom/                 # Doom Emacs配置 | Doom Emacs configuration
│   ├── config.el         # 主要配置 | Main configuration
│   ├── init.el           # 初始化和模块 | Initialization and modules
│   ├── packages.el       # 包管理 | Package management
│   └── custom.el         # 自动生成的自定义配置 | Auto-generated custom configuration
├── nvim/                 # NeoVim配置 | NeoVim configuration
│   ├── init.lua          # 主配置文件 | Main configuration file
│   └── lua/              # Lua模块 | Lua modules
├── tmux/                 # Tmux配置 | Tmux configuration
│   └── .tmux.conf        # 主配置文件 | Main configuration file
├── bin/                  # 实用脚本 | Utility scripts
│   ├── sync-cursor-nvim.sh  # Cursor AI与NeoVim同步 | Cursor AI and NeoVim synchronization
│   └── sync-notes.sh     # 笔记自动同步到GitHub | Auto-sync notes to GitHub
├── .aerospace.toml      # Aerospace窗口管理器配置 | Aerospace window manager configuration
└── docs/                 # 文档 | Documentation
    ├── setup-guide.md    # 安装指南 | Installation guide
    └── keyboard-shortcuts.md # 快捷键参考 | Keyboard shortcuts reference
```

## 特色功能 | Key Features

1. **无鼠标工作流** | **Mouse-free Workflow**: 所有工具都经过配置，确保可以完全使用键盘进行操作 | All tools are configured to ensure complete keyboard operation
2. **AI增强编程** | **AI-enhanced Programming**: 通过GitHub Copilot和Cursor AI集成 | Integration with GitHub Copilot and Cursor AI
3. **笔记管理系统** | **Note Management System**: 基于Org-roam的知识库 | Knowledge base built on Org-roam
4. **自动同步** | **Auto Synchronization**: 笔记自动提交到GitHub | Notes automatically committed to GitHub
5. **高效窗口管理** | **Efficient Window Management**: 使用Tmux和Aerospace实现无缝窗口操作 | Seamless window operations with Tmux and Aerospace

## 安装指南 | Installation Guide

详细安装步骤请查看 [安装指南](docs/setup-guide.md)

For detailed installation steps, please check the [Installation Guide](docs/setup-guide.md)

## 快捷键参考 | Keyboard Shortcuts Reference

所有重要快捷键的完整列表请参考 [快捷键指南](docs/keyboard-shortcuts.md)

For a complete list of all important keyboard shortcuts, please refer to the [Keyboard Shortcuts Guide](docs/keyboard-shortcuts.md)

## 文档 | Documentation

- 所有文档支持中英双语 | All documentation supports both Chinese and English
- 安装脚本有中英文两个版本（`install.sh` 和 `install_en.sh`）| Installation scripts are available in both Chinese and English (`install.sh` and `install_en.sh`)
- 还提供了完全英文的安装指南 `docs/setup-guide-en.md` | A fully English installation guide is also provided at `docs/setup-guide-en.md`

## 配置文件说明 | Configuration Files Explanation

- **Doom Emacs**: 配置了Org-mode、自动折叠、TODO管理、笔记链接等功能 | Configured with Org-mode, auto-folding, TODO management, note linking, etc.
- **NeoVim**: 集成了LSP、Treesitter、Telescope、自动保存等功能 | Integrated with LSP, Treesitter, Telescope, auto-save, etc.
- **Tmux**: 配置了Vim式导航、会话恢复和窗口管理 | Configured with Vim-style navigation, session restoration, and window management
- **脚本** | **Scripts**: 提供了笔记同步和Cursor AI集成功能 | Provides note synchronization and Cursor AI integration

## 许可证 | License

MIT 

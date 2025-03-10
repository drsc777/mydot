# Abby的开发环境配置 (dotfiles)

这个仓库包含了我的完整开发环境配置，基于无鼠标操作理念，专注于高效编程体验。

## 快速安装

```bash
# 克隆仓库
git clone https://github.com/drsc777/mydot.git
cd mydot

# 运行安装脚本
./install.sh
```

## 环境概览

我的开发环境由以下组件组成：

- **Doom Emacs** - 笔记管理和Org Mode
- **NeoVim** - 主要编程工具
- **Cursor AI** - AI辅助编程
- **Tmux** - 终端会话管理
- **Aerospace** - 窗口管理器（macOS）

## 目录结构

```
mydot/
├── doom/                 # Doom Emacs配置
│   ├── config.el         # 主要配置
│   ├── init.el           # 初始化和模块
│   ├── packages.el       # 包管理
│   └── custom.el         # 自动生成的自定义配置
├── nvim/                 # NeoVim配置
│   ├── init.lua          # 主配置文件
│   └── lua/              # Lua模块
├── tmux/                 # Tmux配置
│   └── .tmux.conf        # 主配置文件
├── bin/                  # 实用脚本
│   ├── sync-cursor-nvim.sh  # Cursor AI与NeoVim同步
│   └── sync-notes.sh     # 笔记自动同步到GitHub
├── config.toml           # Aerospace窗口管理器配置
└── docs/                 # 文档
    ├── setup-guide.md    # 安装指南
    └── keyboard-shortcuts.md # 快捷键参考
```

## 特色功能

1. **无鼠标工作流**：所有工具都经过配置，确保可以完全使用键盘进行操作
2. **AI增强编程**：通过GitHub Copilot和Cursor AI集成
3. **笔记管理系统**：基于Org-roam的知识库
4. **自动同步**：笔记自动提交到GitHub
5. **高效窗口管理**：使用Tmux和Aerospace实现无缝窗口操作

## 安装指南

详细安装步骤请查看 [安装指南](docs/setup-guide.md)

## 快捷键参考

所有重要快捷键的完整列表请参考 [快捷键指南](docs/keyboard-shortcuts.md)

## 配置文件说明

- **Doom Emacs**: 配置了Org-mode、自动折叠、TODO管理、笔记链接等功能
- **NeoVim**: 集成了LSP、Treesitter、Telescope、自动保存等功能
- **Tmux**: 配置了Vim式导航、会话恢复和窗口管理
- **脚本**: 提供了笔记同步和Cursor AI集成功能

## 许可证

MIT 

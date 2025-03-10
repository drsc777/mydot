#!/bin/bash

# 设置笔记目录
NOTES_DIR=~/notes

# 如果目录不存在，创建它
if [ ! -d "$NOTES_DIR" ]; then
  echo "创建笔记目录: $NOTES_DIR"
  mkdir -p "$NOTES_DIR"
fi

cd "$NOTES_DIR"

# 如果不是git仓库，初始化它
if [ ! -d ".git" ]; then
  echo "初始化Git仓库"
  git init
  echo "# 我的笔记" > README.md
  git add README.md
  git commit -m "初始化笔记仓库"
  
  echo "请手动设置远程仓库，例如:"
  echo "git remote add origin https://github.com/你的用户名/notes.git"
  exit 0
fi

# 检查是否有更改
if [[ -z $(git status -s) ]]; then
  echo "没有需要同步的变更"
  exit 0
fi

# 添加所有更改
git add .

# 提交更改
git commit -m "Auto-sync notes: $(date +'%Y-%m-%d %H:%M:%S')"

# 尝试推送到远程
if git remote | grep -q origin; then
  git push origin main || git push origin master || echo "推送失败，可能需要设置远程仓库"
else
  echo "请先设置远程仓库，例如:"
  echo "git remote add origin https://github.com/你的用户名/notes.git"
fi 
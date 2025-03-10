#!/bin/bash

# 设置项目目录
PROJECT_DIR=$1
if [ -z "$PROJECT_DIR" ]; then
  echo "请指定要监控的项目目录"
  exit 1
fi

# 清理旧进程
pkill -f "fswatch -o $PROJECT_DIR"

# 启动双向同步
echo "开始监控 $PROJECT_DIR 的文件变化..."

# 监控NeoVim的更改并通知Cursor
fswatch -o "$PROJECT_DIR" | xargs -n1 -I{} osascript -e 'tell application "Cursor" to activate' &

# 记录进程ID到日志文件
echo "同步进程已启动，PID: $!" 
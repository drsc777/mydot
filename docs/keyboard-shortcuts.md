# Keyboard Shortcuts Guide

This guide contains key shortcuts for all tools, helping you work efficiently without a mouse.

## Navigation Principles

In this configuration:
- **Space key** is usually the main command key (Leader key)
- **C-** means holding the Ctrl key
- **M-** means holding the Alt/Option key
- **S-** means holding the Shift key

## Table of Contents
- [Doom Emacs Shortcuts](#doom-emacs-shortcuts)
- [NeoVim Shortcuts](#neovim-shortcuts)
- [Tmux Shortcuts](#tmux-shortcuts)
- [Aerospace Shortcuts](#aerospace-shortcuts)
- [General Workflow](#general-workflow)

## Doom Emacs Shortcuts

### Basic Operations

| Shortcut | Function |
|----------|----------|
| `SPC` | Leader key (main command key) |
| `SPC .` | Open file |
| `SPC ,` | Switch buffer |
| `SPC b b` | List all buffers |
| `SPC f s` | Save file |
| `SPC f f` | Find file |
| `SPC q q` | Quit Emacs |
| `SPC w v` | Split window vertically |
| `SPC w s` | Split window horizontally |
| `SPC w w` | Switch window |
| `SPC w c` | Close window |

### Org Mode

| Shortcut | Function |
|----------|----------|
| `SPC m h` | Insert header |
| `SPC m i` | Insert item |
| `SPC m t` | Insert TODO |
| `SPC m d` | Insert DONE |
| `TAB` | Fold/unfold section |
| `S-TAB` | Fold/unfold all |
| `C-c C-t` | Cycle TODO state |
| `C-c C-s` | Schedule item |
| `C-c C-d` | Set deadline |
| `C-c C-c` | refresh task status |
| `C-c C-c` | add tags |
| `C-c C-e` | export file |
| `q` | quit export file |
| `C-x C-s` | save file |
| `C-x C-f` | open file |
| `M-up/down` | move a headline up or down |
| `M-left/right` | promote or demote a headline |
| `M-RET` | insert a new headline |
| `SPC o A` | agenda |


### Org Roam

| Shortcut | Function |
|----------|----------|
| `SPC n r f` | Find note |
| `SPC n r i` | Insert link |
| `SPC n r I` | Insert node |
| `SPC n r c` | Capture new note |
| `SPC n r t` | Toggle roam buffer |

## NeoVim Shortcuts

### Basic Navigation

| Shortcut | Function |
|----------|----------|
| `h, j, k, l` | Move left, down, up, right |
| `w, b` | Move forward/backward by word |
| `0, $` | Move to start/end of line |
| `gg, G` | Move to start/end of file |
| `Ctrl+u, Ctrl+d` | Scroll half-page up/down |
| `{, }` | Move between paragraphs |
| `%` | Jump to matching bracket |

### Editing

| Shortcut | Function |
|----------|----------|
| `i, a` | Enter insert mode (at/after cursor) |
| `o, O` | Insert new line below/above |
| `v, V` | Enter visual mode (character/line) |
| `y, d, c` | Yank (copy), delete, change |
| `p, P` | Paste after/before cursor |
| `u, Ctrl+r` | Undo, redo |
| `.` | Repeat last command |

### Custom Mappings

| Shortcut | Function |
|----------|----------|
| `<Space>` | Leader key |
| `<Space>ff` | Find files |
| `<Space>fg` | Live grep |
| `<Space>fb` | Buffers |
| `<Space>fh` | Help tags |
| `Ctrl+h/j/k/l` | Navigate between splits |
| `<Space>e` | Toggle file explorer |
| `<Space>w` | Save file |
| `<Space>q` | Close buffer |
| `<Space>h` | Clear search highlight |

## Tmux Shortcuts

### Session Management

| Shortcut | Function |
|----------|----------|
| `Ctrl+a c` | Create new window |
| `Ctrl+a w` | List windows |
| `Ctrl+a n` | Next window |
| `Ctrl+a p` | Previous window |
| `Ctrl+a ,` | Rename window |
| `Ctrl+a &` | Kill window |
| `Ctrl+a d` | Detach from session |
| `Ctrl+a s` | List sessions |
| `Ctrl+a $` | Rename session |

### Pane Management

| Shortcut | Function |
|----------|----------|
| `Ctrl+a |` | Split pane vertically |
| `Ctrl+a -` | Split pane horizontally |
| `Ctrl+a h/j/k/l` | Navigate between panes |
| `Ctrl+a z` | Toggle pane zoom |
| `Ctrl+a x` | Kill pane |
| `Ctrl+a {, }` | Swap panes |
| `Ctrl+a q` | Show pane numbers |
| `Ctrl+a o` | Rotate panes |

## Aerospace Shortcuts

### Window Management

| Shortcut | Function |
|----------|----------|
| `Alt+h/j/k/l` | Move focus between windows |
| `Alt+Shift+h/j/k/l` | Resize focused window |
| `Alt+1-9` | Switch to workspace 1-9 |
| `Alt+Shift+1-9` | Move window to workspace 1-9 |
| `Alt+f` | Toggle fullscreen |
| `Alt+Shift+f` | Toggle floating mode |
| `Alt+Shift+space` | Center floating window |
| `Alt+Shift+e` | Balance windows |

## General Workflow

Typical workflow patterns:

1. **Starting a coding session**:
   - Launch terminal: `⌘+Space` → "Terminal"
   - Start tmux: `tmux` or `tmux attach`
   - Split panes as needed: `Ctrl+a |` and `Ctrl+a -`
   - Navigate to project directory: `cd project`
   - Open NeoVim: `nvim`

2. **Taking notes**:
   - Launch Emacs: `⌘+Space` → "Emacs"
   - Open Org Roam: `SPC n r f` to find notes or `SPC n r c` to create new note
   - Work with TODO items: `SPC m t` to add TODOs
   - Schedule tasks: `C-c C-s`

3. **AI-assisted coding**:
   - Work in NeoVim/Cursor
   - Start sync script: `sync-cursor project-dir`
   - Use GitHub Copilot with `Tab` to accept completions
   - In Cursor, use AI features with `⌘+K`

*Note: Chinese users can also check the [Chinese version of this guide](keyboard-shortcuts.zh.md).* 

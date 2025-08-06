# Emacs Demo - Guile Multilanguage Examples

## Quick Start

```bash
# Automated demo (runs steps automatically)
./run-emacs-demo.sh

# Manual launch (interactive exploration)  
./emacs-launch.sh
```

## Manual Demo Steps

### 1. Launch Environment
```bash
./emacs-launch.sh
```

### 2. Open Project File
- `C-x C-f complete-demo.scm`
- Notice syntax highlighting for Scheme

### 3. Start Geiser REPL
- `M-x geiser-guile` or `C-c C-z`
- Interactive Guile REPL in Emacs

### 4. Test Basic Scheme
```scheme
(+ 10 20 30)
(map (lambda (x) (* x x)) '(1 2 3 4 5))
```

### 5. Test Multilanguage Compilation
```scheme
(use-modules (system base compile))
(compile '(+ 5 10 15) #:from 'elisp #:to 'value)
(compile '(defun square (x) (* x x)) #:from 'elisp #:to 'value)
(compile '(square 8) #:from 'elisp #:to 'value)
```

### 6. Explore Project
- `C-x C-f README.md` - Project documentation
- `M-x magit-status` - Git integration
- `C-c C-d C-d` - Geiser documentation lookup

### 7. Advanced Features
- `C-c C-k` - Compile current buffer
- `C-c C-e` - Evaluate expression
- `C-c C-r` - Evaluate region
- `C-c C-a` - Enter module

## Key Bindings Added

| Key | Command | Description |
|-----|---------|-------------|
| `F5` | `geiser-guile` | Start Geiser REPL |
| `F6` | `magit-status` | Open Git status |
| `F7` | `forge-dispatch` | GitHub/GitLab operations |
| `F8` | `claude-code-ide-explain` | AI code explanation |
| `C-c g` | `magit-file-dispatch` | Git file operations |
| `C-c f` | `forge-dispatch` | Forge operations |
| `C-c c` | `claude-code-ide-menu` | Claude IDE menu |

## Features Demonstrated

✅ **Syntax Highlighting** - Full Scheme/Guile support  
✅ **REPL Integration** - Live interactive development  
✅ **Multilanguage** - Compile Elisp, test Brainfuck  
✅ **Git Integration** - Magit for version control  
✅ **GitHub/GitLab** - Forge for repository management  
✅ **AI Assistance** - Claude Code IDE integration  
✅ **Documentation** - Built-in help and lookup  
✅ **Project Navigation** - File management and search  

## Troubleshooting

### REPL Not Starting
```bash
# Ensure Guile is in PATH
which guile3
export PATH="/usr/local/bin:$PATH"
```

### Packages Not Loading
```bash
# Reinstall packages
./setup-emacs-integrations.sh
```

### Authentication Issues
```bash
# Set up GitHub token for Forge
# Follow prompts in Emacs: M-x forge-add-repository
```

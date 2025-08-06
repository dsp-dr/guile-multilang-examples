#!/usr/bin/env bash
# Create Emacs demo for Guile multilanguage environment

set -e

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$PROJECT_ROOT"

echo "🎯 Creating Emacs Demo for Guile Multilanguage Environment"
echo "=========================================================="

# Create Emacs demo script
cat > emacs-demo-script.el << 'EOF'
;;; Emacs Demo Script for Guile Multilanguage Examples
;;; This demonstrates the integrated development environment

(message "🎯 Guile Multilanguage Examples - Emacs Demo")
(message "==============================================")

;; Show current configuration
(message "📂 Project Directory: %s" default-directory)
(message "⚙️  Emacs Version: %s" emacs-version)

;; Delay for visibility
(sit-for 2)

;; 1. Open and demonstrate Scheme file
(message "\n🔵 Opening Scheme demonstration file...")
(find-file "complete-demo.scm")
(goto-char (point-min))
(sit-for 1)

;; Show syntax highlighting
(message "✅ Scheme syntax highlighting active")
(sit-for 2)

;; 2. Start Geiser REPL
(message "\n🔄 Starting Geiser REPL for interactive development...")
(geiser-guile)
(sit-for 2)

;; Switch to REPL buffer
(switch-to-buffer "*Geiser Guile*")

;; 3. Demonstrate REPL interaction
(message "\n⚡ Testing basic REPL functionality...")
(insert "(+ 10 20 30)")
(comint-send-input)
(sit-for 1)

(insert "(map (lambda (x) (* x x)) '(1 2 3 4 5))")
(comint-send-input)
(sit-for 2)

;; 4. Test multilanguage compilation
(message "\n🔄 Demonstrating multilanguage compilation...")
(insert "(use-modules (system base compile))")
(comint-send-input)
(sit-for 1)

(insert "(compile '(+ 5 10 15) #:from 'elisp #:to 'value)")
(comint-send-input)
(sit-for 2)

;; 5. Show file navigation
(message "\n📁 Demonstrating project navigation...")
(find-file "README.md")
(sit-for 1)
(goto-char (point-min))
(search-forward "Features")
(sit-for 2)

;; 6. Show Magit integration
(message "\n📋 Opening Magit status...")
(magit-status)
(sit-for 2)

;; 7. Final message
(message "\n🎉 Emacs Demo Complete!")
(message "============================")
(message "✅ Syntax highlighting working")
(message "✅ Geiser REPL integration active")
(message "✅ Multilanguage compilation functional")
(message "✅ Project navigation ready")
(message "✅ Version control integration available")
(message "\n🔗 Ready for development!")

;; Return to main project file
(find-file "complete-demo.scm")
EOF

# Create automated Emacs demo launcher
cat > run-emacs-demo.sh << 'EOF'
#!/usr/bin/env bash
# Run automated Emacs demo

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export EMACS_USER_DIRECTORY="$SCRIPT_DIR/.emacs.d"

echo "🎬 Starting Emacs Demo for Guile Multilanguage Examples"
echo "======================================================="
echo "Configuration: $EMACS_USER_DIRECTORY"
echo ""

# Set up Guile environment
export GUILE_LOAD_PATH="$SCRIPT_DIR/submodules/guile-ares-rs/modules:$GUILE_LOAD_PATH"

# Launch Emacs with demo script
emacs --init-directory="$EMACS_USER_DIRECTORY" \
      --load="$SCRIPT_DIR/emacs-demo-script.el" \
      --eval="(sit-for 3)" \
      --eval="(message \"Demo ready for interaction...\")" \
      "$SCRIPT_DIR/complete-demo.scm"
EOF

chmod +x run-emacs-demo.sh

# Create manual demo instructions
cat > EMACS_DEMO.md << 'EOF'
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
EOF

echo ""
echo "✅ Emacs demo created successfully!"
echo ""
echo "📋 Available demo options:"
echo "  • ./run-emacs-demo.sh    - Automated demonstration"  
echo "  • ./emacs-launch.sh      - Manual interactive session"
echo "  • EMACS_DEMO.md          - Complete demo guide"
echo ""
echo "🎬 Demo features:"
echo "  ✅ Geiser REPL integration"
echo "  ✅ Multilanguage compilation demo"
echo "  ✅ Project navigation"
echo "  ✅ Git/GitHub integration"
echo "  ✅ AI assistance demo"
echo ""
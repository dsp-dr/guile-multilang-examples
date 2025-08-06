#!/usr/bin/env bash
# Setup script for Magit Forge and Claude Code IDE integrations

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$SCRIPT_DIR"

echo "=== Setting up Emacs integrations for Guile Multilang Examples ==="

# Check prerequisites
check_prerequisites() {
    echo "Checking prerequisites..."
    
    if ! command -v emacs >/dev/null 2>&1; then
        echo "❌ Emacs not found. Please install Emacs first."
        echo "   FreeBSD: pkg install emacs"
        exit 1
    fi
    
    if ! command -v git >/dev/null 2>&1; then
        echo "❌ Git not found. Please install Git first."
        exit 1
    fi
    
    if ! command -v guile >/dev/null 2>&1; then
        echo "❌ Guile not found. Please install Guile first."
        echo "   FreeBSD: pkg install guile3"
        exit 1
    fi
    
    echo "✅ Prerequisites met"
}

# Initialize submodules
setup_submodules() {
    echo "Setting up submodules..."
    
    cd "$PROJECT_ROOT"
    
    # Initialize all submodules
    git submodule update --init --recursive
    
    echo "✅ Submodules initialized"
}

# Setup Emacs directory structure
setup_emacs_config() {
    echo "Setting up Emacs configuration..."
    
    local emacs_dir="$PROJECT_ROOT/.emacs.d"
    
    # Create directory if it doesn't exist
    mkdir -p "$emacs_dir"
    
    # Set the correct path in init.el for submodules
    sed -i.bak "s|user-emacs-directory|\"$PROJECT_ROOT/\"|g" "$emacs_dir/init.el"
    
    echo "✅ Emacs configuration updated"
}

# Install required Emacs packages
install_emacs_packages() {
    echo "Installing required Emacs packages..."
    
    # Create a temporary Emacs script to install packages
    cat > /tmp/install-packages.el << 'EOF'
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(defvar required-packages
  '(use-package geiser geiser-guile magit projectile company which-key
    flycheck multiple-cursors doom-themes doom-modeline))

(dolist (package required-packages)
  (unless (package-installed-p package)
    (package-install package)))

(message "All packages installed successfully!")
EOF

    emacs --batch --load /tmp/install-packages.el
    rm /tmp/install-packages.el
    
    echo "✅ Emacs packages installed"
}

# Setup Magit Forge authentication
setup_forge_auth() {
    echo "Setting up Forge authentication..."
    
    local authinfo_file="$HOME/.authinfo.gpg"
    local authinfo_plain="$HOME/.authinfo"
    
    if [ ! -f "$authinfo_file" ] && [ ! -f "$authinfo_plain" ]; then
        echo "⚠️  GitHub authentication not configured."
        echo "   To use Forge with GitHub, you need to set up authentication."
        echo "   Please follow these steps:"
        echo ""
        echo "   1. Create a GitHub personal access token:"
        echo "      https://github.com/settings/tokens"
        echo "      Scopes needed: repo, user, admin:org"
        echo ""
        echo "   2. Add to ~/.authinfo (or ~/.authinfo.gpg for encryption):"
        echo "      machine api.github.com login YOUR_USERNAME^forge password YOUR_TOKEN"
        echo ""
        echo "   3. Set correct permissions:"
        echo "      chmod 600 ~/.authinfo"
        echo ""
    else
        echo "✅ Authentication file found at $authinfo_file"
    fi
}

# Setup Claude Code IDE
setup_claude_ide() {
    echo "Setting up Claude Code IDE..."
    
    echo "⚠️  Claude Code IDE configuration needed:"
    echo "   1. Get your Anthropic API key from: https://console.anthropic.com/"
    echo "   2. Add to your environment:"
    echo "      export ANTHROPIC_API_KEY='your-api-key-here'"
    echo "   3. Or add to ~/.authinfo:"
    echo "      machine api.anthropic.com password your-api-key-here"
    echo ""
    echo "   The integration is configured with these keybindings:"
    echo "   C-c c c  - Start Claude chat"
    echo "   C-c c e  - Explain code"
    echo "   C-c c r  - Refactor code" 
    echo "   C-c c g  - Generate code"
    echo "   C-c c d  - Generate docstring"
}

# Create project-specific commands
create_project_commands() {
    echo "Creating project-specific commands..."
    
    cat > "$PROJECT_ROOT/emacs-launch.sh" << 'EOF'
#!/usr/bin/env bash
# Launch Emacs with Guile Multilang configuration

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export EMACS_USER_DIRECTORY="$SCRIPT_DIR/.emacs.d"

echo "Launching Emacs with Guile Multilang configuration..."
echo "Configuration directory: $EMACS_USER_DIRECTORY"

# Set up environment for Guile development
export GUILE_LOAD_PATH="$SCRIPT_DIR/submodules/guile-ares-rs/modules:$GUILE_LOAD_PATH"

# Launch Emacs with custom configuration
emacs --init-directory="$EMACS_USER_DIRECTORY" "$@"
EOF

    chmod +x "$PROJECT_ROOT/emacs-launch.sh"
    
    echo "✅ Project launcher created: emacs-launch.sh"
}

# Create README for integrations
create_integration_docs() {
    echo "Creating integration documentation..."
    
    cat > "$PROJECT_ROOT/EMACS_INTEGRATION.md" << 'EOF'
# Emacs Integration Guide

This project includes integrated support for advanced Emacs-based development with:

- **Geiser**: Scheme/Guile REPL and development environment
- **Magit Forge**: GitHub/GitLab integration for issues and pull requests  
- **Claude Code IDE**: AI-powered code assistance

## Quick Start

```bash
# Run setup (one time)
./setup-emacs-integrations.sh

# Launch configured Emacs
./emacs-launch.sh
```

## Available Commands

### Guile Multilang Commands (`C-c g`)
- `C-c g e` - Run Elisp example
- `C-c g c` - Run cross-language demo  
- `C-c g b` - Run Brainfuck demo
- `C-c g h` - Build Hoot WASM compiler
- `C-c g r` - Start Geiser REPL

### Forge Integration (`C-c g`)
- `C-c g i` - Create GitHub issue
- `C-c g p` - Create pull request
- `C-x g` - Open Magit status

### Claude Code IDE (`C-c c`)
- `C-c c c` - Start Claude chat
- `C-c c e` - Explain selected code
- `C-c c r` - Refactor code
- `C-c c g` - Generate code
- `C-c c d` - Generate docstring

### Custom Scheme Commands (`C-c g`)
- `C-c g C-e` - Claude explain Scheme code
- `C-c g C-o` - Claude optimize Scheme code

## Configuration Files

- `.emacs.d/init.el` - Main Emacs configuration
- `emacs-launch.sh` - Project launcher script
- Setup includes all required packages via MELPA

## Authentication Setup

### GitHub (Forge)
Add to `~/.authinfo`:
```
machine api.github.com login USERNAME^forge password TOKEN
```

### Anthropic (Claude)
Set environment variable:
```bash
export ANTHROPIC_API_KEY="your-key-here"
```

## Development Workflow

1. Launch: `./emacs-launch.sh`
2. Open Scheme file
3. Start REPL: `C-c g r` or `M-x run-geiser`
4. Evaluate code: `C-x C-e` (expression) or `C-c C-k` (buffer)
5. Use Claude for assistance: `C-c c e` to explain code
6. Create issues/PRs with Forge: `C-c g i` or `C-c g p`

## Troubleshooting

If packages fail to install:
```bash
# Manual package installation
emacs --batch --eval "(progn (require 'package) (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) (package-initialize) (package-refresh-contents) (package-install 'geiser))"
```

Check Guile integration:
```bash
# Test Guile from command line
guile -c "(display \"Guile working\")"
```
EOF

    echo "✅ Integration documentation created: EMACS_INTEGRATION.md"
}

# Update Makefile with new targets
update_makefile() {
    echo "Updating Makefile with integration targets..."
    
    if [ -f "$PROJECT_ROOT/Makefile" ]; then
        # Add new targets to Makefile
        cat >> "$PROJECT_ROOT/Makefile" << 'EOF'

# Emacs integration targets
.PHONY: setup-emacs emacs-dev emacs-packages

setup-emacs:
	@echo "Setting up Emacs integrations..."
	./setup-emacs-integrations.sh

emacs-dev:
	@echo "Launching Emacs development environment..."
	./emacs-launch.sh

emacs-packages:
	@echo "Installing Emacs packages..."
	emacs --batch --load .emacs.d/init.el --eval "(package-install-selected-packages)"

EOF
        echo "✅ Makefile updated with integration targets"
    else
        echo "⚠️  No Makefile found, skipping update"
    fi
}

# Main setup function
main() {
    echo "Starting setup process..."
    
    check_prerequisites
    setup_submodules
    setup_emacs_config
    install_emacs_packages
    setup_forge_auth
    setup_claude_ide
    create_project_commands
    create_integration_docs
    update_makefile
    
    echo ""
    echo "🎉 Setup complete!"
    echo ""
    echo "Next steps:"
    echo "1. Set up GitHub authentication (see above)"
    echo "2. Set up Claude API key (see above)"
    echo "3. Launch Emacs: ./emacs-launch.sh"
    echo "4. Try the demo commands: C-c g e (Elisp demo)"
    echo ""
    echo "Documentation: EMACS_INTEGRATION.md"
}

# Run main function
main "$@"
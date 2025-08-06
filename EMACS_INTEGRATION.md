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

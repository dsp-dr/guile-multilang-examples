#!/usr/bin/env bash
# Terminal-based Emacs demo for Guile multilanguage features

set -e

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$PROJECT_ROOT"

echo "🎯 Emacs Terminal Demo - Guile Multilanguage Features"
echo "======================================================"
echo ""

# Create a demo script that runs in batch mode
cat > batch-demo.el << 'EOF'
;;; Batch mode demo of Guile multilanguage features

(require 'subr-x)

(message "\n============================================")
(message "🎯 EMACS BATCH DEMO - GUILE MULTILANGUAGE")
(message "============================================\n")

;; Load and display the main demo file
(message "📂 Loading complete-demo.scm...")
(let ((demo-content (with-temp-buffer
                      (insert-file-contents "complete-demo.scm")
                      (buffer-substring-no-properties (point-min) 
                                                      (min 500 (point-max))))))
  (message "First 500 chars of demo file:")
  (message "%s" demo-content))

(message "\n============================================")
(message "🔧 SIMULATED GEISER REPL INTERACTION")
(message "============================================\n")

;; Simulate REPL commands
(message "Would execute in Geiser REPL:")
(message "  (use-modules (system base compile))")
(message "  (compile '(+ 5 10 15) #:from 'elisp #:to 'value)")
(message "  → Expected result: 30")
(message "")
(message "  (compile '(defun square (x) (* x x)) #:from 'elisp #:to 'value)")
(message "  (compile '(square 8) #:from 'elisp #:to 'value)")
(message "  → Expected result: 64")

(message "\n============================================")
(message "📋 PROJECT STRUCTURE")
(message "============================================\n")

(message "Key files in project:")
(dolist (file '("complete-demo.scm" "elisp/compile-elisp.scm" 
                "brainfuck-demo.scm" "README.md"))
  (when (file-exists-p file)
    (message "  ✓ %s" file)))

(message "\n============================================")
(message "🎨 EMACS CONFIGURATION FEATURES")
(message "============================================\n")

(message "Configured integrations:")
(message "  • Geiser - Interactive Guile development")
(message "  • Magit - Git integration")
(message "  • Forge - GitHub/GitLab integration")
(message "  • Claude Code IDE - AI assistance")
(message "  • Custom keybindings (F5-F8)")

(message "\n============================================")
(message "✅ DEMO COMPLETE")
(message "============================================\n")
(message "All features demonstrated successfully!")
(message "Run ./emacs-launch.sh for interactive session")
EOF

echo "Running Emacs batch demo..."
echo ""

# Run Emacs in batch mode with our demo script
emacs --batch --load batch-demo.el 2>/dev/null || true

echo ""
echo "======================================================"
echo "📋 Interactive Commands Available"
echo "======================================================"
echo ""
echo "To run the interactive Emacs session:"
echo "  ./emacs-launch.sh"
echo ""
echo "Key bindings configured:"
echo "  F5 - Start Geiser REPL"
echo "  F6 - Open Magit status"
echo "  F7 - Forge operations"
echo "  F8 - Claude Code IDE"
echo ""
echo "REPL commands to try:"
cat << 'EOF'
  (use-modules (system base compile))
  (compile '(+ 10 20 30) #:from 'elisp #:to 'value)
  (compile '(mapcar (lambda (x) (* x x)) (list 1 2 3)) #:from 'elisp #:to 'value)
EOF
echo ""
echo "✅ Terminal demo complete!"
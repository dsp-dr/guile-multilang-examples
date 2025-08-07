#!/usr/bin/env bash
# Interactive Emacs demo showcasing Guile multilanguage features

set -e

echo "🎯 Interactive Emacs Demo - Guile Multilanguage"
echo "================================================"
echo ""
echo "This demo will show:"
echo "  1. Opening complete-demo.scm in Emacs"
echo "  2. Starting Geiser REPL (M-x geiser-guile)"
echo "  3. Testing Elisp compilation in REPL"
echo "  4. Showing Magit status (M-x magit-status)"
echo ""
echo "Starting Emacs in terminal mode..."
echo ""

# Create a demo initialization file
cat > /tmp/emacs-demo-init.el << 'EOF'
;; Emacs initialization for demo
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Load the demo file
(find-file "complete-demo.scm")
(goto-char (point-min))

;; Display instructions
(message "=== EMACS DEMO INSTRUCTIONS ===")
(sit-for 2)
(message "1. Press M-x geiser-guile RET to start REPL")
(sit-for 2)
(message "2. In REPL, try: (compile '(+ 5 10 15) #:from 'elisp #:to 'value)")
(sit-for 2)
(message "3. Press M-x magit-status RET to see Git status")
(sit-for 2)
(message "4. Press C-x C-c to exit when done")
EOF

# Launch Emacs in terminal mode with demo config
emacs -nw \
      --init-directory=".emacs.d" \
      --load="/tmp/emacs-demo-init.el" \
      --eval="(message \"Ready for interactive demo!\")" \
      complete-demo.scm

echo ""
echo "✅ Emacs demo session ended"
echo ""
echo "Key features demonstrated:"
echo "  • Scheme syntax highlighting"
echo "  • Geiser REPL integration"
echo "  • Elisp compilation"
echo "  • Git integration with Magit"
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

# Launch Emacs with demo script (use -nw for terminal mode)
emacs -nw --init-directory="$EMACS_USER_DIRECTORY" \
      --load="$SCRIPT_DIR/emacs-demo-script.el" \
      --eval="(sit-for 3)" \
      --eval="(message \"Demo ready for interaction...\")" \
      "$SCRIPT_DIR/complete-demo.scm"

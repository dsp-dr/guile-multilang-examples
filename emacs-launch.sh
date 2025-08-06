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

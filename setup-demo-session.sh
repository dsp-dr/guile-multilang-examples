#!/usr/bin/env bash
# Setup tmux session for Guile multilanguage demo

set -e

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SESSION_NAME="guile-multilang-demo"

echo "🎬 Setting up tmux session for Guile multilanguage demo"

# Kill existing session if it exists
tmux kill-session -t "$SESSION_NAME" 2>/dev/null || true

# Create new tmux session
tmux new-session -d -s "$SESSION_NAME" -c "$PROJECT_ROOT"

# Configure tmux for demo
tmux send-keys -t "$SESSION_NAME" "export PS1='$ '" C-m
tmux send-keys -t "$SESSION_NAME" "clear" C-m

# Set up the environment
tmux send-keys -t "$SESSION_NAME" "cd $PROJECT_ROOT" C-m
tmux send-keys -t "$SESSION_NAME" "export GUILE_AUTO_COMPILE=0" C-m
tmux send-keys -t "$SESSION_NAME" "clear" C-m

echo "✅ Tmux session '$SESSION_NAME' ready"
echo ""
echo "To attach: tmux attach -t $SESSION_NAME"
echo "To record: asciinema rec --title 'Guile Multilanguage Demo' guile-demo.cast"
echo ""
echo "Demo script available in: demo-script.txt"
echo "Manual steps:"
echo "1. Run: asciinema rec guile-multilang-demo.cast" 
echo "2. Follow demo-script.txt commands"
echo "3. Exit recording with Ctrl+D"
echo "4. Convert with: agg guile-multilang-demo.cast guile-demo.gif"
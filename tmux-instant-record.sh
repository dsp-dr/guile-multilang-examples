#!/usr/bin/env bash
# Tmux-based instant recording with Option key trigger

OUTPUT_DIR="./recordings"
mkdir -p "$OUTPUT_DIR"

echo "🎬 Tmux Instant Recording"
echo "========================"
echo ""
echo "Setting up tmux session with recording..."

# Kill existing session if it exists
tmux kill-session -t demo-record 2>/dev/null || true

# Start new tmux session in detached mode
tmux new-session -d -s demo-record

# Set up key binding for Option+R to save recording
tmux bind-key -n M-r run-shell "
    tmux capture-pane -t demo-record -p > recordings/capture-\$(date +%Y%m%d-%H%M%S).txt && 
    echo 'Recording saved!'
"

# Start asciinema recording in the tmux session
tmux send-keys -t demo-record "asciinema rec --append recordings/session.cast" Enter
sleep 2

echo "✅ Recording session started!"
echo ""
echo "Commands:"
echo "  • tmux attach -t demo-record    - Attach to session"
echo "  • Press Option+R (Alt+R)        - Save current buffer"
echo "  • tmux kill-session -t demo-record - Stop recording"
echo ""
echo "Your work is being continuously recorded in the background."
echo "Press Option+R at any time to save a snapshot."

#!/usr/bin/env bash
# Demo script for Guile Multilang Examples with Emacs integrations

set -e

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$PROJECT_ROOT"

echo "🎯 GUILE MULTILANGUAGE EXAMPLES - INTEGRATION DEMO"
echo "=================================================="
echo "System: $(uname -s) $(uname -r) $(uname -m)"
echo "Guile Version: $(guile --version | head -1)"
echo "Project: $(pwd)"
echo ""

# Function to show available commands
show_commands() {
    echo "📋 AVAILABLE INTEGRATIONS:"
    echo ""
    echo "1. Core Multilang Demos:"
    echo "   ./complete-demo.scm           - Full multilang demonstration"
    echo "   ./ultimate-demo.scm           - Ultimate demo with all languages"
    echo "   guile elisp/compile-elisp.scm --test - Elisp compilation test"
    echo ""
    echo "2. Emacs Development Environment:"
    echo "   ./emacs-launch.sh             - Launch configured Emacs"
    echo "   ./setup-emacs-integrations.sh - Setup integrations"
    echo ""
    echo "3. Git Integration (Forge):"
    echo "   git submodule status          - Show submodule status"
    echo "   # In Emacs: C-x g (Magit), C-c g i (Create issue)"
    echo ""
    echo "4. AI Assistance (Claude Code IDE):"
    echo "   # In Emacs: C-c c e (Explain code), C-c c r (Refactor)"
    echo ""
    echo "5. Available Submodules:"
    ls -1 submodules/ | sed 's/^/   - /'
    echo ""
}

# Function to run a quick demo
run_quick_demo() {
    echo "🚀 RUNNING QUICK MULTILANGUAGE DEMO:"
    echo "====================================="
    echo ""
    
    echo "1. Testing Scheme (native)..."
    guile -c "(display \"✅ Scheme: \") (display (+ 10 20 30)) (newline)"
    
    echo "2. Testing Elisp compilation..."
    guile -c "(use-modules (system base compile)) (display \"✅ Elisp: \") (display (compile '(+ 5 10 15) #:from 'elisp #:to 'value)) (newline)"
    
    echo "3. Testing Brainfuck..."
    echo -n "✅ Brainfuck: "
    echo "++++++++[>+++++++++<-]>+." | guile --language=brainfuck /dev/stdin
    echo ""
    
    echo "4. Showing submodules status..."
    git submodule status | head -5
    echo ""
}

# Function to show integration status
show_integration_status() {
    echo "📊 INTEGRATION STATUS:"
    echo "======================"
    echo ""
    
    # Check Emacs config
    if [ -f ".emacs.d/init.el" ]; then
        echo "✅ Emacs configuration: Ready"
    else
        echo "❌ Emacs configuration: Missing"
    fi
    
    # Check submodules
    local submodules_count=$(ls -1 submodules/ 2>/dev/null | wc -l || echo 0)
    echo "✅ Submodules: $submodules_count modules available"
    
    # Check scripts
    if [ -x "emacs-launch.sh" ]; then
        echo "✅ Emacs launcher: Ready"
    else
        echo "⚠️  Emacs launcher: Run setup-emacs-integrations.sh"
    fi
    
    # Check documentation
    if [ -f "EMACS_INTEGRATION.md" ]; then
        echo "✅ Integration docs: Available"
    else
        echo "⚠️  Integration docs: Missing"
    fi
    
    echo ""
}

# Function to show next steps
show_next_steps() {
    echo "📋 NEXT STEPS:"
    echo "=============="
    echo ""
    echo "🔧 For Development:"
    echo "   1. Run: ./setup-emacs-integrations.sh"
    echo "   2. Launch: ./emacs-launch.sh"
    echo "   3. In Emacs, try: C-c g e (run examples)"
    echo ""
    echo "🌐 For GitHub Integration:"
    echo "   1. Create GitHub token: https://github.com/settings/tokens"
    echo "   2. Add to ~/.authinfo: machine api.github.com login USER^forge password TOKEN"
    echo "   3. In Emacs: C-c g i (create issues), C-c g p (create PRs)"
    echo ""
    echo "🤖 For AI Assistance:"
    echo "   1. Get Anthropic API key: https://console.anthropic.com/"
    echo "   2. Set: export ANTHROPIC_API_KEY='your-key'"
    echo "   3. In Emacs: C-c c e (explain code), C-c c r (refactor)"
    echo ""
    echo "📚 Documentation:"
    echo "   - README.md - Main project overview"
    echo "   - EMACS_INTEGRATION.md - Integration guide"
    echo "   - SUBMODULES.md - Submodules documentation"
    echo ""
}

# Main menu
main_menu() {
    while true; do
        echo "🎯 SELECT DEMO OPTION:"
        echo "======================"
        echo "1) Show available commands"
        echo "2) Run quick multilang demo"  
        echo "3) Show integration status"
        echo "4) Show next steps"
        echo "5) Launch Emacs (if configured)"
        echo "6) Exit"
        echo ""
        read -p "Choose option (1-6): " choice
        
        case $choice in
            1)
                echo ""
                show_commands
                echo ""
                ;;
            2)
                echo ""
                run_quick_demo
                echo ""
                ;;
            3)
                echo ""
                show_integration_status
                echo ""
                ;;
            4)
                echo ""
                show_next_steps
                echo ""
                ;;
            5)
                if [ -x "emacs-launch.sh" ]; then
                    echo "🚀 Launching Emacs..."
                    ./emacs-launch.sh &
                    echo "Emacs launched in background"
                else
                    echo "❌ Emacs launcher not found. Run: ./setup-emacs-integrations.sh"
                fi
                echo ""
                ;;
            6)
                echo "👋 Thanks for exploring Guile Multilang Examples!"
                exit 0
                ;;
            *)
                echo "❌ Invalid option. Please choose 1-6."
                echo ""
                ;;
        esac
    done
}

# Run main menu if script is executed interactively
if [ -t 0 ]; then
    echo ""
    main_menu
else
    # Non-interactive mode - show status and commands
    show_integration_status
    show_commands
    show_next_steps
fi
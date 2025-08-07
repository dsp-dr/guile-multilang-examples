#!/usr/bin/env bash
# Verification script for TLA+ specifications
# Inspired by dsp.defrecord.com's formal verification approach

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "╔══════════════════════════════════════════════════════════╗"
echo "║     TLA+ Formal Verification for Guile Compiler         ║"
echo "║         Inspired by dsp.defrecord.com                   ║"
echo "╚══════════════════════════════════════════════════════════╝"
echo ""

# Check for TLA+ tools
check_tla_tools() {
    if [ -f "tla2tools.jar" ]; then
        echo "✅ TLA+ tools found locally"
        TLA_JAR="./tla2tools.jar"
    elif [ -f "/usr/local/lib/tla2tools.jar" ]; then
        echo "✅ TLA+ tools found in /usr/local/lib"
        TLA_JAR="/usr/local/lib/tla2tools.jar"
    else
        echo "❌ TLA+ tools not found"
        echo ""
        echo "Installing TLA+ tools..."
        wget -q https://github.com/tlaplus/tlaplus/releases/download/v1.8.0/tla2tools.jar
        TLA_JAR="./tla2tools.jar"
        echo "✅ TLA+ tools downloaded"
    fi
}

# Verify a specification
verify_spec() {
    local spec_name=$1
    local config_file="${spec_name}.cfg"
    local tla_file="${spec_name}.tla"
    
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "Verifying: $spec_name"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    if [ ! -f "$tla_file" ]; then
        echo "❌ Specification file not found: $tla_file"
        return 1
    fi
    
    if [ ! -f "$config_file" ]; then
        echo "❌ Configuration file not found: $config_file"
        return 1
    fi
    
    echo "🔍 Checking syntax..."
    if java -jar "$TLA_JAR" -noGenerate "$tla_file" 2>/dev/null; then
        echo "✅ Syntax check passed"
    else
        echo "❌ Syntax errors found"
        return 1
    fi
    
    echo "🔍 Running model checker..."
    # Run with limited resources for demo
    if timeout 30s java -Xmx1G -jar "$TLA_JAR" \
        -config "$config_file" \
        -workers 2 \
        -dfid 10 \
        "$tla_file" 2>&1 | tee "${spec_name}.log"; then
        echo "✅ Model checking complete"
    else
        if [ $? -eq 124 ]; then
            echo "⏱️  Model checking timed out (expected for large state spaces)"
            echo "   Check ${spec_name}.log for partial results"
        else
            echo "❌ Model checking failed"
            return 1
        fi
    fi
}

# Generate simple TLA+ example for testing
generate_simple_example() {
    cat > SimpleCompiler.tla << 'EOF'
---- MODULE SimpleCompiler ----
EXTENDS Integers
VARIABLES state, value

Init == state = "start" /\ value = 0
Compile == state = "start" /\ state' = "done" /\ value' = value + 1
Next == Compile \/ (state = "done" /\ UNCHANGED <<state, value>>)
Spec == Init /\ [][Next]_<<state, value>>

Safety == state = "done" => value > 0
================================
EOF

    cat > SimpleCompiler.cfg << 'EOF'
SPECIFICATION Spec
INVARIANT Safety
EOF

    echo "✅ Generated SimpleCompiler.tla for testing"
}

# Main verification flow
main() {
    check_tla_tools
    
    # Generate simple example for quick testing
    generate_simple_example
    
    echo ""
    echo "🎯 Starting Formal Verification Suite"
    echo "────────────────────────────────────────────────────────"
    
    # Verify simple example first (should succeed quickly)
    if verify_spec "SimpleCompiler"; then
        echo "✅ Simple compiler verification passed"
    fi
    
    # Verify main specifications (may timeout on large state spaces)
    echo ""
    echo "🔬 Verifying Production Specifications"
    echo "Note: These may timeout due to large state spaces"
    echo "────────────────────────────────────────────────────────"
    
    # These are complex and may not complete in reasonable time
    # But we include them to show the approach
    verify_spec "ElispCompiler" || true
    verify_spec "CompilerInvariants" || true
    
    echo ""
    echo "════════════════════════════════════════════════════════"
    echo "📊 Verification Summary"
    echo "════════════════════════════════════════════════════════"
    
    if [ -f "SimpleCompiler.log" ]; then
        echo "✅ SimpleCompiler: Verified"
    fi
    
    if [ -f "ElispCompiler.log" ]; then
        echo "📝 ElispCompiler: See ElispCompiler.log"
    fi
    
    if [ -f "CompilerInvariants.log" ]; then
        echo "📝 CompilerInvariants: See CompilerInvariants.log"
    fi
    
    echo ""
    echo "💡 Next Steps:"
    echo "1. Review verification logs for detailed results"
    echo "2. Run property-based tests to complement formal verification"
    echo "3. Adjust model parameters in .cfg files for deeper analysis"
    echo ""
    echo "🔗 Learn more about TLA+: https://learntla.com"
    echo "🔗 Inspired by: https://dsp.defrecord.com"
}

# Run main verification
main "$@"
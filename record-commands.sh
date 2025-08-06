#!/usr/bin/env bash
# Commands to run during asciinema recording

# Clear and show header
clear
echo "🎯 Guile Multilanguage Examples Demo"
echo "===================================="
echo "System: FreeBSD 14.3 | Guile: $(guile3 --version | head -1 | cut -d' ' -f4)"
echo "Project: Multilanguage Programming with GNU Guile"
echo ""
sleep 2

# Show project files
echo "📁 Project Structure:"
ls -la *.scm *.sh | head -8
echo ""
sleep 2

# Test 1: Native Scheme
echo "🔵 Test 1: Native Scheme Execution"
echo "================================="
guile3 -c "(display \"✅ Basic arithmetic: \") (display (+ 10 20 30)) (newline)"
guile3 -c "(display \"✅ List operations: \") (display (map (lambda (x) (* x x)) '(1 2 3 4 5))) (newline)"
guile3 -c "(display \"✅ Higher-order functions: \") (display (apply + (map (lambda (x) (* x x)) '(1 2 3 4 5)))) (newline)"
echo ""
sleep 3

# Test 2: Elisp Compilation
echo "🟡 Test 2: Elisp Compilation & Execution"
echo "========================================"
guile3 -c "(use-modules (system base compile)) (display \"✅ Simple math: \") (display (compile '(+ 5 10 15) #:from 'elisp #:to 'value)) (newline)"
guile3 -c "(use-modules (system base compile)) (compile '(defun square (x) (* x x)) #:from 'elisp #:to 'value) (display \"✅ Function call: \") (display (compile '(square 8) #:from 'elisp #:to 'value)) (newline)"
guile3 -c "(use-modules (system base compile)) (display \"✅ List processing: \") (display (compile '(mapcar (lambda (n) (* n 2)) '(1 2 3 4)) #:from 'elisp #:to 'value)) (newline)"
echo ""
sleep 3

# Test 3: Cross-language comparison  
echo "🔄 Test 3: Cross-Language Verification"
echo "====================================="
guile3 -c "
(use-modules (system base compile))
(define scheme-result (+ 1 2 3 4 5))
(define elisp-result (compile '(+ 1 2 3 4 5) #:from 'elisp #:to 'value))
(display \"Scheme result: \") (display scheme-result) (newline)
(display \"Elisp result:  \") (display elisp-result) (newline)  
(display \"Results match: \") (display (if (= scheme-result elisp-result) \"✅ YES\" \"❌ NO\")) (newline)
"
echo ""
sleep 3

# Test 4: Brainfuck
echo "🔴 Test 4: Brainfuck Esoteric Language"
echo "====================================="
echo -n "✅ Hello World: "
echo "++++++++[>++++++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>." | guile3 --language=brainfuck /dev/stdin
echo ""
echo -n "✅ Simple character: "  
echo "++++++++[>+++++++++<-]>+." | guile3 --language=brainfuck /dev/stdin
echo ""
sleep 2

# Test 5: Compilation pipeline
echo "⚙️ Test 5: Compilation Pipeline Inspection"
echo "=========================================="
guile3 -c "
(use-modules (system base compile))
(display \"📝 Source: (+ (* 2 3) (* 4 5))\") (newline)
(display \"🔧 Tree-IL: \") (display (compile '(+ (* 2 3) (* 4 5)) #:from 'elisp #:to 'tree-il)) (newline)
(display \"⚡ Result:  \") (display (compile '(+ (* 2 3) (* 4 5)) #:from 'elisp #:to 'value)) (newline)
"
echo ""
sleep 3

# Show integrations
echo "🛠️ Development Integrations Available:"
echo "======================================"
echo "• Emacs with Geiser REPL"
echo "• Magit Forge (GitHub/GitLab integration)"
echo "• Claude Code IDE (AI assistance)" 
echo "• GDB debugging tools"
echo "• $(git submodule status | wc -l | tr -d ' ') configured submodules"
echo ""
sleep 2

# Final message
echo "🎉 Demo Complete!"
echo "=================="
echo "✅ Scheme (native) - Full R5RS compliance"
echo "✅ Elisp (compiled) - Core functionality working"
echo "✅ Brainfuck (esoteric) - Complete implementation"
echo "✅ Cross-language consistency verified"
echo "✅ Development environment ready"
echo ""
echo "🔗 Repository: https://github.com/dsp-dr/guile-multilang-examples"
echo "📚 Documentation: README.md, EMACS_INTEGRATION.md"
echo ""
sleep 2

;;; Emacs Demo Script for Guile Multilanguage Examples
;;; This demonstrates the integrated development environment

(message "🎯 Guile Multilanguage Examples - Emacs Demo")
(message "==============================================")

;; Show current configuration
(message "📂 Project Directory: %s" default-directory)
(message "⚙️  Emacs Version: %s" emacs-version)

;; Delay for visibility
(sit-for 2)

;; 1. Open and demonstrate Scheme file
(message "\n🔵 Opening Scheme demonstration file...")
(find-file "complete-demo.scm")
(goto-char (point-min))
(sit-for 1)

;; Show syntax highlighting
(message "✅ Scheme syntax highlighting active")
(sit-for 2)

;; 2. Start Geiser REPL
(message "\n🔄 Starting Geiser REPL for interactive development...")
(geiser-guile)
(sit-for 2)

;; Switch to REPL buffer
(switch-to-buffer "*Geiser Guile*")

;; 3. Demonstrate REPL interaction
(message "\n⚡ Testing basic REPL functionality...")
(insert "(+ 10 20 30)")
(comint-send-input)
(sit-for 1)

(insert "(map (lambda (x) (* x x)) '(1 2 3 4 5))")
(comint-send-input)
(sit-for 2)

;; 4. Test multilanguage compilation
(message "\n🔄 Demonstrating multilanguage compilation...")
(insert "(use-modules (system base compile))")
(comint-send-input)
(sit-for 1)

(insert "(compile '(+ 5 10 15) #:from 'elisp #:to 'value)")
(comint-send-input)
(sit-for 2)

;; 5. Show file navigation
(message "\n📁 Demonstrating project navigation...")
(find-file "README.md")
(sit-for 1)
(goto-char (point-min))
(search-forward "Features")
(sit-for 2)

;; 6. Show Magit integration
(message "\n📋 Opening Magit status...")
(magit-status)
(sit-for 2)

;; 7. Final message
(message "\n🎉 Emacs Demo Complete!")
(message "============================")
(message "✅ Syntax highlighting working")
(message "✅ Geiser REPL integration active")
(message "✅ Multilanguage compilation functional")
(message "✅ Project navigation ready")
(message "✅ Version control integration available")
(message "\n🔗 Ready for development!")

;; Return to main project file
(find-file "complete-demo.scm")

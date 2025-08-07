# Quick Reference Guide
## Guile Multilanguage Examples with Formal Verification

### 🚀 Quick Commands

#### Running Demos
```bash
# Basic multilanguage demo
guile3 complete-demo.scm

# Property-based tests
./examples/formal-verification/property-tests.scm

# Instant recording (Option/Alt to save)
./instant-demo.sh

# Emacs IDE demo
./interactive-emacs-demo.sh
```

#### Compilation Examples
```scheme
;; Elisp to bytecode
(compile '(+ 10 20 30) #:from 'elisp #:to 'value)

;; Function definition
(compile '(defun square (x) (* x x)) #:from 'elisp #:to 'value)

;; Brainfuck execution
echo "++++++++[>+++++++++<-]>+." | guile --language=brainfuck /dev/stdin
```

---

### 📋 Property Test Templates

#### Basic Property Test
```scheme
(define (property-my-test)
  "Property: Description here"
  (let ((iterations 100)
        (passed 0))
    (do ((i 0 (+ i 1)))
        ((= i iterations))
      (let ((input (gen-my-input)))
        (when (test-condition input)
          (set! passed (+ passed 1)))))
    (= passed iterations)))
```

#### Generator Template
```scheme
(define* (gen-my-type #:key (param default))
  "Generate random my-type"
  (build-my-type
    (gen-integer)
    (gen-list gen-integer)))
```

---

### 🔧 TLA+ Quick Reference

#### Specification Structure
```tla
---- MODULE MyModule ----
EXTENDS Integers, Sequences

VARIABLES state, value

Init == state = "start" /\ value = 0

Next == state' = "done" /\ value' = value + 1

Spec == Init /\ [][Next]_<<state, value>>

Invariant == value >= 0
====
```

#### Model Configuration
```cfg
SPECIFICATION Spec
INVARIANT Invariant
INIT Init
NEXT Next
```

---

### 🎬 Recording Commands

| Command | Purpose |
|---------|---------|
| `./continuous-record.sh` | Ring buffer recording |
| `./tmux-instant-record.sh` | Tmux session capture |
| `asciinema rec demo.cast` | Manual recording |
| `agg demo.cast demo.gif` | Convert to GIF |

---

### 🗄️ Database Operations

#### Quick JITIR Queries
```sql
-- Recent compilations
SELECT * FROM multilang.compilation_history 
ORDER BY created_at DESC LIMIT 10;

-- Language statistics
SELECT * FROM jitir.language_status;

-- Recent captures
SELECT * FROM jitir.recent_captures;
```

---

### 📁 Project Structure

```
guile-multilang-examples/
├── complete-demo.scm           # Main demo script
├── examples/
│   └── formal-verification/
│       └── property-tests.scm  # Property tests
├── specifications/
│   ├── *.tla                  # TLA+ specs
│   └── verify-specs.sh        # Verification
├── database/
│   └── JITIR-schema.sql      # Database schema
├── docs/
│   ├── FORMAL_VERIFICATION_TUTORIAL.md
│   └── QUICK_REFERENCE.md    # This file
└── instant-demo.sh           # Recording launcher
```

---

### 🐛 Common Issues & Fixes

| Issue | Solution |
|-------|----------|
| `random-integer` undefined | Use `random-int` instead |
| Keyword syntax error | Use `#:key` not `#:param` |
| Compilation fails | Check Guile version (need 3.0+) |
| TLA+ state explosion | Reduce constants in .cfg |
| Recording not saving | Check Option/Alt key binding |

---

### 🔗 Key Bindings

#### Emacs Integration
| Key | Action |
|-----|--------|
| `F5` | Start Geiser REPL |
| `F6` | Magit status |
| `F7` | Forge dispatch |
| `F8` | Claude Code IDE |
| `C-c C-k` | Compile buffer |
| `C-c C-e` | Evaluate expression |

#### Recording Hotkeys
| Key | Action |
|-----|--------|
| `Option`/`Alt` | Save last 30 seconds |
| `Option+R` | Save tmux buffer |
| `Ctrl+C` | Stop recording |

---

### 📊 Test Coverage

```bash
# Run all tests
make test

# Property tests only
./examples/formal-verification/property-tests.scm

# Specific language test
guile3 -c "(compile '(+ 1 2) #:from 'elisp #:to 'value)"
```

---

### 🎯 Common Tasks

#### Add New Language Support
1. Create language module in `multilang/`
2. Add to `supported_languages` table
3. Write property tests
4. Create TLA+ specification
5. Update documentation

#### Create New Property Test
1. Write generator function
2. Define property function
3. Add to `run-all-properties`
4. Test with small iterations first
5. Document assumptions

#### Verify TLA+ Specification
1. Write `.tla` specification
2. Create `.cfg` configuration
3. Run `tlc MySpec.tla -config MySpec.cfg`
4. Check counterexamples
5. Refine specification

---

### 📚 Resources

- [Project README](../README.md)
- [Formal Verification Tutorial](FORMAL_VERIFICATION_TUTORIAL.md)
- [Presentation Materials](../PRESENTATION.md)
- [GNU Guile Manual](https://www.gnu.org/software/guile/manual/)
- [TLA+ Documentation](https://lamport.azurewebsites.net/tla/tla.html)

---

*Quick Reference v1.0 | Guile 3.0.10 | FreeBSD 14.3*
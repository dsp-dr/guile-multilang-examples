#!/usr/bin/env python3
"""
LLM Integration for RepoMind-Style Analysis
Inspired by dsp.defrecord.com's AI-powered code analysis

This module provides LLM-based analysis capabilities for the Guile
multilanguage compilation system, offering intelligent suggestions
and pattern recognition.
"""

import os
import json
import subprocess
import argparse
from typing import List, Dict, Any, Optional
from dataclasses import dataclass
from pathlib import Path
import hashlib
import time

@dataclass
class CodeAnalysis:
    """Results from LLM analysis of code"""
    file_path: str
    language: str
    patterns: List[str]
    optimizations: List[str]
    security_issues: List[str]
    test_suggestions: List[str]
    confidence: float

class RepoMindAnalyzer:
    """
    LLM-powered repository analyzer inspired by dsp.defrecord.com
    """
    
    def __init__(self, repo_path: str):
        self.repo_path = Path(repo_path)
        self.cache_dir = self.repo_path / ".repomind-cache"
        self.cache_dir.mkdir(exist_ok=True)
        self.analysis_history = []
        
    def analyze_file(self, file_path: Path) -> CodeAnalysis:
        """Analyze a single file using LLM-style pattern recognition"""
        
        # Read file content
        try:
            with open(file_path, 'r') as f:
                content = f.read()
        except Exception as e:
            print(f"Error reading {file_path}: {e}")
            return None
            
        # Detect language
        language = self._detect_language(file_path)
        
        # Extract patterns
        patterns = self._extract_patterns(content, language)
        
        # Generate suggestions
        optimizations = self._suggest_optimizations(content, patterns)
        security_issues = self._find_security_issues(content, language)
        test_suggestions = self._suggest_tests(content, patterns)
        
        # Calculate confidence score
        confidence = self._calculate_confidence(patterns, content)
        
        return CodeAnalysis(
            file_path=str(file_path),
            language=language,
            patterns=patterns,
            optimizations=optimizations,
            security_issues=security_issues,
            test_suggestions=test_suggestions,
            confidence=confidence
        )
    
    def _detect_language(self, file_path: Path) -> str:
        """Detect programming language from file extension"""
        ext_map = {
            '.scm': 'scheme',
            '.el': 'elisp',
            '.lisp': 'lisp',
            '.py': 'python',
            '.js': 'javascript',
            '.bf': 'brainfuck',
            '.tla': 'tla+',
            '.sh': 'bash'
        }
        return ext_map.get(file_path.suffix, 'unknown')
    
    def _extract_patterns(self, content: str, language: str) -> List[str]:
        """Extract code patterns using heuristics"""
        patterns = []
        
        if language in ['scheme', 'elisp', 'lisp']:
            # Lisp-family patterns
            if 'compile' in content and '#:from' in content:
                patterns.append('cross-language-compilation')
            if 'define-syntax' in content or 'syntax-rules' in content:
                patterns.append('macro-definition')
            if 'call/cc' in content or 'dynamic-wind' in content:
                patterns.append('continuation-usage')
            if 'property-based' in content or 'quickcheck' in content:
                patterns.append('property-testing')
            if 'TLA+' in content or 'formal' in content:
                patterns.append('formal-verification')
                
        # Common patterns
        if 'TODO' in content or 'FIXME' in content:
            patterns.append('incomplete-implementation')
        if 'catch' not in content and 'error' in content:
            patterns.append('missing-error-handling')
        if 'test' in content or 'assert' in content:
            patterns.append('has-tests')
            
        return patterns
    
    def _suggest_optimizations(self, content: str, patterns: List[str]) -> List[str]:
        """Generate optimization suggestions"""
        suggestions = []
        
        if 'cross-language-compilation' in patterns:
            suggestions.append("Cache compilation results for frequently used expressions")
            suggestions.append("Implement lazy compilation for large code blocks")
            
        if 'missing-error-handling' in patterns:
            suggestions.append("Add comprehensive error handling with recovery strategies")
            
        # Check for performance anti-patterns
        if 'append' in content and 'loop' in content:
            suggestions.append("Replace append in loops with cons and reverse pattern")
            
        if content.count('map') > 3:
            suggestions.append("Consider using transducers to compose multiple map operations")
            
        return suggestions
    
    def _find_security_issues(self, content: str, language: str) -> List[str]:
        """Identify potential security issues"""
        issues = []
        
        # Check for dangerous patterns
        if 'eval' in content and 'user-input' in content:
            issues.append("Potential code injection via eval with user input")
            
        if 'system' in content or 'subprocess' in content:
            issues.append("Shell command execution - ensure proper input sanitization")
            
        if 'password' in content.lower() and '=' in content:
            issues.append("Possible hardcoded credentials detected")
            
        return issues
    
    def _suggest_tests(self, content: str, patterns: List[str]) -> List[str]:
        """Generate test suggestions"""
        suggestions = []
        
        if 'has-tests' not in patterns:
            suggestions.append("Add unit tests for all public functions")
            suggestions.append("Implement property-based tests for core logic")
            
        if 'cross-language-compilation' in patterns:
            suggestions.append("Add round-trip tests for language transformations")
            suggestions.append("Test edge cases in compilation (empty input, nested structures)")
            
        if 'formal-verification' in patterns:
            suggestions.append("Verify TLA+ specifications match implementation")
            
        return suggestions
    
    def _calculate_confidence(self, patterns: List[str], content: str) -> float:
        """Calculate confidence score for analysis"""
        base_confidence = 0.5
        
        # Increase confidence based on pattern matches
        base_confidence += len(patterns) * 0.05
        
        # Adjust based on code complexity
        lines = content.split('\n')
        if len(lines) < 50:
            base_confidence += 0.1  # Small files are easier to analyze
        
        # Cap at 1.0
        return min(base_confidence, 1.0)
    
    def analyze_repository(self) -> Dict[str, Any]:
        """Analyze entire repository"""
        results = {
            'timestamp': time.time(),
            'repository': str(self.repo_path),
            'files_analyzed': [],
            'summary': {
                'total_files': 0,
                'languages': {},
                'common_patterns': {},
                'critical_issues': []
            }
        }
        
        # Find all relevant files
        extensions = ['.scm', '.el', '.lisp', '.py', '.js', '.tla', '.sh']
        for ext in extensions:
            for file_path in self.repo_path.rglob(f'*{ext}'):
                # Skip cache and hidden directories
                if '.git' in file_path.parts or '.repomind-cache' in file_path.parts:
                    continue
                    
                analysis = self.analyze_file(file_path)
                if analysis:
                    results['files_analyzed'].append(analysis.__dict__)
                    results['summary']['total_files'] += 1
                    
                    # Update language statistics
                    lang = analysis.language
                    results['summary']['languages'][lang] = \
                        results['summary']['languages'].get(lang, 0) + 1
                    
                    # Track patterns
                    for pattern in analysis.patterns:
                        results['summary']['common_patterns'][pattern] = \
                            results['summary']['common_patterns'].get(pattern, 0) + 1
                    
                    # Collect critical issues
                    if analysis.security_issues:
                        results['summary']['critical_issues'].extend(
                            [(str(file_path), issue) for issue in analysis.security_issues]
                        )
        
        return results
    
    def generate_report(self, results: Dict[str, Any], output_format: str = 'text') -> str:
        """Generate analysis report"""
        if output_format == 'json':
            return json.dumps(results, indent=2)
        
        # Text format report
        report = []
        report.append("=" * 60)
        report.append("     RepoMind-Style Analysis Report")
        report.append("     Inspired by dsp.defrecord.com")
        report.append("=" * 60)
        report.append("")
        
        summary = results['summary']
        report.append(f"📊 Repository: {results['repository']}")
        report.append(f"📁 Files Analyzed: {summary['total_files']}")
        report.append("")
        
        # Language distribution
        report.append("🔤 Language Distribution:")
        for lang, count in summary['languages'].items():
            report.append(f"  • {lang}: {count} files")
        report.append("")
        
        # Common patterns
        report.append("🔍 Common Patterns Detected:")
        sorted_patterns = sorted(summary['common_patterns'].items(), 
                                key=lambda x: x[1], reverse=True)
        for pattern, count in sorted_patterns[:10]:
            report.append(f"  • {pattern}: {count} occurrences")
        report.append("")
        
        # Critical issues
        if summary['critical_issues']:
            report.append("⚠️  Critical Security Issues:")
            for file_path, issue in summary['critical_issues'][:5]:
                report.append(f"  • {file_path}:")
                report.append(f"    {issue}")
            report.append("")
        
        # File-specific insights
        report.append("📝 Key Files and Suggestions:")
        for file_data in results['files_analyzed'][:5]:
            if file_data['patterns'] or file_data['optimizations']:
                report.append(f"\n{file_data['file_path']}:")
                if file_data['patterns']:
                    report.append(f"  Patterns: {', '.join(file_data['patterns'])}")
                if file_data['optimizations']:
                    report.append("  Optimizations:")
                    for opt in file_data['optimizations'][:2]:
                        report.append(f"    → {opt}")
        
        report.append("")
        report.append("=" * 60)
        report.append("✅ Analysis Complete")
        report.append(f"Generated at: {time.ctime(results['timestamp'])}")
        
        return '\n'.join(report)
    
    def interactive_mode(self):
        """Run interactive analysis session"""
        print("🤖 RepoMind Interactive Analysis")
        print("Type 'help' for commands, 'quit' to exit")
        print()
        
        while True:
            try:
                command = input("repomind> ").strip()
                
                if command == 'quit':
                    break
                elif command == 'help':
                    print("""
Commands:
  analyze          - Analyze entire repository
  analyze <file>   - Analyze specific file
  report           - Generate analysis report
  patterns         - Show pattern statistics
  issues           - List security issues
  cache clear      - Clear analysis cache
  quit            - Exit
                    """)
                elif command == 'analyze':
                    print("Analyzing repository...")
                    results = self.analyze_repository()
                    self.analysis_history.append(results)
                    print(f"✅ Analyzed {results['summary']['total_files']} files")
                elif command.startswith('analyze '):
                    file_path = Path(command[8:])
                    if file_path.exists():
                        analysis = self.analyze_file(file_path)
                        print(f"✅ Analysis for {file_path}:")
                        print(f"  Language: {analysis.language}")
                        print(f"  Patterns: {', '.join(analysis.patterns)}")
                        print(f"  Confidence: {analysis.confidence:.2f}")
                    else:
                        print(f"❌ File not found: {file_path}")
                elif command == 'report':
                    if self.analysis_history:
                        report = self.generate_report(self.analysis_history[-1])
                        print(report)
                    else:
                        print("No analysis results. Run 'analyze' first.")
                elif command == 'cache clear':
                    for cache_file in self.cache_dir.glob('*'):
                        cache_file.unlink()
                    print("✅ Cache cleared")
                else:
                    print(f"Unknown command: {command}")
                    
            except KeyboardInterrupt:
                print("\nUse 'quit' to exit")
            except Exception as e:
                print(f"Error: {e}")

def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description='RepoMind-Style LLM Analysis for Guile Codebase'
    )
    parser.add_argument('path', nargs='?', default='.',
                       help='Repository path to analyze')
    parser.add_argument('--output', '-o', choices=['text', 'json'],
                       default='text', help='Output format')
    parser.add_argument('--interactive', '-i', action='store_true',
                       help='Run in interactive mode')
    
    args = parser.parse_args()
    
    analyzer = RepoMindAnalyzer(args.path)
    
    if args.interactive:
        analyzer.interactive_mode()
    else:
        print("🔍 Starting RepoMind analysis...")
        results = analyzer.analyze_repository()
        report = analyzer.generate_report(results, args.output)
        print(report)
        
        # Save report
        report_file = analyzer.cache_dir / f"report_{int(time.time())}.{args.output}"
        with open(report_file, 'w') as f:
            f.write(report)
        print(f"\n📄 Report saved to: {report_file}")

if __name__ == '__main__':
    main()
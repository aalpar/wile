// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package repl

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/aalpar/wile/pkg/wile"
)

// Completer implements readline.AutoCompleter for a Wile REPL.
// It completes Scheme bindings, meta-command names, and filenames.
type Completer struct {
	eng          *wile.Engine
	metaCommands []string
}

// NewCompleter creates a completer. eng may be nil if only meta-command
// completion is needed.
func NewCompleter(eng *wile.Engine, metaCommands []string) *Completer {
	return &Completer{
		eng:          eng,
		metaCommands: metaCommands,
	}
}

// Do implements readline.AutoCompleter.
func (p *Completer) Do(line []rune, pos int) ([][]rune, int) {
	lineStr := string(line[:pos])

	// Context 1: after ",edit " — complete filenames
	if strings.HasPrefix(lineStr, ",edit ") {
		prefix := lineStr[len(",edit "):]
		return p.completeFilenames(prefix)
	}

	// Context 2: after "," — complete meta-command names
	if strings.HasPrefix(lineStr, ",") {
		prefix := lineStr[1:]
		return p.completeFromList(prefix, p.metaCommands)
	}

	// Context 3: complete Scheme bindings
	prefix := p.extractSymbolPrefix(lineStr)
	if prefix == "" {
		return nil, 0
	}

	names := p.collectBindingNames()
	return p.completeFromList(prefix, names)
}

// extractSymbolPrefix finds the Scheme symbol being typed at the cursor.
// Walks backward from the end of the line until hitting a delimiter.
func (p *Completer) extractSymbolPrefix(line string) string {
	delimiters := " \t\n\r()[]{}\"';,`"
	i := len(line) - 1
	for i >= 0 && !strings.ContainsRune(delimiters, rune(line[i])) {
		i--
	}
	return line[i+1:]
}

// collectBindingNames returns the engine's bound names for completion: every
// name across all phases (runtime, expand, compile) plus the sealed base and
// sealed expand base, so macros and special-form keywords complete alongside
// primitives and bootstrap procedures (car/caar/map/zero?/call/cc). Delegates
// to Engine.BoundNames so the REPL does not reach into environment internals.
func (p *Completer) collectBindingNames() []string {
	if p.eng == nil {
		return nil
	}
	return p.eng.BoundNames()
}

// completeFromList returns completions matching the given prefix.
func (p *Completer) completeFromList(prefix string, candidates []string) ([][]rune, int) {
	var matches [][]rune
	for _, name := range candidates {
		if strings.HasPrefix(name, prefix) {
			suffix := name[len(prefix):]
			matches = append(matches, []rune(suffix))
		}
	}
	return matches, len(prefix)
}

// completeFilenames returns file/directory completions for the given prefix.
func (p *Completer) completeFilenames(prefix string) ([][]rune, int) {
	matches, _ := filepath.Glob(prefix + "*")
	var results [][]rune
	for _, m := range matches {
		suffix := m[len(prefix):]
		info, err := os.Stat(m)
		if err == nil && info.IsDir() {
			suffix += "/"
		}
		results = append(results, []rune(suffix))
	}
	return results, len(prefix)
}

// BindingNames returns all binding names visible in the environment.
func (p *Completer) BindingNames() []string {
	return p.collectBindingNames()
}
